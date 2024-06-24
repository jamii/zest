const std = @import("std");
const panic = std.debug.panic;
const assert = std.debug.assert;
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;

const zest = @import("./zest.zig");
const oom = zest.oom;
const deepEqual = zest.deepEqual;
const Compiler = zest.Compiler;
const Local = zest.Local;
const Value = zest.Value;
const Builtin = zest.Builtin;
const sir = zest.sir;
const dir = zest.dir;

pub fn desugar(c: *Compiler) error{DesugarError}!void {
    c.dir_fun_main = try desugarFun(c, .{ .keys = &.{}, .values = &.{} }, c.sir_expr_data.lastKey().?);
}

fn desugarFun(c: *Compiler, params: sir.Object, body: sir.Expr) error{DesugarError}!dir.Fun {
    var f = dir.FunData.init(c.allocator);

    try desugarObjectPattern(c, &f, .arg, params, .args);
    f.pattern_local_count = f.local_data.count();
    f.pattern_expr_count = f.expr_data.count();

    {
        _ = f.expr_data.append(.begin);
        defer _ = f.expr_data.append(.@"return");

        _ = f.expr_data.append(.begin);
        defer _ = f.expr_data.append(.assert_has_no_ref);

        try desugarExpr(c, &f, body);
    }
    return c.dir_fun_data.append(f);
}

// Would be nicer for this to be a closure that emits some exprs, but zig closures are painful.
const PatternInput = union(enum) {
    arg,
    expr: sir.Expr,
    object_get: struct {
        object: dir.Local,
        key: sir.Expr,
    },
};

const PatternContext = enum { args, let };

fn desugarPatternInput(c: *Compiler, f: *dir.FunData, input: PatternInput) error{DesugarError}!void {
    switch (input) {
        .arg => _ = f.expr_data.append(.arg),
        .expr => |expr| try desugarExpr(c, f, expr),
        .object_get => |object_get| {
            _ = f.expr_data.append(.begin);
            defer _ = f.expr_data.append(.object_get);

            _ = f.expr_data.append(.{ .local_get = object_get.object });
            try desugarKey(c, f, object_get.key);
        },
    }
}

fn desugarObjectPattern(c: *Compiler, f: *dir.FunData, input: PatternInput, pattern: sir.Object, context: PatternContext) error{DesugarError}!void {
    const local = f.local_data.append(.{
        .is_mutable = false,
        .is_tmp = true,
    });

    {
        _ = f.expr_data.append(.begin);
        defer _ = f.expr_data.append(.{ .local_let = local });

        _ = f.expr_data.append(.begin);
        defer _ = f.expr_data.append(.{ .assert_object = .{ .count = pattern.keys.len } });

        try desugarPatternInput(c, f, input);
    }

    for (pattern.keys, pattern.values) |key_expr, value_expr| {
        try desugarPattern(c, f, .{ .object_get = .{ .object = local, .key = key_expr } }, value_expr, context);
    }
}

fn desugarPattern(c: *Compiler, f: *dir.FunData, input: PatternInput, pattern: sir.Expr, context: PatternContext) error{DesugarError}!void {
    const expr_data = c.sir_expr_data.get(pattern);
    switch (expr_data) {
        .name => |name| {
            if (c.scope.lookup(name.name)) |_|
                return fail(c, pattern, .{ .name_already_bound = .{ .name = name.name } });

            const local = f.local_data.append(.{
                .is_mutable = name.mut,
                .is_tmp = false,
            });

            c.scope.push(.{
                .name = name.name,
                .value = .{ .local = local },
                .mut = name.mut,
            });

            _ = f.expr_data.append(.begin);
            defer _ = f.expr_data.append(.{ .local_let = local });

            // TODO The need for context here indicates that maybe we should treat mut uniformly and add a ref keyword for args.

            if (context == .let and name.mut) _ = f.expr_data.append(.begin);
            defer if (context == .let and name.mut) {
                _ = f.expr_data.append(.ref_init);
            };

            _ = f.expr_data.append(.begin);
            defer _ = f.expr_data.append(if (context == .args and name.mut) .assert_is_ref else .assert_has_no_ref);

            try desugarPatternInput(c, f, input);
        },
        .object => |object| try desugarObjectPattern(c, f, input, object, context),
        .ref_to => {
            _ = f.expr_data.append(.begin);
            defer _ = f.expr_data.append(.ref_set);

            try desugarPath(c, f, pattern);
            try desugarPatternInput(c, f, input);
        },
        else => return fail(c, pattern, .invalid_pattern),
    }
}

fn desugarKey(c: *Compiler, f: *dir.FunData, expr: sir.Expr) error{DesugarError}!void {
    const expr_data = c.sir_expr_data.get(expr);
    switch (expr_data) {
        .name => |name| {
            if (name.mut)
                return fail(c, expr, .meaningless_mut);
            stageString(c, f, name.name);
        },
        else => try stageExpr(c, f, expr),
    }
}

fn desugarObject(c: *Compiler, f: *dir.FunData, object: sir.Object) error{DesugarError}!void {
    _ = f.expr_data.append(.begin);
    defer _ = f.expr_data.append(.{ .struct_init = object.keys.len });

    for (object.keys, object.values) |key, value| {
        try desugarKey(c, f, key);
        try desugarExpr(c, f, value);
    }
}

fn desugarExpr(c: *Compiler, f: *dir.FunData, expr: sir.Expr) error{DesugarError}!void {
    const expr_data = c.sir_expr_data.get(expr);
    switch (expr_data) {
        .name, .get, .ref_to => {
            try desugarPath(c, f, expr);
        },
        .i32 => |i| {
            _ = f.expr_data.append(.{ .i32 = i });
        },
        .string => |string| {
            _ = f.expr_data.append(.{ .string = string });
        },
        .object => |object| {
            try desugarObject(c, f, object);
        },
        .let => |let| {
            try desugarPattern(c, f, .{ .expr = let.value }, let.path, .let);
        },
        .fun => |fun| {
            const dir_fun = dir_fun: {
                const prev_closure_until_len = c.scope.closure_until_len;
                c.scope.closure_until_len = c.scope.bindings.items.len;
                defer c.scope.closure_until_len = prev_closure_until_len;

                const scope_saved = c.scope.save();
                defer c.scope.restore(scope_saved);

                break :dir_fun try desugarFun(c, fun.params, fun.body);
            };

            _ = f.expr_data.append(.begin);
            defer _ = f.expr_data.append(.{ .fun_init = .{ .fun = dir_fun } });

            {
                const dir_fun_data = c.dir_fun_data.get(dir_fun);

                _ = f.expr_data.append(.begin);
                defer _ = f.expr_data.append(.{ .struct_init = dir_fun_data.closure_keys.items.len });

                for (dir_fun_data.closure_keys.items) |name| {
                    stageString(c, f, name);
                    const binding = c.scope.lookup(name).?;
                    desugarWalue(c, f, binding.value, binding.is_staged);
                }
            }
        },
        .call => |call| {
            _ = f.expr_data.append(.begin);
            defer _ = f.expr_data.append(.call);

            try desugarExpr(c, f, call.head);
            try desugarObject(c, f, call.args);
        },
        .call_builtin => |call| {
            _ = f.expr_data.append(.begin);
            defer _ = f.expr_data.append(.{ .call_builtin = call.head });

            try desugarObject(c, f, call.args);
        },
        .block => |block| {
            const scope_saved = c.scope.save();
            defer c.scope.restore(scope_saved);

            var count: usize = 0;

            _ = f.expr_data.append(.begin);

            for (block) |statement| {
                if (c.sir_expr_data.get(statement) != .let)
                    count += 1;
                try desugarExpr(c, f, statement);
            }

            _ = f.expr_data.append(.{ .block = count });
        },
        .@"if" => |@"if"| {
            _ = f.expr_data.append(.if_begin);
            try desugarExpr(c, f, @"if".cond);
            _ = f.expr_data.append(.if_then);
            try desugarExpr(c, f, @"if".then);
            _ = f.expr_data.append(.if_else);
            try desugarExpr(c, f, @"if".@"else");
            _ = f.expr_data.append(.if_end);
        },
        else => return fail(c, expr, .todo),
    }
}

fn desugarPath(c: *Compiler, f: *dir.FunData, expr: sir.Expr) error{DesugarError}!void {
    const expr_data = c.sir_expr_data.get(expr);
    switch (expr_data) {
        .ref_to => |ref_to| {
            _ = try desugarPathPart(c, f, ref_to, true);
        },
        else => {
            _ = f.expr_data.append(.begin);
            const is_mut = try desugarPathPart(c, f, expr, false);
            _ = f.expr_data.append(if (is_mut) .ref_deref else .nop);
        },
    }
}

fn desugarPathPart(c: *Compiler, f: *dir.FunData, expr: sir.Expr, must_be_mut: bool) error{DesugarError}!bool {
    const expr_data = c.sir_expr_data.get(expr);
    switch (expr_data) {
        .name => |name| {
            if (name.mut)
                return fail(c, expr, .meaningless_mut);
            const binding = c.scope.lookup(name.name) orelse
                return fail(c, expr, .{ .name_not_bound = .{ .name = name.name } });
            if (must_be_mut and binding.value == .closure)
                return fail(c, expr, .{ .todo_may_not_close_over_ref = .{ .name = name.name } });
            if (must_be_mut and !binding.mut)
                return fail(c, expr, .{ .may_not_mutate_immutable_binding = .{ .name = name.name } });
            desugarWalue(c, f, binding.value, binding.is_staged);
            return binding.mut;
        },
        .get => |get| {
            _ = f.expr_data.append(.begin);

            const is_mut = try desugarPathPart(c, f, get.object, must_be_mut);
            try desugarKey(c, f, get.key);

            _ = f.expr_data.append(if (is_mut) .ref_get else .object_get);

            return is_mut;
        },
        .ref_to => |ref_to| {
            return desugarPathPart(c, f, ref_to, true);
        },
        else => {
            if (must_be_mut)
                return fail(c, expr, .invalid_path);

            try desugarExpr(c, f, expr);

            return false;
        },
    }
}

fn desugarWalue(c: *Compiler, f: *dir.FunData, value: dir.Walue, is_staged: bool) void {
    if (is_staged)
        _ = f.expr_data.append(.unstage);

    switch (value) {
        .arg => _ = f.expr_data.append(.arg),
        .closure => |name| {
            if (!f.closure_keys_index.contains(name)) {
                f.closure_keys_index.put(name, {}) catch oom();
                f.closure_keys.append(name) catch oom();
            }

            _ = f.expr_data.append(.begin);
            defer _ = f.expr_data.append(.object_get);

            _ = f.expr_data.append(.closure);
            stageString(c, f, name);
        },
        .local => |local| {
            _ = f.expr_data.append(.{ .local_get = local });
        },
    }
}

fn stageExpr(c: *Compiler, f: *dir.FunData, expr: sir.Expr) error{DesugarError}!void {
    const already_staged = c.scope.staged_until_len != null;
    if (!already_staged) {
        _ = f.expr_data.append(.stage);
        c.scope.staged_until_len = c.scope.bindings.items.len;
    }
    defer if (!already_staged) {
        c.scope.staged_until_len = null;
    };

    try desugarExpr(c, f, expr);
}

fn stageString(c: *Compiler, f: *dir.FunData, string: []const u8) void {
    const already_staged = c.scope.staged_until_len != null;
    if (!already_staged) {
        _ = f.expr_data.append(.stage);
    }

    _ = f.expr_data.append(.{ .string = string });
}

fn fail(c: *Compiler, expr: sir.Expr, data: DesugarErrorData) error{DesugarError} {
    c.error_data = .{ .desugar = .{ .expr = expr, .data = data } };
    return error.DesugarError;
}

pub const DesugarErrorData = union(enum) {
    invalid_pattern,
    name_not_bound: struct {
        name: []const u8,
    },
    name_already_bound: struct {
        name: []const u8,
    },
    todo_may_not_close_over_ref: struct {
        name: []const u8,
    },
    may_not_mutate_immutable_binding: struct {
        name: []const u8,
    },
    invalid_let_path,
    invalid_path,
    meaningless_mut,
    todo,
};
