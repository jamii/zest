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

    try desugarObjectPattern(c, &f, .arg, params);
    f.pattern_local_count = f.local_data.count();
    f.pattern_expr_count = f.expr_data.count();

    {
        _ = f.expr_data.append(.begin);
        defer _ = f.expr_data.append(.@"return");

        try desugarExpr(c, &f, body);
    }
    return c.dir_fun_data.append(f);
}

fn desugarObjectPattern(c: *Compiler, f: *dir.FunData, object: dir.AbstractValue, pattern: sir.Object) error{DesugarError}!void {
    {
        _ = f.expr_data.append(.begin);
        defer _ = f.expr_data.append(.{ .assert_object = .{ .count = pattern.keys.len } });

        push(c, f, object, false, false);
    }
    for (pattern.keys, pattern.values) |key_expr, value_expr| {
        const local = f.local_data.append(.{
            .is_mutable = false,
        });
        {
            _ = f.expr_data.append(.begin);
            defer _ = f.expr_data.append(.{ .local_let = .{
                .local = local,
                .mut = false,
            } });

            _ = f.expr_data.append(.begin);
            defer _ = f.expr_data.append(.object_get);

            push(c, f, object, false, false);
            try desugarKey(c, f, key_expr);
        }

        try desugarPattern(c, f, .{ .local = local }, value_expr);
    }
}

fn desugarPattern(c: *Compiler, f: *dir.FunData, value: dir.AbstractValue, pattern: sir.Expr) error{DesugarError}!void {
    const expr_data = c.sir_expr_data.get(pattern);
    switch (expr_data) {
        .name => |name| {
            if (c.scope.lookup(name)) |_|
                return fail(c, pattern, .{ .name_already_bound = .{ .name = name } });
            c.scope.push(.{
                .name = name,
                .value = value,
            });
        },
        .object => |object| try desugarObjectPattern(c, f, value, object),
        else => return fail(c, pattern, .invalid_pattern),
    }
}

fn desugarKey(c: *Compiler, f: *dir.FunData, expr: sir.Expr) error{DesugarError}!void {
    const expr_data = c.sir_expr_data.get(expr);
    switch (expr_data) {
        .name => |name| stageString(c, f, name),
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
        .i32 => |i| {
            _ = f.expr_data.append(.{ .i32 = i });
        },
        .string => |string| {
            _ = f.expr_data.append(.{ .string = string });
        },
        .object => |object| {
            try desugarObject(c, f, object);
        },
        .name => |name| {
            const binding = c.scope.lookup(name) orelse
                return fail(c, expr, .{ .name_not_bound = .{ .name = name } });
            push(c, f, binding.value, binding.is_staged, false);
        },
        .ref_to => |ref_to| {
            try desugarPath(c, f, ref_to);
        },
        .let_or_set => |let_or_set| {
            switch (c.sir_expr_data.get(let_or_set.path)) {
                .name => |name| {
                    if (c.scope.lookup(name)) |_|
                        return fail(c, expr, .{ .name_already_bound = .{ .name = name } });

                    const local = f.local_data.append(.{
                        .is_mutable = let_or_set.mut,
                    });

                    _ = f.expr_data.append(.begin);
                    defer _ = f.expr_data.append(.{ .local_let = .{
                        .local = local,
                        .mut = let_or_set.mut,
                    } });
                    try desugarExpr(c, f, let_or_set.value);

                    c.scope.push(.{
                        .name = name,
                        .value = .{ .local = local },
                    });
                },
                .ref_to => |ref_to| {
                    if (let_or_set.mut)
                        return fail(c, expr, .mut_on_assign);

                    _ = f.expr_data.append(.begin);
                    defer _ = f.expr_data.append(.ref_set);

                    try desugarPath(c, f, ref_to);
                    _ = f.expr_data.append(.ref_set_middle);
                    try desugarExpr(c, f, let_or_set.value);
                },
                else => return fail(c, let_or_set.path, .invalid_let_path),
            }

            _ = f.expr_data.append(.begin);
            defer _ = f.expr_data.append(.{ .struct_init = 0 });
        },
        .get => |get| {
            _ = f.expr_data.append(.begin);
            defer _ = f.expr_data.append(.object_get);

            try desugarExpr(c, f, get.object);
            try desugarKey(c, f, get.key);
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
                    push(c, f, binding.value, binding.is_staged, false);
                }
            }
        },
        .call => |call| {
            _ = f.expr_data.append(.begin);
            defer _ = f.expr_data.append(.call);

            try desugarExpr(c, f, call.head);
            try desugarObject(c, f, call.args);
        },
        .block => |block| {
            const scope_saved = c.scope.save();
            defer c.scope.restore(scope_saved);

            _ = f.expr_data.append(.begin);
            defer _ = f.expr_data.append(.block);

            for (block, 0..) |statement, i| {
                if (i < block.len - 1)
                    _ = f.expr_data.append(.begin);
                defer if (i < block.len - 1) {
                    _ = f.expr_data.append(.drop);
                };

                try desugarExpr(c, f, statement);
            }
            if (block.len == 0) {
                _ = f.expr_data.append(.begin);
                defer _ = f.expr_data.append(.{ .struct_init = 0 });
            }
        },
        else => return fail(c, expr, .todo),
    }
}

fn desugarPath(c: *Compiler, f: *dir.FunData, expr: sir.Expr) error{DesugarError}!void {
    const expr_data = c.sir_expr_data.get(expr);
    switch (expr_data) {
        .name => |name| {
            const binding = c.scope.lookup(name) orelse
                return fail(c, expr, .{ .name_not_bound = .{ .name = name } });
            switch (binding.value) {
                .closure => return fail(c, expr, .{ .todo_may_not_close_over_ref = .{ .name = name } }),
                .arg => panic("Shouldn't be able to bind to arg directly", .{}),
                .local => |local| {
                    if (!f.local_data.get(local).is_mutable)
                        return fail(c, expr, .{ .may_not_mutate_immutable_binding = .{ .name = name } });
                },
            }
            push(c, f, binding.value, binding.is_staged, true);
        },
        .get => |get| {
            _ = f.expr_data.append(.begin);
            defer _ = f.expr_data.append(.ref_get);

            try desugarPath(c, f, get.object);
            try desugarKey(c, f, get.key);
        },
        else => return fail(c, expr, .invalid_path),
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

fn push(c: *Compiler, f: *dir.FunData, value: dir.AbstractValue, is_staged: bool, allow_mut: bool) void {
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
            const deref = !is_staged and !allow_mut and f.local_data.get(local).is_mutable;
            if (deref)
                _ = f.expr_data.append(.begin);
            defer if (deref) {
                _ = f.expr_data.append(.ref_deref);
            };
            _ = f.expr_data.append(.{ .local_get = local });
        },
    }
}

fn fail(c: *Compiler, expr: sir.Expr, data: DesugarErrorData) error{DesugarError} {
    c.error_data = .{ .desugar = .{ .expr = expr, .data = data } };
    return error.DesugarError;
}

pub const DesugarErrorData = union(enum) {
    invalid_pattern,
    mut_on_assign,
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
    todo,
};
