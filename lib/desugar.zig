const std = @import("std");
const panic = std.debug.panic;
const assert = std.debug.assert;
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;

const zest = @import("./zest.zig");
const oom = zest.oom;
const deepEqual = zest.deepEqual;
const treePart = zest.treePart;
const convertPostorderToPreorder = zest.convertPostorderToPreorder;
const Compiler = zest.Compiler;
const Local = zest.Local;
const Value = zest.Value;
const Builtin = zest.Builtin;
const sir = zest.sir;
const dir = zest.dir;

pub fn desugar(c: *Compiler) error{DesugarError}!void {
    var f = dir.FunData.init(c.allocator);
    try desugarExpr(c, &f);
    emit(c, &f, .assert_has_no_ref);
    emit(c, &f, .@"return");
    convertPostorderToPreorder(c, dir.Expr, dir.ExprData, f.expr_data_post, &f.expr_data_pre);
    c.dir_fun_main = c.dir_fun_data.append(f);
}

fn desugarExpr(c: *Compiler, f: *dir.FunData) error{DesugarError}!void {
    switch (take(c)) {
        .name, .get, .ref_to => {
            untake(c);
            try desugarPath(c, f);
        },
        .i64 => |i| {
            emit(c, f, .{ .i64 = i });
        },
        .string => |string| {
            emit(c, f, .{ .string = string });
        },
        .object => |object| {
            for (0..object.count) |_|
                try desugarKeyValue(c, f);
            emit(c, f, .{ .struct_init = .{ .count = object.count } });
        },
        .let => {
            const pattern = skipTree(c);
            try desugarExpr(c, f);
            var bindings = ArrayList(dir.Binding).init(c.allocator);
            {
                const next = c.sir_expr_next;
                c.sir_expr_next = pattern;
                defer c.sir_expr_next = next;

                try desugarPattern(c, f, &bindings, .let);
            }
            for (bindings.items) |binding| {
                c.scope.push(binding);
            }
        },
        .fun => {
            untake(c);
            const dir_funs = dir_fun: {
                const prev_closure_until_len = c.scope.closure_until_len;
                c.scope.closure_until_len = c.scope.bindings.items.len;
                defer c.scope.closure_until_len = prev_closure_until_len;

                const scope_saved = c.scope.save();
                defer c.scope.restore(scope_saved);

                break :dir_fun try desugarFun(c);
            };
            const body_fun_data = c.dir_fun_data.get(dir_funs.body);
            for (body_fun_data.closure_keys.items) |name| {
                stageString(c, f, name);
                const binding = c.scope.lookup(name).?;
                desugarBinding(c, f, binding);
            }
            emit(c, f, .{ .struct_init = .{ .count = body_fun_data.closure_keys.items.len } });
            emit(c, f, .{ .fun_init = .{ .fun = dir_funs.wrapper } });
        },
        .call => {
            try desugarExpr(c, f); // fun
            try desugarExpr(c, f); // arg struct
            emit(c, f, .{ .call = .{ .arg_count = 1 } });
        },
        .call_slash => {
            const arg0 = skipTree(c);
            try desugarExpr(c, f); // fun;
            const object = take(c).object; // arg struct
            emit(c, f, .stage_begin);
            emit(c, f, .{ .i64 = 0 });
            emit(c, f, .{ .stage = .{} });
            {
                const next = c.sir_expr_next;
                c.sir_expr_next = arg0;
                defer c.sir_expr_next = next;

                try desugarExpr(c, f); // arg0
            }
            for (0..object.count) |_|
                try desugarKeyValue(c, f);
            emit(c, f, .{ .struct_init = .{ .count = object.count + 1 } });
            emit(c, f, .{ .call = .{ .arg_count = 1 } });
        },
        .call_builtin => |builtin| {
            for (0..builtin.argCount()) |arg_index| {
                const is_staged = (builtin == .load and arg_index == 1) or
                    (builtin == .@"size-of" and arg_index == 0) or
                    (builtin == .@"union-has-key" and arg_index == 1);
                if (is_staged)
                    emit(c, f, .stage_begin);
                try desugarExpr(c, f);
                if (is_staged)
                    emit(c, f, .{ .stage = .{} });
            }
            emit(c, f, .{ .call_builtin = builtin });
        },
        .repr_of => {
            emit(c, f, .{ .repr_of_begin = .{} });
            try desugarExpr(c, f);
            emit(c, f, .repr_of);
        },
        .make => {
            try stageExpr(c, f);
            try desugarExpr(c, f);
            emit(c, f, .make);
        },
        .make_slash => {
            const arg = skipTree(c);
            try stageExpr(c, f);
            emit(c, f, .stage_begin);
            emit(c, f, .{ .i64 = 0 });
            emit(c, f, .{ .stage = .{} });
            {
                const next = c.sir_expr_next;
                c.sir_expr_next = arg;
                defer c.sir_expr_next = next;

                try desugarExpr(c, f); // arg
            }
            emit(c, f, .{ .struct_init = .{ .count = 1 } });
            emit(c, f, .make);
        },
        .block => |block| {
            const scope_saved = c.scope.save();
            defer c.scope.restore(scope_saved);

            const block_start = f.expr_data_post.count();
            for (block.count) |_| {
                try desugarExpr(c, f);
            }
            emit(c, f, .{ .block = .{ .count = countTreesSince(c, f, .{ .id = block_start }) } });
        },
        .@"if" => {
            try desugarExpr(c, f);
            emit(c, f, .if_then);
            try desugarExpr(c, f);
            emit(c, f, .if_else);
            try desugarExpr(c, f);
            emit(c, f, .@"if");
        },
        .@"while" => {
            emit(c, f, .while_begin);
            try desugarExpr(c, f);
            emit(c, f, .while_body);
            try desugarExpr(c, f);
            emit(c, f, .@"while");
        },
        else => {
            return fail(c, .todo);
        },
    }
}

fn desugarFun(c: *Compiler) error{DesugarError}!struct { wrapper: dir.Fun, body: dir.Fun } {
    _ = take(c).fun;

    var bindings = ArrayList(dir.Binding).init(c.allocator);

    const body_fun = c.dir_fun_data.append(dir.FunData.init(c.allocator));

    const wrapper_fun = wrapper_fun: {
        var f = dir.FunData.init(c.allocator);
        f.@"inline" = true;
        const block_start = f.expr_data_post.count();
        const arg = f.arg_data.append(.{});
        emit(c, &f, .{ .arg = arg });
        try desugarPattern(c, &f, &bindings, .args);
        emit(c, &f, .closure);
        emit(c, &f, .{ .fun_init = .{ .fun = body_fun } });
        for (bindings.items) |binding| {
            desugarBinding(c, &f, .{
                .name = binding.name,
                .value = binding.value,
                .mut = binding.mut,
                .is_staged = false,
            });
        }
        emit(c, &f, .{ .call = .{ .arg_count = bindings.items.len } });
        emit(c, &f, .{ .block = .{ .count = countTreesSince(c, &f, .{ .id = block_start }) } });
        emit(c, &f, .@"return");
        convertPostorderToPreorder(c, dir.Expr, dir.ExprData, f.expr_data_post, &f.expr_data_pre);
        break :wrapper_fun c.dir_fun_data.append(f);
    };

    {
        const scope_saved = c.scope.save();
        defer c.scope.restore(scope_saved);

        const f = c.dir_fun_data.getPtr(body_fun);
        for (bindings.items) |binding| {
            const arg = f.arg_data.append(.{});
            c.scope.push(.{
                .name = binding.name,
                .value = .{ .arg = arg },
                .mut = binding.mut,
            });
        }
        try desugarExpr(c, f);
        emit(c, f, .assert_has_no_ref);
        emit(c, f, .@"return");
        convertPostorderToPreorder(c, dir.Expr, dir.ExprData, f.expr_data_post, &f.expr_data_pre);
    }

    return .{
        .wrapper = wrapper_fun,
        .body = body_fun,
    };
}

fn desugarPath(c: *Compiler, f: *dir.FunData) error{DesugarError}!void {
    switch (take(c)) {
        .ref_to => {
            _ = try desugarPathPart(c, f, true);
        },
        else => {
            untake(c);
            const is_mut = try desugarPathPart(c, f, false);
            if (is_mut) emit(c, f, .ref_deref);
        },
    }
}

fn desugarPathPart(c: *Compiler, f: *dir.FunData, must_be_mut: bool) error{DesugarError}!bool {
    switch (take(c)) {
        .name => |name| {
            if (name.mut)
                return fail(c, .meaningless_mut);
            const binding = c.scope.lookup(name.name) orelse
                return fail(c, .{ .name_not_bound = .{ .name = name.name } });
            if (must_be_mut and !binding.mut)
                return fail(c, .{ .may_not_mutate_immutable_binding = .{ .name = name.name } });
            desugarBinding(c, f, binding);
            return binding.mut;
        },
        .get => {
            const is_mut = try desugarPathPart(c, f, must_be_mut);
            try desugarKey(c, f);
            emit(c, f, if (is_mut) .ref_get else .object_get);
            return is_mut;
        },
        .ref_to => {
            const is_mut = try desugarPathPart(c, f, true);
            return is_mut;
        },
        else => {
            untake(c);
            if (must_be_mut)
                return fail(c, .invalid_path);
            try desugarExpr(c, f);
            return false;
        },
    }
}

fn desugarBinding(c: *Compiler, f: *dir.FunData, binding: dir.BindingInfo) void {
    if (binding.is_staged)
        emit(c, f, .{ .unstage_begin = .{} });

    switch (binding.value) {
        .arg => |arg| emit(c, f, .{ .arg = arg }),
        .closure => |name| {
            if (!f.closure_keys_index.contains(name)) {
                f.closure_keys_index.put(name, {}) catch oom();
                f.closure_keys.append(name) catch oom();
            }
            emit(c, f, .closure);
            stageString(c, f, name);
            emit(c, f, .object_get);
        },
        .local => |local| {
            emit(c, f, .{ .local_get = local });
        },
        .constant => |constant| {
            emit(c, f, constant);
        },
    }

    if (binding.is_staged)
        emit(c, f, .unstage);
}

fn desugarKey(c: *Compiler, f: *dir.FunData) error{DesugarError}!void {
    switch (take(c)) {
        .name => |name| {
            if (name.mut)
                return fail(c, .meaningless_mut);
            stageString(c, f, name.name);
        },
        else => {
            untake(c);
            try stageExpr(c, f);
        },
    }
}

fn desugarKeyValue(c: *Compiler, f: *dir.FunData) error{DesugarError}!void {
    switch (take(c)) {
        .key_value => {
            try desugarKey(c, f);
            try desugarExpr(c, f);
        },
        .pos_value => |pos| {
            emit(c, f, .stage_begin);
            emit(c, f, .{ .i64 = pos });
            emit(c, f, .{ .stage = .{} });
            try desugarExpr(c, f);
        },
        else => |other| panic("Invalid key_value: {}", .{other}),
    }
}

fn stageExpr(c: *Compiler, f: *dir.FunData) error{DesugarError}!void {
    emit(c, f, .stage_begin);
    const already_staged = c.scope.staged_until_len != null;
    if (!already_staged) {
        c.scope.staged_until_len = c.scope.bindings.items.len;
    }
    try desugarExpr(c, f);
    if (!already_staged) {
        c.scope.staged_until_len = null;
    }
    // TODO Do we need to emit .stage when already_staged?
    emit(c, f, .{ .stage = .{} });
}

fn stageString(c: *Compiler, f: *dir.FunData, string: []const u8) void {
    emit(c, f, .stage_begin);
    emit(c, f, .{ .string = string });
    emit(c, f, .{ .stage = .{} });
}

const PatternContext = enum { args, let };

fn desugarPattern(c: *Compiler, f: *dir.FunData, bindings: *ArrayList(dir.Binding), context: PatternContext) error{DesugarError}!void {
    switch (take(c)) {
        .name => |name| {
            if (c.scope.lookup(name.name)) |_|
                return fail(c, .{ .name_already_bound = .{ .name = name.name } });
            const local = f.local_data.append(.{
                .is_tmp = false,
                .is_mutable = name.mut,
            });
            bindings.append(.{
                .name = name.name,
                .value = .{ .local = local },
                .mut = name.mut,
            }) catch oom();
            emit(c, f, if (name.mut)
                switch (context) {
                    .args => .assert_is_ref,
                    .let => .assert_has_no_ref,
                }
            else
                .assert_has_no_ref_visible);
            // TODO The need for context here indicates that maybe we should treat mut uniformly and add a ref keyword for args.
            if (context == .let and name.mut) {
                emit(c, f, .ref_init);
            }
            emit(c, f, .{ .local_let = local });
        },
        .object => |object| {
            emit(c, f, .{ .assert_object = .{ .count = object.count } });
            var local: ?dir.Local = null;
            if (object.count > 1) {
                local = f.local_data.append(.{
                    .is_tmp = true,
                    .is_mutable = false,
                });
                emit(c, f, .{ .local_let = local.? });
            }
            for (0..object.count) |_| {
                if (object.count > 1) emit(c, f, .{ .local_get = local.? });
                switch (take(c)) {
                    .key_value => {
                        try desugarKey(c, f);
                        emit(c, f, .object_get);
                        try desugarPattern(c, f, bindings, context);
                    },
                    .pos_value => |pos| {
                        emit(c, f, .stage_begin);
                        emit(c, f, .{ .i64 = pos });
                        emit(c, f, .{ .stage = .{} });
                        emit(c, f, .object_get);
                        try desugarPattern(c, f, bindings, context);
                    },
                    else => |other| panic("Invalid key_value: {}", .{other}),
                }
            }
        },
        .ref_to => {
            untake(c);
            const local = f.local_data.append(.{
                .is_tmp = true,
                .is_mutable = false,
            });
            emit(c, f, .{ .local_let = local });
            try desugarPath(c, f);
            emit(c, f, .{ .local_get = local });
            emit(c, f, .ref_set);
        },
        .make_slash => {
            const arg = skipTree(c);

            const local = f.local_data.append(.{
                .is_tmp = true,
                .is_mutable = false,
            });
            emit(c, f, .{ .local_let = local });
            try stageExpr(c, f);
            emit(c, f, .stage_begin);
            emit(c, f, .{ .i64 = 0 });
            emit(c, f, .{ .stage = .{} });
            emit(c, f, .{ .local_get = local });
            emit(c, f, .{ .struct_init = .{ .count = 1 } });
            emit(c, f, .make);

            {
                const next = c.sir_expr_next;
                c.sir_expr_next = arg;
                defer c.sir_expr_next = next;

                try desugarPattern(c, f, bindings, context);
            }
        },
        else => return fail(c, .invalid_pattern),
    }
}

fn emit(c: *Compiler, f: *dir.FunData, expr_data: dir.ExprData) void {
    _ = c;
    _ = f.expr_data_post.append(expr_data);
}

fn take(c: *Compiler) sir.ExprData {
    const expr_data = c.sir_expr_data_pre.get(c.sir_expr_next);
    c.sir_expr_next.id += 1;
    return expr_data;
}

fn untake(c: *Compiler) void {
    c.sir_expr_next.id -= 1;
}

fn skipTree(c: *Compiler) sir.Expr {
    const start = c.sir_expr_next;
    var children_remaining: usize = 1;
    while (children_remaining > 0) {
        children_remaining += take(c).childCount(c);
        children_remaining -= 1;
    }
    return start;
}

fn countTreesSince(c: *Compiler, f: *dir.FunData, start: dir.Expr) usize {
    var count: usize = 0;
    var expr = start;
    while (expr.id < f.expr_data_post.count()) : (expr.id += 1) {
        count += 1;
        count -= f.expr_data_post.get(expr).childCount(c);
    }
    return count;
}

fn fail(c: *Compiler, data: DesugarErrorData) error{DesugarError} {
    c.error_data = .{ .desugar = .{
        .expr = .{ .id = c.sir_expr_next.id - 1 },
        .data = data,
    } };
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
    may_not_mutate_immutable_binding: struct {
        name: []const u8,
    },
    invalid_let_path,
    invalid_path,
    meaningless_mut,
    wrong_builtin_arg_count: struct {
        builtin: Builtin,
        expected: usize,
        found: usize,
    },
    todo,
};
