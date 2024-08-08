const std = @import("std");
const panic = std.debug.panic;
const assert = std.debug.assert;
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;

const zest = @import("./zest.zig");
const oom = zest.oom;
const deepEqual = zest.deepEqual;
const treePart = zest.treePart;
const Compiler = zest.Compiler;
const Local = zest.Local;
const Value = zest.Value;
const Builtin = zest.Builtin;
const sir = zest.sir;
const dir = zest.dir;

pub fn desugar(c: *Compiler) error{DesugarError}!void {
    c.sir_expr_next = c.sir_expr_main.?;
    var f = dir.FunData.init(c.allocator);

    {
        emit(c, &f, .return_begin);
        defer emit(c, &f, .return_end);

        emit(c, &f, .assert_has_no_ref_begin);
        defer emit(c, &f, .assert_has_no_ref_end);

        try desugarExpr(c, &f);
    }

    c.dir_fun_main = c.dir_fun_data.append(f);
}

fn desugarExpr(c: *Compiler, f: *dir.FunData) error{DesugarError}!void {
    switch (peek(c)) {
        .indirect => |expr| {
            const next = c.sir_expr_next;
            c.sir_expr_next = expr;
            try desugarExpr(c, f);
            c.sir_expr_next = .{ .id = next.id + 1 };
        },
        .name, .get_begin, .ref_to_begin => {
            try desugarPath(c, f);
        },
        .i64 => |i| {
            _ = take(c).i64;
            emit(c, f, .{ .i64 = i });
        },
        .string => |string| {
            _ = take(c).string;
            emit(c, f, .{ .string = string });
        },
        .object_begin => {
            var count: usize = 0;

            emit(c, f, .struct_init_begin);
            defer emit(c, f, .{ .struct_init_end = count });

            _ = take(c).object_begin;
            while (true) {
                if (takeIf(c, .object_end))
                    break;
                count += 1;
                try desugarKey(c, f);
                try desugarExpr(c, f);
            }
        },
        .let_begin => {
            _ = take(c).let_begin;
            const value = skipTree(c);
            var bindings = ArrayList(dir.Binding).init(c.allocator);
            try desugarPattern(c, f, &bindings, .{ .expr = value }, .let);
            for (bindings.items) |binding| {
                c.scope.push(binding);
            }
            _ = take(c).let_end;
        },
        .fun_begin => {
            const dir_funs = dir_fun: {
                const prev_closure_until_len = c.scope.closure_until_len;
                c.scope.closure_until_len = c.scope.bindings.items.len;
                defer c.scope.closure_until_len = prev_closure_until_len;

                const scope_saved = c.scope.save();
                defer c.scope.restore(scope_saved);

                break :dir_fun try desugarFun(c);
            };
            const body_fun_data = c.dir_fun_data.get(dir_funs.body);

            emit(c, f, .fun_init_begin);
            defer emit(c, f, .{ .fun_init_end = .{ .fun = dir_funs.wrapper } });

            emit(c, f, .struct_init_begin);
            defer emit(c, f, .{ .struct_init_end = body_fun_data.closure_keys.items.len });

            for (body_fun_data.closure_keys.items) |name| {
                stageString(c, f, name);
                const binding = c.scope.lookup(name).?;
                desugarBinding(c, f, binding);
            }
        },
        .call_begin => {
            emit(c, f, .call_begin);
            defer emit(c, f, .{ .call_end = .{ .arg_count = 1 } });

            _ = take(c).call_begin;
            try desugarExpr(c, f);
            try desugarExpr(c, f);
            _ = take(c).call_end;
        },
        .call_builtin_begin => {
            const builtin = take(c).call_builtin_begin;
            emit(c, f, .call_builtin_begin);
            var arg_count: usize = 0;
            while (peek(c) != .call_builtin_end) {
                const is_staged =
                    (builtin == .load and arg_count == 1) or
                    (builtin == .@"size-of" and arg_count == 0);

                if (is_staged) emit(c, f, .stage_begin);
                defer if (is_staged) emit(c, f, .stage_end);

                try desugarExpr(c, f);
                arg_count += 1;
            }
            if (arg_count != builtin.argCount())
                return fail(c, .{ .wrong_builtin_arg_count = .{
                    .builtin = builtin,
                    .expected = builtin.argCount(),
                    .found = arg_count,
                } });
            _ = take(c).call_builtin_end;
            emit(c, f, .{ .call_builtin_end = builtin });
        },
        .make_begin => {
            emit(c, f, .make_begin);
            defer emit(c, f, .make_end);

            _ = take(c).make_begin;
            try stageExpr(c, f);
            try desugarExpr(c, f);
            _ = take(c).make_end;
        },
        .block_begin => {
            _ = take(c).block_begin;

            if (takeIf(c, .block_end)) {
                // Invariant: dir/tir blocks are never empty.
                emit(c, f, .struct_init_begin);
                defer emit(c, f, .{ .struct_init_end = 0 });
                return;
            }

            const scope_saved = c.scope.save();
            defer c.scope.restore(scope_saved);

            emit(c, f, .block_begin);
            defer emit(c, f, .block_end);

            while (true) {
                if (takeIf(c, .block_last))
                    emit(c, f, .block_last);
                if (takeIf(c, .block_end))
                    break;
                try desugarExpr(c, f);
            }
        },
        .if_begin => {
            _ = take(c).if_begin;
            emit(c, f, .if_begin);
            try desugarExpr(c, f);
            _ = take(c).if_then;
            emit(c, f, .if_then);
            try desugarExpr(c, f);
            _ = take(c).if_else;
            emit(c, f, .if_else);
            try desugarExpr(c, f);
            _ = take(c).if_end;
            emit(c, f, .if_end);
        },
        .while_begin => {
            _ = take(c).while_begin;
            emit(c, f, .while_begin);
            try desugarExpr(c, f);
            _ = take(c).while_body;
            emit(c, f, .while_body);
            try desugarExpr(c, f);
            _ = take(c).while_end;
            emit(c, f, .while_end);
        },
        else => {
            return fail(c, .todo);
        },
    }
}

fn desugarFun(c: *Compiler) error{DesugarError}!struct { wrapper: dir.Fun, body: dir.Fun } {
    _ = take(c).fun_begin;

    var bindings = ArrayList(dir.Binding).init(c.allocator);
    var body_fun_init: ?dir.Expr = null;

    const wrapper_fun = wrapper_fun: {
        var f = dir.FunData.init(c.allocator);
        f.@"inline" = true;
        {
            const arg = f.arg_data.append(.{});

            emit(c, &f, .return_begin);
            defer emit(c, &f, .return_end);

            emit(c, &f, .block_begin);
            defer emit(c, &f, .block_end);

            try desugarPattern(c, &f, &bindings, .{ .arg = arg }, .args);

            emit(c, &f, .block_last);

            emit(c, &f, .call_begin);
            defer emit(c, &f, .{ .call_end = .{ .arg_count = bindings.items.len } });

            {
                emit(c, &f, .fun_init_begin);
                defer body_fun_init = f.expr_data.append(.{ .fun_init_end = .{ .fun = .{ .id = 0 } } });

                emit(c, &f, .closure);
            }

            for (bindings.items) |binding| {
                desugarBinding(c, &f, .{
                    .name = binding.name,
                    .value = binding.value,
                    .mut = binding.mut,
                    .is_staged = false,
                });
            }
        }
        break :wrapper_fun c.dir_fun_data.append(f);
    };

    const body_fun = body_fun: {
        var f = dir.FunData.init(c.allocator);
        {
            const scope_saved = c.scope.save();
            defer c.scope.restore(scope_saved);

            for (bindings.items) |binding| {
                const arg = f.arg_data.append(.{});
                c.scope.push(.{
                    .name = binding.name,
                    .value = .{ .arg = arg },
                    .mut = binding.mut,
                });
            }

            emit(c, &f, .return_begin);
            defer emit(c, &f, .return_end);

            emit(c, &f, .assert_has_no_ref_begin);
            defer emit(c, &f, .assert_has_no_ref_end);

            try desugarExpr(c, &f);
        }
        break :body_fun c.dir_fun_data.append(f);
    };

    _ = take(c).fun_end;

    c.dir_fun_data.getPtr(wrapper_fun).expr_data.getPtr(body_fun_init.?).fun_init_end.fun = body_fun;

    return .{
        .wrapper = wrapper_fun,
        .body = body_fun,
    };
}

fn desugarPath(c: *Compiler, f: *dir.FunData) error{DesugarError}!void {
    switch (peek(c)) {
        .indirect => |expr| {
            const next = c.sir_expr_next;
            c.sir_expr_next = expr;
            try desugarPath(c, f);
            c.sir_expr_next = .{ .id = next.id + 1 };
        },
        .ref_to_begin => {
            _ = try desugarPathPart(c, f, true);
        },
        else => {
            const nop = f.expr_data.append(.nop_begin);
            const is_mut = try desugarPathPart(c, f, false);
            if (is_mut) {
                f.expr_data.getPtr(nop).* = .ref_deref_begin;
                emit(c, f, .ref_deref_end);
            } else {
                emit(c, f, .nop_end);
            }
        },
    }
}

fn desugarPathPart(c: *Compiler, f: *dir.FunData, must_be_mut: bool) error{DesugarError}!bool {
    switch (peek(c)) {
        .indirect => |expr| {
            const next = c.sir_expr_next;
            c.sir_expr_next = expr;
            const is_mut = try desugarPathPart(c, f, must_be_mut);
            c.sir_expr_next = .{ .id = next.id + 1 };
            return is_mut;
        },
        .name => |name| {
            _ = take(c).name;
            if (name.mut)
                return fail(c, .meaningless_mut);
            const binding = c.scope.lookup(name.name) orelse
                return fail(c, .{ .name_not_bound = .{ .name = name.name } });
            if (must_be_mut and !binding.mut)
                return fail(c, .{ .may_not_mutate_immutable_binding = .{ .name = name.name } });
            desugarBinding(c, f, binding);
            return binding.mut;
        },
        .get_begin => {
            const nop = f.expr_data.append(.nop_begin);

            _ = take(c).get_begin;
            const is_mut = try desugarPathPart(c, f, must_be_mut);
            try desugarKey(c, f);
            _ = take(c).get_end;

            if (is_mut) {
                f.expr_data.getPtr(nop).* = .ref_get_begin;
                emit(c, f, .ref_get_end);
            } else {
                f.expr_data.getPtr(nop).* = .object_get_begin;
                emit(c, f, .object_get_end);
            }

            return is_mut;
        },
        .ref_to_begin => {
            _ = take(c).ref_to_begin;
            const is_mut = try desugarPathPart(c, f, true);
            _ = take(c).ref_to_end;
            return is_mut;
        },
        else => {
            if (must_be_mut)
                return fail(c, .invalid_path);
            try desugarExpr(c, f);
            return false;
        },
    }
}

fn desugarBinding(c: *Compiler, f: *dir.FunData, binding: dir.BindingInfo) void {
    if (binding.is_staged) {
        emit(c, f, .unstage_begin);
    }
    defer if (binding.is_staged) {
        emit(c, f, .unstage_end);
    };

    switch (binding.value) {
        .arg => |arg| emit(c, f, .{ .arg = arg }),
        .closure => |name| {
            if (!f.closure_keys_index.contains(name)) {
                f.closure_keys_index.put(name, {}) catch oom();
                f.closure_keys.append(name) catch oom();
            }

            emit(c, f, .object_get_begin);
            defer emit(c, f, .object_get_end);

            emit(c, f, .closure);
            stageString(c, f, name);
        },
        .local => |local| {
            emit(c, f, .{ .local_get = local });
        },
        .constant => |constant| {
            emit(c, f, constant);
        },
    }
}

fn desugarKey(c: *Compiler, f: *dir.FunData) error{DesugarError}!void {
    switch (peek(c)) {
        .indirect => |expr| {
            const next = c.sir_expr_next;
            c.sir_expr_next = expr;
            try desugarKey(c, f);
            c.sir_expr_next = .{ .id = next.id + 1 };
        },
        .name => |name| {
            _ = take(c).name;
            if (name.mut)
                return fail(c, .meaningless_mut);
            stageString(c, f, name.name);
        },
        else => try stageExpr(c, f),
    }
}

fn stageExpr(c: *Compiler, f: *dir.FunData) error{DesugarError}!void {
    const already_staged = c.scope.staged_until_len != null;
    if (!already_staged) {
        emit(c, f, .stage_begin);
        c.scope.staged_until_len = c.scope.bindings.items.len;
    }
    defer if (!already_staged) {
        emit(c, f, .stage_end);
        c.scope.staged_until_len = null;
    };

    try desugarExpr(c, f);
}

fn stageString(c: *Compiler, f: *dir.FunData, string: []const u8) void {
    const already_staged = c.scope.staged_until_len != null;
    if (!already_staged) {
        emit(c, f, .stage_begin);
    }
    defer if (!already_staged) {
        emit(c, f, .stage_end);
    };

    emit(c, f, .{ .string = string });
}

// Would be nicer for this to be a closure that emits some exprs, but zig closures are painful.
const PatternInput = union(enum) {
    arg: dir.Arg,
    expr: sir.Expr,
    object_get: struct {
        object: dir.Local,
        key: sir.Expr,
    },
    make: struct {
        head: sir.Expr,
        input: *PatternInput,
    },
};

const PatternContext = enum { args, let };

fn desugarPattern(c: *Compiler, f: *dir.FunData, bindings: *ArrayList(dir.Binding), input: PatternInput, context: PatternContext) error{DesugarError}!void {
    switch (peek(c)) {
        .indirect => |expr| {
            const next = c.sir_expr_next;
            c.sir_expr_next = expr;
            try desugarPattern(c, f, bindings, input, context);
            c.sir_expr_next = .{ .id = next.id + 1 };
        },
        .name => |name| {
            _ = take(c).name;

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

            emit(c, f, .local_let_begin);
            defer emit(c, f, .{ .local_let_end = local });

            // TODO The need for context here indicates that maybe we should treat mut uniformly and add a ref keyword for args.

            if (context == .let and name.mut) {
                emit(c, f, .ref_init_begin);
            }
            defer if (context == .let and name.mut) {
                emit(c, f, .ref_init_end);
            };

            const assert_begin: dir.ExprData, const assert_end: dir.ExprData = if (name.mut)
                switch (context) {
                    .args => .{
                        .assert_is_ref_begin,
                        .assert_is_ref_end,
                    },
                    .let => .{
                        .assert_has_no_ref_begin,
                        .assert_has_no_ref_end,
                    },
                }
            else
                .{
                    .assert_has_no_ref_visible_begin,
                    .assert_has_no_ref_visible_end,
                };
            emit(c, f, assert_begin);
            defer emit(c, f, assert_end);

            try desugarPatternInput(c, f, input);
        },
        .object_begin => {
            const local = f.local_data.append(.{
                .is_tmp = true,
                .is_mutable = false,
            });

            const assert_object_end = assert_object_end: {
                emit(c, f, .local_let_begin);
                defer emit(c, f, .{ .local_let_end = local });

                emit(c, f, .assert_object_begin);
                try desugarPatternInput(c, f, input);
                break :assert_object_end f.expr_data.append(.{ .assert_object_end = .{ .count = 0 } });
            };

            var count: usize = 0;
            _ = take(c).object_begin;
            while (true) {
                if (takeIf(c, .object_end))
                    break;

                const key_expr = skipTree(c);
                try desugarPattern(c, f, bindings, .{ .object_get = .{ .object = local, .key = key_expr } }, context);
                count += 1;
            }

            f.expr_data.getPtr(assert_object_end).assert_object_end.count = count;
        },
        .ref_to_begin => {
            emit(c, f, .ref_set_begin);
            defer emit(c, f, .ref_set_end);

            try desugarPath(c, f);
            try desugarPatternInput(c, f, input);
        },
        .make_begin => {
            _ = take(c).make_begin;

            const head_expr = skipTree(c);

            _ = take(c).object_begin;
            var key = take(c);
            while (key == .indirect) {
                key = c.sir_expr_data.get(key.indirect);
            }
            if (key != .i64 or key.i64 != 0) return fail(c, .invalid_pattern);
            try desugarPattern(c, f, bindings, .{ .make = .{ .head = head_expr, .input = c.box(input) } }, context);
            if (take(c) != .object_end) return fail(c, .invalid_pattern);

            _ = take(c).make_end;
        },
        else => return fail(c, .invalid_pattern),
    }
}

fn desugarPatternInput(c: *Compiler, f: *dir.FunData, input: PatternInput) error{DesugarError}!void {
    switch (input) {
        .arg => |arg| emit(c, f, .{ .arg = arg }),
        .expr => |expr| {
            const next = c.sir_expr_next;
            c.sir_expr_next = expr;
            try desugarExpr(c, f);
            c.sir_expr_next = next;
        },
        .object_get => |object_get| {
            emit(c, f, .object_get_begin);
            defer emit(c, f, .object_get_end);

            emit(c, f, .{ .local_get = object_get.object });

            const next = c.sir_expr_next;
            c.sir_expr_next = object_get.key;
            try desugarKey(c, f);
            c.sir_expr_next = next;
        },
        .make => |make| {
            emit(c, f, .make_begin);
            defer emit(c, f, .make_end);

            const next = c.sir_expr_next;
            c.sir_expr_next = make.head;
            try stageExpr(c, f);
            c.sir_expr_next = next;

            emit(c, f, .struct_init_begin);
            defer emit(c, f, .{ .struct_init_end = 1 });

            {
                emit(c, f, .stage_begin);
                defer emit(c, f, .stage_end);

                emit(c, f, .{ .i64 = 0 });
            }
            try desugarPatternInput(c, f, make.input.*);
        },
    }
}

fn emit(c: *Compiler, f: *dir.FunData, expr_data: dir.ExprData) void {
    _ = c;
    _ = f.expr_data.append(expr_data);
}

fn peek(c: *Compiler) sir.ExprData {
    return c.sir_expr_data.get(c.sir_expr_next);
}

fn take(c: *Compiler) sir.ExprData {
    const expr_data = peek(c);
    c.sir_expr_next.id += 1;
    return expr_data;
}

fn takeIf(c: *Compiler, tag: std.meta.Tag(sir.ExprData)) bool {
    const expr_data = peek(c);
    if (expr_data == tag) {
        c.sir_expr_next.id += 1;
        return true;
    } else {
        return false;
    }
}

fn skipTree(c: *Compiler) sir.Expr {
    const start = c.sir_expr_next;
    var ends_remaining: usize = 0;
    while (true) {
        switch (treePart(take(c))) {
            .branch_begin => ends_remaining += 1,
            .branch_end => ends_remaining -= 1,
            .leaf => {},
        }
        if (ends_remaining == 0) break;
    }
    return start;
}

fn fail(c: *Compiler, data: DesugarErrorData) error{DesugarError} {
    c.error_data = .{ .desugar = data };
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
