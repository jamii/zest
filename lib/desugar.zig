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
    pushNext(c, c.sir_expr_main.?);
    var f = dir.FunData.init(c.allocator);

    {
        emit(c, &f, .return_begin);
        defer emit(c, &f, .return_end);

        try desugarExpr(c, &f);
    }

    c.dir_fun_main = c.dir_fun_data.append(f);
}

fn desugarExpr(c: *Compiler, f: *dir.FunData) error{DesugarError}!void {
    switch (peek(c)) {
        .name, .get_begin, .ref_to_begin => {
            try desugarPath(c, f);
        },
        .i32 => |i| {
            _ = take(c).i32;
            emit(c, f, .{ .i32 = i });
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
            try desugarPattern(c, f, .{ .expr = value }, .let);
            _ = take(c).let_end;
        },
        .fun_begin => {
            const dir_fun = dir_fun: {
                const prev_closure_until_len = c.scope.closure_until_len;
                c.scope.closure_until_len = c.scope.bindings.items.len;
                defer c.scope.closure_until_len = prev_closure_until_len;

                const scope_saved = c.scope.save();
                defer c.scope.restore(scope_saved);

                break :dir_fun try desugarFun(c);
            };
            const dir_fun_data = c.dir_fun_data.get(dir_fun);

            emit(c, f, .fun_init_begin);
            defer emit(c, f, .{ .fun_init_end = .{ .fun = dir_fun } });

            emit(c, f, .struct_init_begin);
            defer emit(c, f, .{ .struct_init_end = dir_fun_data.closure_keys.items.len });

            for (dir_fun_data.closure_keys.items) |name| {
                stageString(c, f, name);
                const binding = c.scope.lookup(name).?;
                desugarBinding(c, f, binding);
            }
        },
        .call_begin => {
            emit(c, f, .call_begin);
            defer emit(c, f, .call_end);

            _ = take(c).call_begin;
            try desugarExpr(c, f);
            try desugarExpr(c, f);
            _ = take(c).call_end;
        },
        .call_builtin_begin => {
            emit(c, f, .call_builtin_begin);
            _ = take(c).call_builtin_begin;
            try desugarExpr(c, f);
            const builtin = take(c).call_builtin_end;
            emit(c, f, .{ .call_builtin_end = builtin });
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
            // XXX
            panic("TODO", .{});
            return fail(c, .todo);
        },
    }
}

fn desugarFun(c: *Compiler) error{DesugarError}!dir.Fun {
    var f = dir.FunData.init(c.allocator);

    _ = take(c).fun_begin;
    try desugarPattern(c, &f, .arg, .args);
    f.pattern_local_count = f.local_data.count();
    f.pattern_expr_count = f.expr_data.count();
    {
        emit(c, &f, .return_begin);
        defer emit(c, &f, .return_end);

        emit(c, &f, .assert_has_no_ref_begin);
        defer emit(c, &f, .assert_has_no_ref_end);

        try desugarExpr(c, &f);
    }
    _ = take(c).fun_end;

    return c.dir_fun_data.append(f);
}

fn desugarPath(c: *Compiler, f: *dir.FunData) error{DesugarError}!void {
    switch (peek(c)) {
        .ref_to_begin => {
            _ = take(c).ref_to_begin;
            _ = try desugarPathPart(c, f, true);
            _ = take(c).ref_to_end;
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
            const is_mut = desugarPathPart(c, f, true);
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
        .arg => emit(c, f, .arg),
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
    }
}

fn desugarKey(c: *Compiler, f: *dir.FunData) error{DesugarError}!void {
    switch (peek(c)) {
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
    arg,
    expr: sir.Expr,
    object_get: struct {
        object: dir.Local,
        key: sir.Expr,
    },
};

const PatternContext = enum { args, let };

fn desugarPattern(c: *Compiler, f: *dir.FunData, input: PatternInput, context: PatternContext) error{DesugarError}!void {
    switch (peek(c)) {
        .name => |name| {
            _ = take(c).name;

            if (c.scope.lookup(name.name)) |_|
                return fail(c, .{ .name_already_bound = .{ .name = name.name } });

            const local = f.local_data.append(.{
                .is_mutable = name.mut,
                .is_tmp = false,
            });

            c.scope.push(.{
                .name = name.name,
                .value = .{ .local = local },
                .mut = name.mut,
            });

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
                .is_mutable = false,
                .is_tmp = true,
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
                try desugarPattern(c, f, .{ .object_get = .{ .object = local, .key = key_expr } }, context);
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
        else => return fail(c, .invalid_pattern),
    }
}

fn desugarPatternInput(c: *Compiler, f: *dir.FunData, input: PatternInput) error{DesugarError}!void {
    switch (input) {
        .arg => emit(c, f, .arg),
        .expr => |expr| {
            pushNext(c, expr);
            try desugarExpr(c, f);
        },
        .object_get => |object_get| {
            emit(c, f, .object_get_begin);
            defer emit(c, f, .object_get_end);

            emit(c, f, .{ .local_get = object_get.object });

            pushNext(c, object_get.key);
            try desugarKey(c, f);
        },
    }
}

fn emit(c: *Compiler, f: *dir.FunData, expr_data: dir.ExprData) void {
    _ = c;
    _ = f.expr_data.append(expr_data);
}

fn peek(c: *Compiler) sir.ExprData {
    const next = &c.sir_expr_next.items[c.sir_expr_next.items.len - 1];
    const expr_data = c.sir_expr_data.get(next.expr);
    return expr_data;
}

fn take(c: *Compiler) sir.ExprData {
    const expr_data = peek(c);
    takeInner(c);
    return expr_data;
}

fn takeIf(c: *Compiler, tag: std.meta.Tag(sir.ExprData)) bool {
    const expr_data = peek(c);
    if (expr_data == tag) {
        takeInner(c);
        return true;
    } else {
        return false;
    }
}

// TODO This is awful
fn takeInner(c: *Compiler) void {
    {
        const next = &c.sir_expr_next.items[c.sir_expr_next.items.len - 1];
        const expr_data = c.sir_expr_data.get(next.expr);
        switch (treePart(expr_data)) {
            .branch_begin => next.ends_remaining += 1,
            .branch_end => next.ends_remaining -= 1,
            .leaf => {},
        }
    }
    while (true) {
        const next = &c.sir_expr_next.items[c.sir_expr_next.items.len - 1];
        next.expr.id += 1;
        if (next.ends_remaining > 0)
            break;
        _ = c.sir_expr_next.pop();
        if (c.sir_expr_next.items.len == 0)
            return;
        if (c.sir_expr_data.get(c.sir_expr_next.items[c.sir_expr_next.items.len - 1].expr) != .indirect)
            break;
    }
    while (true) {
        const next = &c.sir_expr_next.items[c.sir_expr_next.items.len - 1];
        const expr_data = c.sir_expr_data.get(next.expr);
        if (expr_data != .indirect) break;
        c.sir_expr_next.append(.{ .expr = expr_data.indirect, .ends_remaining = 0 }) catch oom();
    }
}

// TODO This is awful
fn pushNext(c: *Compiler, expr: sir.Expr) void {
    c.sir_expr_next.append(.{ .expr = expr, .ends_remaining = 0 }) catch oom();
    while (true) {
        const next = &c.sir_expr_next.items[c.sir_expr_next.items.len - 1];
        const expr_data = c.sir_expr_data.get(next.expr);
        if (expr_data != .indirect) break;
        c.sir_expr_next.append(.{ .expr = expr_data.indirect, .ends_remaining = 0 }) catch oom();
    }
}

fn skipTree(c: *Compiler) sir.Expr {
    const start = c.sir_expr_next.items[c.sir_expr_next.items.len - 1].expr;
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
    todo,
};
