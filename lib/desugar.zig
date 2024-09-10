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
    c.sir_expr_next = c.sir_expr_main.?;
    var f = dir.FunData.init(c.allocator);

    try desugarExpr(c, &f);
    emit(c, &f, .assert_has_no_ref);
    emit(c, &f, .@"return");
    convertPostorderToPreorder(c, dir.Expr, dir.ExprData, f.expr_data_post, &f.expr_data_pre);

    c.dir_fun_main = c.dir_fun_data.append(f);
}

fn desugarExpr(c: *Compiler, f: *dir.FunData) error{DesugarError}!void {
    switch (peek(c)) {
        .indirect => |expr| {
            const next = c.sir_expr_next;
            c.sir_expr_next = expr;
            try desugarExpr(c, f);
            c.sir_expr_next.id = next.id + 1;
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
            _ = take(c).object_begin;
            var count: usize = 0;
            while (true) {
                if (takeIf(c, .object_end))
                    break;
                count += 1;
                try desugarKey(c, f);
                try desugarExpr(c, f);
            }
            emit(c, f, .{ .struct_init = .{ .count = count } });
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
            for (body_fun_data.closure_keys.items) |name| {
                stageString(c, f, name);
                const binding = c.scope.lookup(name).?;
                desugarBinding(c, f, binding);
            }
            emit(c, f, .{ .struct_init = .{ .count = body_fun_data.closure_keys.items.len } });
            emit(c, f, .{ .fun_init = .{ .fun = dir_funs.wrapper } });
        },
        .call_begin => {
            _ = take(c).call_begin;
            try desugarExpr(c, f);
            try desugarExpr(c, f);
            _ = take(c).call_end;
            emit(c, f, .{ .call = .{ .arg_count = 1 } });
        },
        .call_builtin_begin => {
            const builtin = take(c).call_builtin_begin;
            var arg_count: usize = 0;
            while (peek(c) != .call_builtin_end) {
                const is_staged = (builtin == .load and arg_count == 1) or
                    (builtin == .@"size-of" and arg_count == 0) or
                    (builtin == .@"union-has-key" and arg_count == 1);
                if (is_staged)
                    emit(c, f, .stage_begin);
                try desugarExpr(c, f);
                if (is_staged)
                    emit(c, f, .{ .stage = .{ .mapping = .{ .id = 0 } } });
                arg_count += 1;
            }
            if (arg_count != builtin.argCount())
                return fail(c, .{ .wrong_builtin_arg_count = .{
                    .builtin = builtin,
                    .expected = builtin.argCount(),
                    .found = arg_count,
                } });
            _ = take(c).call_builtin_end;
            emit(c, f, .{ .call_builtin = builtin });
        },
        .repr_of_begin => {
            _ = take(c).repr_of_begin;
            emit(c, f, .{ .repr_of_begin = .{ .mapping = .{ .id = 0 } } });
            try desugarExpr(c, f);
            _ = take(c).repr_of_end;
            emit(c, f, .repr_of);
        },
        .make_begin => {
            _ = take(c).make_begin;
            try stageExpr(c, f);
            try desugarExpr(c, f);
            _ = take(c).make_end;
            emit(c, f, .make);
        },
        .block_begin => {
            const scope_saved = c.scope.save();
            defer c.scope.restore(scope_saved);

            const block_start = f.expr_data_post.count();
            _ = take(c).block_begin;
            while (true) {
                if (takeIf(c, .block_end)) break;
                _ = takeIf(c, .block_last);
                try desugarExpr(c, f);
            }
            emit(c, f, .{ .block = .{ .count = countTreesSince(c, f, .{ .id = block_start }) } });
        },
        .if_begin => {
            _ = take(c).if_begin;
            try desugarExpr(c, f);
            _ = take(c).if_then;
            emit(c, f, .if_then);
            try desugarExpr(c, f);
            _ = take(c).if_else;
            emit(c, f, .if_else);
            try desugarExpr(c, f);
            _ = take(c).if_end;
            emit(c, f, .@"if");
        },
        .while_begin => {
            _ = take(c).while_begin;
            emit(c, f, .while_begin);
            try desugarExpr(c, f);
            _ = take(c).while_body;
            emit(c, f, .while_body);
            try desugarExpr(c, f);
            _ = take(c).while_end;
            emit(c, f, .@"while");
        },
        else => {
            return fail(c, .todo);
        },
    }
}

fn desugarFun(c: *Compiler) error{DesugarError}!struct { wrapper: dir.Fun, body: dir.Fun } {
    _ = take(c).fun_begin;

    var bindings = ArrayList(dir.Binding).init(c.allocator);

    const body_fun = c.dir_fun_data.append(dir.FunData.init(c.allocator));

    const wrapper_fun = wrapper_fun: {
        var f = dir.FunData.init(c.allocator);
        f.@"inline" = true;
        const arg = f.arg_data.append(.{});
        const block_start = f.expr_data_post.count();
        try desugarPattern(c, &f, &bindings, .{ .arg = arg }, .args);
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

    _ = take(c).fun_end;

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
            c.sir_expr_next.id = next.id + 1;
        },
        .ref_to_begin => {
            _ = try desugarPathPart(c, f, true);
        },
        else => {
            const is_mut = try desugarPathPart(c, f, false);
            if (is_mut) emit(c, f, .ref_deref);
        },
    }
}

fn desugarPathPart(c: *Compiler, f: *dir.FunData, must_be_mut: bool) error{DesugarError}!bool {
    switch (peek(c)) {
        .indirect => |expr| {
            const next = c.sir_expr_next;
            c.sir_expr_next = expr;
            const is_mut = try desugarPathPart(c, f, must_be_mut);
            c.sir_expr_next.id = next.id + 1;
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
            _ = take(c).get_begin;
            const is_mut = try desugarPathPart(c, f, must_be_mut);
            try desugarKey(c, f);
            _ = take(c).get_end;
            emit(c, f, if (is_mut) .ref_get else .object_get);
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
    if (binding.is_staged)
        emit(c, f, .{ .unstage_begin = .{ .mapping = .{ .id = 0 } } });

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
    switch (peek(c)) {
        .indirect => |expr| {
            const next = c.sir_expr_next;
            c.sir_expr_next = expr;
            try desugarKey(c, f);
            c.sir_expr_next.id = next.id + 1;
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
    emit(c, f, .{ .stage = .{ .mapping = .{ .id = 0 } } });
}

fn stageString(c: *Compiler, f: *dir.FunData, string: []const u8) void {
    emit(c, f, .stage_begin);
    emit(c, f, .{ .string = string });
    emit(c, f, .{ .stage = .{ .mapping = .{ .id = 0 } } });
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
            c.sir_expr_next.id = next.id + 1;
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
            try desugarPatternInput(c, f, input);
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
        .object_begin => {
            const local = f.local_data.append(.{
                .is_tmp = true,
                .is_mutable = false,
            });

            try desugarPatternInput(c, f, input);
            const assert_object = f.expr_data_post.append(.{ .assert_object = .{ .count = 0 } });
            emit(c, f, .{ .local_let = local });

            var count: usize = 0;
            _ = take(c).object_begin;
            while (true) {
                if (takeIf(c, .object_end))
                    break;

                const key_expr = skipTree(c);
                try desugarPattern(c, f, bindings, .{ .object_get = .{ .object = local, .key = key_expr } }, context);
                count += 1;
            }

            f.expr_data_post.getPtr(assert_object).assert_object.count = count;
        },
        .ref_to_begin => {
            try desugarPath(c, f);
            try desugarPatternInput(c, f, input);
            emit(c, f, .ref_set);
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
            emit(c, f, .{ .local_get = object_get.object });
            {
                const next = c.sir_expr_next;
                c.sir_expr_next = object_get.key;
                try desugarKey(c, f);
                c.sir_expr_next = next;
            }
            emit(c, f, .object_get);
        },
        .make => |make| {
            {
                const next = c.sir_expr_next;
                c.sir_expr_next = make.head;
                try stageExpr(c, f);
                c.sir_expr_next = next;
            }
            emit(c, f, .stage_begin);
            emit(c, f, .{ .i64 = 0 });
            emit(c, f, .{ .stage = .{ .mapping = .{ .id = 0 } } });
            try desugarPatternInput(c, f, make.input.*);
            emit(c, f, .{ .struct_init = .{ .count = 1 } });
            emit(c, f, .make);
        },
    }
}

fn emit(c: *Compiler, f: *dir.FunData, expr_data: dir.ExprData) void {
    _ = c;
    _ = f.expr_data_post.append(expr_data);
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
