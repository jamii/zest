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

pub fn desugar(c: *Compiler, source: sir.Source) error{DesugarError}!void {
    c.source_current = source;
    defer c.source_current = null;

    c.sir_expr_next = .{ .id = 0 };
    c.scope = .init(c.allocator);

    const s = c.sir_source_data.get(source);

    switch (s.origin) {
        .runtime => {
            // Gonna throw away this function.
            var f = dir.FunData.init(c.allocator);
            const namespace = try desugarNamespace(c, s, &f);
            c.namespace_by_origin.put(s.origin, namespace) catch oom();
        },
        .main => {
            var f = dir.FunData.init(c.allocator);
            try desugarExpr(c, s, &f);
            emit(c, &f, .assert_has_no_ref);
            emit(c, &f, .@"return");
            convertPostorderToPreorder(c, dir.Expr, dir.ExprData, f.expr_data_post, &f.expr_data_pre);
            assert(c.dir_fun_main == null);
            c.dir_fun_main = c.dir_fun_data.append(f);
        },
        else => panic("TODO", .{}),
    }
}

fn desugarExpr(c: *Compiler, s: sir.SourceData, f: *dir.FunData) error{DesugarError}!void {
    switch (take(c, s)) {
        .name, .get, .ref_to => {
            untake(c);
            try desugarPath(c, s, f);
        },
        .i64 => |i| {
            emit(c, f, .{ .i64 = i });
        },
        .string => |string| {
            emit(c, f, .{ .string = string });
        },
        .object => |object| {
            for (0..object.count) |_|
                try desugarKeyValue(c, s, f);
            emit(c, f, .{ .struct_init = .{ .count = object.count } });
        },
        .let => {
            const pattern = skipTree(c, s);
            try desugarExpr(c, s, f);
            var bindings = ArrayList(dir.Binding).init(c.allocator);
            {
                const next = c.sir_expr_next;
                c.sir_expr_next = pattern;
                defer c.sir_expr_next = next;

                try desugarPattern(c, s, f, &bindings, .let);
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

                break :dir_fun try desugarFun(c, s);
            };
            const body_fun_data = c.dir_fun_data.get(dir_funs.body);
            for (body_fun_data.closure_keys.items) |name| {
                stageString(c, s, f, name);
                const binding = c.scope.lookup(name).?;
                desugarBinding(c, s, f, binding);
            }
            emit(c, f, .{ .struct_init = .{ .count = body_fun_data.closure_keys.items.len } });
            emit(c, f, .{ .fun_init = .{ .fun = dir_funs.wrapper } });
        },
        .call => {
            try desugarExpr(c, s, f); // fun
            try desugarExpr(c, s, f); // arg struct
            emit(c, f, .{ .call = .{ .arg_count = 1 } });
        },
        .call_slash => {
            const arg0 = skipTree(c, s);
            try desugarExpr(c, s, f); // fun;
            const object = take(c, s).object; // arg struct
            emit(c, f, .stage_begin);
            emit(c, f, .{ .i64 = 0 });
            emit(c, f, .{ .stage = .{} });
            {
                const next = c.sir_expr_next;
                c.sir_expr_next = arg0;
                defer c.sir_expr_next = next;

                try desugarExpr(c, s, f); // arg0
            }
            for (0..object.count) |_|
                try desugarKeyValue(c, s, f);
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
                try desugarExpr(c, s, f);
                if (is_staged)
                    emit(c, f, .{ .stage = .{} });
            }
            emit(c, f, .{ .call_builtin = builtin });
        },
        .repr_of => {
            emit(c, f, .{ .repr_of_begin = .{} });
            try desugarExpr(c, s, f);
            emit(c, f, .repr_of);
        },
        .make => {
            try stageExpr(c, s, f);
            try desugarExpr(c, s, f);
            emit(c, f, .make);
        },
        .make_slash => {
            const arg = skipTree(c, s);
            try stageExpr(c, s, f);
            emit(c, f, .stage_begin);
            emit(c, f, .{ .i64 = 0 });
            emit(c, f, .{ .stage = .{} });
            {
                const next = c.sir_expr_next;
                c.sir_expr_next = arg;
                defer c.sir_expr_next = next;

                try desugarExpr(c, s, f); // arg
            }
            emit(c, f, .{ .struct_init = .{ .count = 1 } });
            emit(c, f, .make);
        },
        .block => |block| {
            const scope_saved = c.scope.save();
            defer c.scope.restore(scope_saved);

            const block_start = f.expr_data_post.count();
            for (0..block.count) |_| {
                try desugarExpr(c, s, f);
            }
            emit(c, f, .{ .block = .{ .count = countTreesSince(c, f, .{ .id = block_start }) } });
        },
        .@"if" => {
            try desugarExpr(c, s, f);
            emit(c, f, .if_then);
            try desugarExpr(c, s, f);
            emit(c, f, .if_else);
            try desugarExpr(c, s, f);
            emit(c, f, .@"if");
        },
        .@"while" => {
            emit(c, f, .while_begin);
            const begin = f.expr_data_post.lastKey().?;
            try desugarExpr(c, s, f);
            emit(c, f, .while_body);
            try desugarExpr(c, s, f);
            emit(c, f, .{ .@"while" = .{ .begin = begin } });
        },
        .namespace => {
            _ = try desugarNamespace(c, s, f);
        },
        .namespace_get => {
            try desugarExpr(c, s, f); // namespace
            try desugarKey(c, s, f); // key
            emit(c, f, .namespace_get);
        },
        else => {
            return fail(c, .todo);
        },
    }
}

fn desugarFun(c: *Compiler, s: sir.SourceData) error{DesugarError}!struct { wrapper: dir.Fun, body: dir.Fun } {
    _ = take(c, s).fun;

    var bindings = ArrayList(dir.Binding).init(c.allocator);

    const body_fun = c.dir_fun_data.append(dir.FunData.init(c.allocator));

    const wrapper_fun = wrapper_fun: {
        var f = dir.FunData.init(c.allocator);
        f.@"inline" = true;
        const block_start = f.expr_data_post.count();
        const arg = f.arg_data.append(.{});
        emit(c, &f, .{ .arg = arg });
        try desugarPattern(c, s, &f, &bindings, .args);
        emit(c, &f, .closure);
        emit(c, &f, .{ .fun_init = .{ .fun = body_fun } });
        for (bindings.items) |binding| {
            desugarBinding(c, s, &f, .{
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

        var f = c.dir_fun_data.get(body_fun);
        for (bindings.items) |binding| {
            const arg = f.arg_data.append(.{});
            c.scope.push(.{
                .name = binding.name,
                .value = .{ .arg = arg },
                .mut = binding.mut,
            });
        }
        try desugarExpr(c, s, &f);
        emit(c, &f, .assert_has_no_ref);
        emit(c, &f, .@"return");
        convertPostorderToPreorder(c, dir.Expr, dir.ExprData, f.expr_data_post, &f.expr_data_pre);
        c.dir_fun_data.getPtr(body_fun).* = f;
    }

    return .{
        .wrapper = wrapper_fun,
        .body = body_fun,
    };
}

fn desugarNamespace(c: *Compiler, s: sir.SourceData, f: *dir.FunData) error{DesugarError}!dir.Namespace {
    const namespace = c.namespace_data.append(.init(c.allocator));

    const definition_count = take(c, s).block.count;

    // TODO Allow namespaces to close over outside scope.
    const scope_outside = c.scope;
    c.scope = .init(c.allocator);
    defer c.scope = scope_outside;

    {
        const start = c.sir_expr_next;
        defer c.sir_expr_next = start;

        for (0..definition_count) |_| {
            if (take(c, s) != .let) {
                return fail(c, .statement_in_namespace);
            }
            const name = take(c, s);
            if (name != .name) {
                return fail(c, .pattern_in_namespace);
            }
            if (name.name.mut) {
                return fail(c, .mut_in_namespace);
            }
            c.scope.push(.{
                .name = name.name.name,
                .value = .{ .definition = .{
                    .namespace = namespace,
                    .name = name.name.name,
                } },
                .mut = false,
            });
            _ = skipTree(c, s); // value
        }
    }

    const namespace_data = c.namespace_data.getPtr(namespace);
    for (0..definition_count) |_| {
        _ = take(c, s).let;
        const name = take(c, s).name;

        var def_f = dir.FunData.init(c.allocator);
        try desugarExpr(c, s, &def_f);
        emit(c, &def_f, .@"return");
        convertPostorderToPreorder(c, dir.Expr, dir.ExprData, def_f.expr_data_post, &def_f.expr_data_pre);
        const fun = c.dir_fun_data.append(def_f);

        const definition = namespace_data.definition_data.append(.{
            .fun = fun,
            .value = .unevaluated,
        });
        namespace_data.definition_by_name.put(name.name, definition) catch oom();
    }

    emit(c, f, .{ .namespace = namespace });

    return namespace;
}

fn desugarPath(c: *Compiler, s: sir.SourceData, f: *dir.FunData) error{DesugarError}!void {
    switch (take(c, s)) {
        .ref_to => {
            _ = try desugarPathPart(c, s, f, true);
        },
        else => {
            untake(c);
            const is_mut = try desugarPathPart(c, s, f, false);
            if (is_mut) emit(c, f, .ref_deref);
        },
    }
}

fn desugarPathPart(c: *Compiler, s: sir.SourceData, f: *dir.FunData, must_be_mut: bool) error{DesugarError}!bool {
    switch (take(c, s)) {
        .name => |name| {
            if (name.mut)
                return fail(c, .meaningless_mut);
            const binding = c.scope.lookup(name.name) orelse
                return fail(c, .{ .name_not_bound = .{ .name = name.name } });
            if (must_be_mut and !binding.mut)
                return fail(c, .{ .may_not_mutate_immutable_binding = .{ .name = name.name } });
            desugarBinding(c, s, f, binding);
            return binding.mut;
        },
        .get => {
            const is_mut = try desugarPathPart(c, s, f, must_be_mut);
            try desugarKey(c, s, f);
            emit(c, f, if (is_mut) .ref_get else .object_get);
            return is_mut;
        },
        .ref_to => {
            const is_mut = try desugarPathPart(c, s, f, true);
            return is_mut;
        },
        else => {
            untake(c);
            if (must_be_mut)
                return fail(c, .invalid_path);
            try desugarExpr(c, s, f);
            return false;
        },
    }
}

fn desugarBinding(c: *Compiler, s: sir.SourceData, f: *dir.FunData, binding: dir.BindingInfo) void {
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
            stageString(c, s, f, name);
            emit(c, f, .object_get);
        },
        .local => |local| {
            emit(c, f, .{ .local_get = local });
        },
        .constant => |constant| {
            emit(c, f, constant);
        },
        .definition => |definition| {
            emit(c, f, .{ .namespace = definition.namespace });
            emit(c, f, .stage_begin);
            emit(c, f, .{ .string = definition.name });
            emit(c, f, .{ .stage = .{} });
            emit(c, f, .namespace_get);
        },
    }

    if (binding.is_staged)
        emit(c, f, .unstage);
}

fn desugarKey(c: *Compiler, s: sir.SourceData, f: *dir.FunData) error{DesugarError}!void {
    switch (take(c, s)) {
        .name => |name| {
            if (name.mut)
                return fail(c, .meaningless_mut);
            stageString(c, s, f, name.name);
        },
        else => {
            untake(c);
            try stageExpr(c, s, f);
        },
    }
}

fn desugarKeyValue(c: *Compiler, s: sir.SourceData, f: *dir.FunData) error{DesugarError}!void {
    switch (take(c, s)) {
        .key_value => {
            try desugarKey(c, s, f);
            try desugarExpr(c, s, f);
        },
        .pos_value => |pos| {
            emit(c, f, .stage_begin);
            emit(c, f, .{ .i64 = pos });
            emit(c, f, .{ .stage = .{} });
            try desugarExpr(c, s, f);
        },
        else => |other| panic("Invalid key_value: {}", .{other}),
    }
}

fn stageExpr(c: *Compiler, s: sir.SourceData, f: *dir.FunData) error{DesugarError}!void {
    emit(c, f, .stage_begin);
    const already_staged = c.scope.staged_until_len != null;
    if (!already_staged) {
        c.scope.staged_until_len = c.scope.bindings.items.len;
    }
    try desugarExpr(c, s, f);
    if (!already_staged) {
        c.scope.staged_until_len = null;
    }
    // TODO Do we need to emit .stage when already_staged?
    emit(c, f, .{ .stage = .{} });
}

fn stageString(c: *Compiler, s: sir.SourceData, f: *dir.FunData, string: []const u8) void {
    _ = s;
    emit(c, f, .stage_begin);
    emit(c, f, .{ .string = string });
    emit(c, f, .{ .stage = .{} });
}

const PatternContext = enum { args, let };

fn desugarPattern(c: *Compiler, s: sir.SourceData, f: *dir.FunData, bindings: *ArrayList(dir.Binding), context: PatternContext) error{DesugarError}!void {
    switch (take(c, s)) {
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
                switch (take(c, s)) {
                    .key_value => {
                        try desugarKey(c, s, f);
                        emit(c, f, .object_get);
                        try desugarPattern(c, s, f, bindings, context);
                    },
                    .pos_value => |pos| {
                        emit(c, f, .stage_begin);
                        emit(c, f, .{ .i64 = pos });
                        emit(c, f, .{ .stage = .{} });
                        emit(c, f, .object_get);
                        try desugarPattern(c, s, f, bindings, context);
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
            try desugarPath(c, s, f);
            emit(c, f, .{ .local_get = local });
            emit(c, f, .ref_set);
        },
        .make_slash => {
            const arg = skipTree(c, s);

            const local = f.local_data.append(.{
                .is_tmp = true,
                .is_mutable = false,
            });
            emit(c, f, .{ .local_let = local });
            try stageExpr(c, s, f);
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

                try desugarPattern(c, s, f, bindings, context);
            }
        },
        else => return fail(c, .invalid_pattern),
    }
}

fn emit(c: *Compiler, f: *dir.FunData, expr_data: dir.ExprData) void {
    _ = c;
    _ = f.expr_data_post.append(expr_data);
}

fn take(c: *Compiler, s: sir.SourceData) sir.ExprData {
    const expr_data = s.expr_data_pre.get(c.sir_expr_next);
    c.sir_expr_next.id += 1;
    return expr_data;
}

fn untake(c: *Compiler) void {
    c.sir_expr_next.id -= 1;
}

fn skipTree(c: *Compiler, s: sir.SourceData) sir.Expr {
    const start = c.sir_expr_next;
    var children_remaining: usize = 1;
    while (children_remaining > 0) {
        children_remaining += take(c, s).childCount(c);
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
        .source = c.source_current.?,
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
    statement_in_namespace,
    pattern_in_namespace,
    mut_in_namespace,
    todo,
};
