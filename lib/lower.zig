const std = @import("std");
const panic = std.debug.panic;
const assert = std.debug.assert;
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;

const zest = @import("./zest.zig");
const oom = zest.oom;
const deepEqual = zest.deepEqual;
const Compiler = zest.Compiler;
const SirExpr = zest.SirExpr;
const SirExprData = zest.SirExprData;
const SirObject = zest.SirObject;
const DirExpr = zest.DirExpr;
const DirExprData = zest.DirExprData;
const DirFun = zest.DirFun;
const DirFunData = zest.DirFunData;
const Local = zest.Local;
const Value = zest.Value;
const AbstractValue = zest.AbstractValue;
const Builtin = zest.Builtin;

pub fn lower(c: *Compiler) error{LowerError}!void {
    c.dir_fun_main = try lowerFun(c, .{ .keys = &.{}, .values = &.{} }, c.sir_expr_data.lastKey().?);
}

fn lowerFun(c: *Compiler, params: SirObject, body: SirExpr) error{LowerError}!DirFun {
    var f = DirFunData.init(c.allocator);
    try lowerObjectPattern(c, &f, .arg, params);
    try lowerExpr(c, &f, body);
    _ = f.expr_data.append(.@"return");
    inferExprIsStaged(c, &f);
    return c.dir_fun_data.append(f);
}

fn lowerObjectPattern(c: *Compiler, f: *DirFunData, object: AbstractValue, pattern: SirObject) error{LowerError}!void {
    for (pattern.keys, pattern.values) |key_expr, value_expr| {
        push(c, f, object);
        try lowerKey(c, f, key_expr);
        _ = f.expr_data.append(.object_get);
        const value = pop(c, f);
        try lowerPattern(c, f, value, value_expr);
    }
}

fn lowerPattern(c: *Compiler, f: *DirFunData, value: AbstractValue, pattern: SirExpr) error{LowerError}!void {
    _ = f;
    const expr_data = c.sir_expr_data.get(pattern);
    switch (expr_data) {
        .name => |name| {
            c.scope.push(.{
                .name = name,
                .value = value,
            });
        },
        //.object => |object| {
        //    TODO assert input is an object
        //    try lowerObjectPattern(c, f, input, object);
        //}
        else => return fail(c, pattern, .invalid_pattern),
    }
}

fn lowerKey(c: *Compiler, f: *DirFunData, expr: SirExpr) error{LowerError}!void {
    const expr_data = c.sir_expr_data.get(expr);
    switch (expr_data) {
        .name => |name| _ = f.expr_data.append(.{ .string = name }),
        else => try lowerExpr(c, f, expr),
    }
}

fn lowerObject(c: *Compiler, f: *DirFunData, object: SirObject) error{LowerError}!void {
    for (object.keys, object.values) |key, value| {
        try lowerKey(c, f, key);
        try lowerExpr(c, f, value);
    }
    _ = f.expr_data.append(.{ .struct_init = object.keys.len });
}

fn lowerExpr(c: *Compiler, f: *DirFunData, expr: SirExpr) error{LowerError}!void {
    const expr_data = c.sir_expr_data.get(expr);
    switch (expr_data) {
        .i32 => |i| {
            _ = f.expr_data.append(.{ .i32 = i });
        },
        .string => |string| {
            _ = f.expr_data.append(.{ .string = string });
        },
        .object => |object| {
            try lowerObject(c, f, object);
        },
        .name => |name| {
            const binding = c.scope.lookup(name) orelse
                return fail(c, expr, .name_not_in_scope);
            push(c, f, binding.value);
        },
        .let_or_set => |let_or_set| {
            try lowerExpr(c, f, let_or_set.value);
            switch (c.sir_expr_data.get(let_or_set.path)) {
                .mut => return fail(c, expr, .todo),
                .name => |name| {
                    const local = f.local_data.append(.{});
                    _ = f.expr_data.append(.{ .local_set = local });
                    _ = f.expr_data.append(.{ .struct_init = 0 });
                    c.scope.push(.{
                        .name = name,
                        .value = .{ .local = local },
                    });
                },
                else => return fail(c, let_or_set.path, .invalid_let_path),
            }
        },
        .get => |get| {
            try lowerExpr(c, f, get.object);
            try lowerKey(c, f, get.key);
            _ = f.expr_data.append(.object_get);
        },
        .fun => |fun| {
            const dir_fun = dir_fun: {
                const closure = c.scope.pushClosure();
                defer c.scope.popClosure(closure);

                break :dir_fun try lowerFun(c, fun.params, fun.body);
            };

            const dir_fun_data = c.dir_fun_data.get(dir_fun);
            for (dir_fun_data.closure_keys.items) |name| {
                _ = f.expr_data.append(.{ .string = name });
                push(c, f, c.scope.lookup(name).?.value);
            }
            _ = f.expr_data.append(.{ .struct_init = dir_fun_data.closure_keys.items.len });

            _ = f.expr_data.append(.{ .fun_init = .{ .fun = dir_fun } });
        },
        .call => |call| {
            try lowerExpr(c, f, call.head);
            try lowerObject(c, f, call.args);
            _ = f.expr_data.append(.call);
        },
        .block => |block| {
            const scope_saved = c.scope.save();
            defer c.scope.restore(scope_saved);

            const begin = f.expr_data.append(.{ .block_begin = .{ .expr_count = 0 } });
            for (block, 0..) |statement, i| {
                try lowerExpr(c, f, statement);
                if (i < block.len - 1)
                    _ = f.expr_data.append(.drop);
            }
            const expr_count = f.expr_data.lastKey().?.id - begin.id;
            f.expr_data.getPtr(begin).block_begin.expr_count = expr_count;
            _ = f.expr_data.append(.{ .block_end = .{ .expr_count = expr_count } });
        },
        else => return fail(c, expr, .todo),
    }
}

fn push(c: *Compiler, f: *DirFunData, value: AbstractValue) void {
    _ = c;
    switch (value) {
        .arg => _ = f.expr_data.append(.arg),
        .closure => |name| {
            if (!f.closure_keys_index.contains(name)) {
                f.closure_keys_index.put(name, {}) catch oom();
                f.closure_keys.append(name) catch oom();
            }
            _ = f.expr_data.append(.closure);
            _ = f.expr_data.append(.{ .string = name });
            _ = f.expr_data.append(.object_get);
        },
        .local => |local| _ = f.expr_data.append(.{ .local_get = local }),
        .builtin => panic("TODO: {}", .{value}),
    }
}

fn pop(c: *Compiler, f: *DirFunData) AbstractValue {
    _ = c;
    const local = f.local_data.append(.{});
    _ = f.expr_data.append(.{ .local_set = local });
    return .{ .local = local };
}

// TODO Could have just done this during lower itself.
fn inferExprIsStaged(c: *Compiler, f: *DirFunData) void {
    f.expr_is_staged.resize(f.expr_data.count(), false) catch oom();
    assert(c.is_staged_stack.items.len == 0);
    var expr = DirExpr{ .id = f.expr_data.count() };
    while (expr.id > 0) {
        expr.id -= 1;
        const expr_data = f.expr_data.get(expr);
        switch (expr_data) {
            // 0 input, 0 output
            .block_begin => {},
            // 0 input, 1 output
            .i32, .f32, .string, .arg, .closure, .local_get => {
                const is_staged = c.is_staged_stack.pop();
                f.expr_is_staged.setValue(expr.id, is_staged);
            },
            // 1 input, 0 output
            .local_set, .drop, .@"return" => {
                c.is_staged_stack.append(false) catch oom();
            },
            // 1 input, 1 output
            .fun_init => {
                const is_staged = c.is_staged_stack.pop();
                f.expr_is_staged.setValue(expr.id, is_staged);
                c.is_staged_stack.append(is_staged) catch oom();
            },
            .struct_init => |pair_count| {
                const is_staged = c.is_staged_stack.pop();
                f.expr_is_staged.setValue(expr.id, is_staged);
                for (0..pair_count) |_| {
                    c.is_staged_stack.append(true) catch oom();
                    c.is_staged_stack.append(is_staged) catch oom();
                }
            },
            .object_get => {
                const is_staged = c.is_staged_stack.pop();
                f.expr_is_staged.setValue(expr.id, is_staged);
                // object
                c.is_staged_stack.append(is_staged) catch oom();
                // key
                c.is_staged_stack.append(true) catch oom();
            },
            .call => {
                const is_staged = c.is_staged_stack.pop();
                f.expr_is_staged.setValue(expr.id, is_staged);
                // fun
                c.is_staged_stack.append(is_staged) catch oom();
                // args
                c.is_staged_stack.append(is_staged) catch oom();
            },
            .block_end => |block_end| {
                const is_staged = c.is_staged_stack.pop();
                f.expr_is_staged.setValue(expr.id, is_staged);
                if (is_staged) {
                    // Everything in this block is staged
                    for (expr.id - block_end.expr_count..expr.id) |id| {
                        f.expr_is_staged.setValue(id, true);
                    }
                    expr.id -= block_end.expr_count;
                } else {
                    // Otherwise look inside block.
                    c.is_staged_stack.append(is_staged) catch oom();
                }
            },
        }
    }
}

fn fail(c: *Compiler, expr: SirExpr, data: LowerErrorData) error{LowerError} {
    c.error_data = .{ .lower = .{ .expr = expr, .data = data } };
    return error.LowerError;
}

pub const LowerErrorData = union(enum) {
    invalid_pattern,
    name_not_in_scope,
    invalid_let_path,
    todo,
};
