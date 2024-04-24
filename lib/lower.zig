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

            for (block, 0..) |statement, i| {
                try lowerExpr(c, f, statement);
                if (i < block.len - 1)
                    _ = f.expr_data.append(.drop);
            }
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
