const std = @import("std");
const panic = std.debug.panic;
const assert = std.debug.assert;
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;

const zest = @import("./zest.zig");
const oom = zest.oom;
const deepEqual = zest.deepEqual;
const List = zest.List;
const Compiler = zest.Compiler;
const DirExpr = zest.DirExpr;
const DirExprData = zest.DirExprData;
const DirFun = zest.DirFun;
const DirFunData = zest.DirFunData;
const Value = zest.Value;

pub fn eval(c: *Compiler) error{EvalError}!Value {
    return evalFun(c, c.dir_fun_main.?, Value.emptyStruct());
}

fn evalFun(c: *Compiler, fun: DirFun, args: Value) error{EvalError}!Value {
    const f = c.dir_fun_data.get(fun);
    var frame = List(DirExpr, Value).init(c.allocator);
    frame.appendNTimes(Value.emptyStruct(), f.expr_data.count());

    assert(f.expr_data.get(.{ .id = 0 }) == .arg);
    frame.getPtr(.{ .id = 0 }).* = args;

    var expr = DirExpr{ .id = 1 };
    while (true) {
        const expr_data = f.expr_data.get(expr);
        switch (expr_data) {
            .@"return" => |return_expr| return frame.get(return_expr),
            else => {
                const value = try evalExpr(c, f, &frame, expr);
                frame.getPtr(expr).* = value;
                expr.id += 1;
            },
        }
    }
    return evalExpr(c, f, &frame, .{ .id = 1 });
}

fn evalExpr(c: *Compiler, f: DirFunData, frame: *List(DirExpr, Value), expr: DirExpr) error{EvalError}!Value {
    const expr_data = f.expr_data.get(expr);
    switch (expr_data) {
        .i32 => |i| return .{ .i32 = i },
        .get => |get| {
            const object = frame.get(get.object);
            const key = frame.get(get.key);
            return object.get(key) orelse
                fail(c, expr, .{ .get_missing = .{ .object = object, .key = key } });
        },
        .@"return" => panic("Can't eval control flow expr: {}", .{expr_data}),
        else => return fail(c, expr, .todo),
    }
}

fn fail(c: *Compiler, expr: DirExpr, data: EvalErrorData) error{EvalError} {
    c.error_data = .{ .eval = .{ .expr = expr, .data = data } };
    return error.EvalError;
}

pub const EvalErrorData = union(enum) {
    get_missing: struct {
        object: Value,
        key: Value,
    },
    todo,
};
