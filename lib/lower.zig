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
const Value = zest.Value;
const Repr = zest.Repr;
const tir = zest.tir;
const wir = zest.wir;

pub fn lower(c: *Compiler) error{LowerError}!void {
    for (0..c.tir_fun_data.count()) |tir_fun_id| {
        const tir_fun = tir.Fun{ .id = tir_fun_id };
        const wir_fun = c.wir_fun_data.append(wir.FunData.init(c.allocator, tir_fun));
        assert(tir_fun.id == wir_fun.id);
        try lowerFun(c, wir_fun);
    }
    c.wir_fun_main = .{ .id = c.tir_fun_main.?.id };
}

fn lowerFun(c: *Compiler, fun: wir.Fun) error{LowerError}!void {
    const f = c.wir_fun_data.getPtr(fun);
    const tir_f = c.tir_fun_data.get(f.tir_fun);
    _ = tir_f;
}

fn fail(c: *Compiler, fun: wir.Fun, expr: tir.Expr, data: LowerErrorData) error{LowerError} {
    c.error_data = .{ .lower = .{ .fun = fun, .expr = expr, .data = data } };
    return error.LowerError;
}

pub const LowerErrorData = union(enum) {
    todo,
};
