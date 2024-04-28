const std = @import("std");
const panic = std.debug.panic;
const assert = std.debug.assert;
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const wasm = std.wasm;

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
        try lowerFun(c, .{ .id = tir_fun_id });
    }
    c.wir_fun_main = .{ .id = c.tir_fun_main.?.id };
}

fn lowerFun(c: *Compiler, tir_fun: tir.Fun) error{LowerError}!void {
    const tir_f = c.tir_fun_data.get(tir_fun);

    var arg_types = ArrayList(wasm.Valtype).init(c.allocator);
    var return_types = ArrayList(wasm.Valtype).init(c.allocator);
    if (!tir_f.key.closure_repr.isEmptyStruct() or
        !tir_f.key.arg_repr.isEmptyStruct())
        return fail(c, .todo);
    return_types.append(try lowerRepr(c, tir_f.return_repr.one)) catch oom();
    const fun_type_data = wir.FunTypeData{
        .arg_types = arg_types.toOwnedSlice() catch oom(),
        .return_types = return_types.toOwnedSlice() catch oom(),
    };
    var fun_type: ?wir.FunType = c.fun_type_memo.get(fun_type_data);
    if (fun_type == null) {
        fun_type = c.fun_type_data.append(fun_type_data);
        c.fun_type_memo.put(fun_type_data, fun_type.?) catch oom();
    }

    const fun = c.wir_fun_data.append(wir.FunData.init(c.allocator, tir_fun, fun_type.?));
    const f = c.wir_fun_data.getPtr(fun);
    assert(tir_fun.id == fun.id);

    for (tir_f.local_data.items()) |local_data| {
        _ = f.local_data.append(.{
            .type = try lowerRepr(c, local_data.repr.one),
        });
    }

    for (tir_f.expr_data.items(), tir_f.expr_repr.items()) |expr_data, repr| {
        switch (expr_data) {
            .i32 => |i| {
                _ = f.expr_data.append(.{ .i32 = i });
            },
            .struct_init => {
                if (!repr.?.isEmptyStruct())
                    return fail(c, .todo);
                // TODO How do we get rid of these zero-size values?
                _ = f.expr_data.append(.{ .i32 = 0 });
            },
            .local_get => |local| {
                _ = f.expr_data.append(.{ .local_get = .{ .id = local.id } });
            },
            .local_let => |local| {
                _ = f.expr_data.append(.{ .local_set = .{ .id = local.id } });
            },
            .drop => {
                _ = f.expr_data.append(.drop);
            },
            .block_begin, .block_end => {
                // TODO Can ignore these for now, until we add break.
            },
            .@"return" => {
                _ = f.expr_data.append(.@"return");
            },
            else => {
                std.debug.print("{}\n", .{expr_data});
                return fail(c, .todo);
            },
        }
    }
}

fn lowerRepr(c: *Compiler, repr: Repr) error{LowerError}!wasm.Valtype {
    return switch (repr) {
        .i32 => .i32,
        else => fail(c, .todo),
    };
}

fn fail(c: *Compiler, data: LowerErrorData) error{LowerError} {
    c.error_data = .{ .lower = .{ .data = data } };
    return error.LowerError;
}

pub const LowerErrorData = union(enum) {
    todo,
};
