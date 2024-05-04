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
        const repr = local_data.repr.one;
        if (storeOnShadow(repr)) {
            _ = f.local_from_tir.append(null);
        } else {
            const local = f.local_data.append(.{
                .type = try lowerRepr(c, local_data.repr.one),
            });
            _ = f.local_from_tir.append(local);
        }
    }

    assert(c.wir_address_stack.items.len == 0);

    for (tir_f.expr_data.items(), tir_f.expr_repr.items(), 0..) |expr_data, repr, expr_id| {
        switch (expr_data) {
            .i32 => |i| {
                _ = f.expr_data.append(.{ .i32 = i });
                c.wir_address_stack.append(null) catch oom();
            },
            .struct_init => {
                if (!repr.?.isEmptyStruct())
                    return fail(c, .todo);
                // TODO Replace with constant address
                _ = f.expr_data.append(.{ .i32 = 0 });
                c.wir_address_stack.append(null) catch oom();
            },
            .local_get => |local| {
                const local_repr = tir_f.local_data.get(local).repr.one;
                if (storeOnShadow(local_repr)) {
                    return fail(c, .todo);
                } else {
                    _ = f.expr_data.append(.{ .local_get = f.local_from_tir.get(local).? });
                    c.wir_address_stack.append(null) catch oom();
                }
            },
            .local_let => |local| {
                const local_repr = tir_f.local_data.get(local).repr.one;
                if (storeOnShadow(local_repr)) {
                    return fail(c, .todo);
                } else {
                    assert(c.wir_address_stack.pop() == null);
                    _ = f.expr_data.append(.{ .local_set = f.local_from_tir.get(local).? });
                }
            },
            .drop => {
                _ = c.wir_address_stack.pop();
                _ = f.expr_data.append(.drop);
            },
            .block_begin => {
                f.block_stack.append(.{
                    .block_begin = .{ .id = expr_id },
                    .shadow_offset_next = f.shadow_offset_next,
                    .shadow_address_index = f.shadow_address_stack.items.len,
                }) catch oom();
            },
            .block_end => |block_end| {
                const block = f.block_stack.pop();
                assert(expr_id - block_end.expr_count == block.block_begin.id);
                f.shadow_offset_next = block.shadow_offset_next;
                f.shadow_address_stack.shrinkRetainingCapacity(block.shadow_address_index);
            },
            .@"return" => {
                assert(c.wir_address_stack.pop() == null);
                _ = f.expr_data.append(.@"return");
            },
            else => {
                //std.debug.print("{}\n", .{expr_data});
                return fail(c, .todo);
            },
        }
    }

    assert(c.wir_address_stack.items.len == 0);
}

fn lowerRepr(c: *Compiler, repr: Repr) error{LowerError}!wasm.Valtype {
    return switch (repr) {
        .i32 => .i32,
        else => fail(c, .todo),
    };
}

fn shadowPush(c: *Compiler, f: *wir.FunData, shadow: tir.Shadow, repr: Repr) usize {
    _ = c;
    for (f.shadow_address_stack) |shadow_address| {
        if (shadow_address.shadow == shadow)
            return shadow_address.offset;
    }
    const offset = f.shadow_offset_next;
    f.shadow_address_stack.append(.{ .shadow = shadow, .offset = offset });
    f.shadow_offset_next += repr.sizeOf();
    f.shadow_offset_max = @max(f.shadow_offset_max, f.shadow_offset_nex);
    return offset;
}

fn storeOnShadow(repr: Repr) bool {
    return switch (repr) {
        .i32 => false,
        .string, .@"struct", .@"union", .fun, .only, .repr => true,
    };
}

fn fail(c: *Compiler, data: LowerErrorData) error{LowerError} {
    c.error_data = .{ .lower = .{ .data = data } };
    return error.LowerError;
}

pub const LowerErrorData = union(enum) {
    todo,
};
