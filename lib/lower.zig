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
    return_types.append(lowerRepr(tir_f.return_repr.one).abi()) catch oom();
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
        switch (lowerRepr(repr)) {
            .primitive => |typ| {
                const local = f.local_data.append(.{ .type = typ });
                _ = f.local_from_tir.append(local);
            },
            .shadow => {
                _ = f.local_from_tir.append(null);
            },
        }
    }

    assert(c.wir_address_stack.items.len == 0);

    //std.debug.print("---\n", .{});
    for (tir_f.expr_data.items(), tir_f.expr_repr.items(), tir_f.expr_address.items(), 0..) |expr_data, repr, tir_address, expr_id| {
        //std.debug.print("{} {}\n", .{ expr_data, c.wir_address_stack.items.len });
        switch (expr_data) {
            .i32 => |i| {
                _ = f.expr_data.append(.{ .i32 = i });
                c.wir_address_stack.append(null) catch oom();
            },
            .struct_init => {
                const address = shadowPush(c, f, tir_address.?, repr.?);
                const value_reprs = repr.?.@"struct".reprs;
                var i: usize = value_reprs.len;
                while (i > 0) : (i -= 1) {
                    const value_repr = value_reprs[i - 1];
                    const value_offset = repr.?.@"struct".offsetOf(i - 1);
                    store(c, f, .{
                        .base = address.base,
                        .offset = @intCast(address.offset + value_offset),
                        .repr = value_repr,
                    });
                }
                c.wir_address_stack.append(address) catch oom();
            },
            .local_get => |tir_local| {
                const local_repr = tir_f.local_data.get(tir_local).repr.one;
                switch (lowerRepr(local_repr)) {
                    .primitive => {
                        _ = f.expr_data.append(.{ .local_get = f.local_from_tir.get(tir_local).? });
                        c.wir_address_stack.append(null) catch oom();
                    },
                    .shadow => {
                        return fail(c, .todo);
                    },
                }
            },
            .local_let => |tir_local| {
                const local_repr = tir_f.local_data.get(tir_local).repr.one;
                switch (lowerRepr(local_repr)) {
                    .primitive => {
                        assert(c.wir_address_stack.pop() == null);
                        _ = f.expr_data.append(.{ .local_set = f.local_from_tir.get(tir_local).? });
                    },
                    .shadow => {
                        return fail(c, .todo);
                    },
                }
            },
            .object_get => |object_get| {
                const object_address = c.wir_address_stack.pop().?;
                const object_repr = object_address.repr.@"struct"; // TODO other object reprs
                const i = object_repr.get(object_get.key).?;
                const offset = object_repr.offsetOf(i);
                c.wir_address_stack.append(.{
                    .base = object_address.base,
                    .offset = object_address.offset + @as(u32, @intCast(offset)),
                    .repr = object_repr.reprs[i],
                }) catch oom();
            },
            .drop => {
                const dropped_address = c.wir_address_stack.pop();
                if (dropped_address == null) {
                    _ = f.expr_data.append(.drop);
                }
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
                if (c.wir_address_stack.pop()) |from_address| {
                    copy(c, f, from_address, .{ .base = .@"return", .offset = 0, .repr = tir_f.return_repr.one });
                }
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

const WirRepr = union(enum) {
    primitive: wasm.Valtype,
    shadow,

    fn abi(self: WirRepr) wasm.Valtype {
        return switch (self) {
            .primitive => |primitive| primitive,
            .shadow => .i32, // pointer to shadow
        };
    }
};

fn lowerRepr(repr: Repr) WirRepr {
    return switch (repr) {
        .i32 => .{ .primitive = .i32 },
        .string, .@"struct", .@"union", .fun, .only, .repr => .shadow,
    };
}

fn store(c: *Compiler, f: *wir.FunData, to_address: wir.Address) void {
    const from_address = c.wir_address_stack.pop();
    switch (lowerRepr(to_address.repr)) {
        .primitive => {
            assert(from_address == null);
            _ = f.expr_data.append(.{ .store = .{ .address = to_address } });
        },
        .shadow => {
            assert(from_address != null);
            copy(c, f, from_address.?, to_address);
        },
    }
}

fn copy(c: *Compiler, f: *wir.FunData, from_address: wir.Address, to_address: wir.Address) void {
    _ = c;
    assert(from_address.repr.equal(to_address.repr));
    if (deepEqual(from_address, to_address)) return;
    _ = f.expr_data.append(.{ .copy = .{
        .from_address = from_address,
        .to_address = to_address,
    } });
}

fn shadowPush(c: *Compiler, f: *wir.FunData, tir_address: tir.Address, repr: Repr) wir.Address {
    _ = c;
    for (f.shadow_address_stack.items) |shadow_address| {
        if (deepEqual(shadow_address.tir_address, tir_address))
            return .{
                .base = .shadow,
                .offset = @intCast(shadow_address.offset + tir_address.offset),
                .repr = repr,
            };
    }
    const offset = f.shadow_offset_next;
    f.shadow_address_stack.append(.{ .tir_address = tir_address, .offset = @intCast(offset) }) catch oom();
    f.shadow_offset_next += repr.sizeOf();
    f.shadow_offset_max = @max(f.shadow_offset_max, f.shadow_offset_next);
    return .{
        .base = .shadow,
        .offset = @intCast(offset + tir_address.offset),
        .repr = repr,
    };
}

fn fail(c: *Compiler, data: LowerErrorData) error{LowerError} {
    c.error_data = .{ .lower = .{ .data = data } };
    return error.LowerError;
}

pub const LowerErrorData = union(enum) {
    todo,
};
