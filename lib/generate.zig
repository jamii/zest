const std = @import("std");
const panic = std.debug.panic;
const assert = std.debug.assert;
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const wasm = std.wasm;

const zest = @import("./zest.zig");
const oom = zest.oom;
const deepEqual = zest.deepEqual;
const Compiler = zest.Compiler;
const Repr = zest.Repr;
const tir = zest.tir;
const wir = zest.wir;

const stack_global = 0;
const stack_top = 1 * wasm.page_size;

pub fn generate(c: *Compiler) error{GenerateError}!void {
    emitBytes(c, &wasm.magic);
    emitBytes(c, &wasm.version);

    {
        const section = emitSectionStart(c, wasm.Section.type);
        defer emitSectionEnd(c, section);

        emitLebU32(c, @intCast(c.fun_type_data.count()));
        for (c.fun_type_data.items()) |fun_type_data| {
            emitByte(c, wasm.function_type);

            emitLebU32(c, @intCast(fun_type_data.arg_types.len));
            for (fun_type_data.arg_types) |arg_type| {
                emitEnum(c, arg_type);
            }

            emitLebU32(c, @intCast(fun_type_data.return_types.len));
            for (fun_type_data.return_types) |return_type| {
                emitEnum(c, return_type);
            }
        }
    }

    {
        const section = emitSectionStart(c, wasm.Section.function);
        defer emitSectionEnd(c, section);

        emitLebU32(c, @intCast(c.wir_fun_data.count()));
        for (c.wir_fun_data.items()) |f| {
            emitLebU32(c, @intCast(f.fun_type.id));
        }
    }

    {
        const section = emitSectionStart(c, wasm.Section.memory);
        defer emitSectionEnd(c, section);

        // Number of memories.
        emitLebU32(c, 1);
        // No maximum.
        emitByte(c, 0x00);
        // At minimum enough space for stack.
        emitLebU32(c, @divExact(stack_top, wasm.page_size));
    }

    {
        const section = emitSectionStart(c, wasm.Section.global);
        defer emitSectionEnd(c, section);

        // Number of globals.
        emitLebU32(c, 1);

        emitEnum(c, wasm.Valtype.i32);
        emitByte(c, 0x01); // mutable
        emitEnum(c, wasm.Opcode.i32_const);
        emitLebU32(c, stack_top);
        emitEnum(c, wasm.Opcode.end);
    }

    {
        const section = emitSectionStart(c, wasm.Section.@"export");
        defer emitSectionEnd(c, section);

        // Number of exports.
        emitLebU32(c, 2);

        emitName(c, "main");
        emitEnum(c, wasm.ExternalKind.function);
        emitLebU32(c, @intCast(c.wir_fun_main.?.id));

        emitName(c, "memory");
        emitEnum(c, wasm.ExternalKind.memory);
        emitLebU32(c, 0);
    }

    {
        const section = emitSectionStart(c, wasm.Section.code);
        defer emitSectionEnd(c, section);

        emitLebU32(c, @intCast(c.wir_fun_data.count()));
        for (c.wir_fun_data.items()) |f| {
            const start = emitByteCountLater(c);
            defer emitByteCount(c, start);

            var locals_count: u32 = 0;

            const arg_start = locals_count;
            locals_count += @intCast(c.fun_type_data.get(f.fun_type).arg_types.len);

            const local_start = locals_count;
            locals_count += @intCast(f.local_data.count());

            const frame_ptr = locals_count;
            locals_count += 1;

            const local_map = LocalMap{
                .arg_start = arg_start,
                .local_start = local_start,
                .frame_ptr = frame_ptr,
            };

            // Locals
            emitLebU32(c, @intCast(locals_count));
            for (c.fun_type_data.get(f.fun_type).arg_types) |typ| {
                emitLebU32(c, 1);
                emitEnum(c, typ);
            }
            for (f.local_data.items()) |l| {
                emitLebU32(c, 1);
                emitEnum(c, l.type);
            }
            emitLebU32(c, 1);
            emitEnum(c, wasm.Valtype.i32);

            //// Frame push
            //if (shadow_size != 0) {
            //    emitEnum(c, wasm.Opcode.global_get);
            //    emitLebU32(c, stack_global);
            //    emitEnum(c, wasm.Opcode.i32_const);
            //    emitLebU32(c, @intCast(shadow_size));
            //    emitEnum(c, wasm.Opcode.i32_sub);
            //    emitEnum(c, wasm.Opcode.global_set);
            //    emitLebU32(c, stack_global);
            //}

            for (f.expr_data.items()) |expr_data| {
                emitExpr(c, f, local_map, expr_data);
            }

            //// Frame pop
            //if (shadow_size != 0) {
            //    emitEnum(c, wasm.Opcode.global_get);
            //    emitLebU32(c, stack_global);
            //    emitEnum(c, wasm.Opcode.i32_const);
            //    emitLebU32(c, @intCast(shadow_size));
            //    emitEnum(c, wasm.Opcode.i32_add);
            //    emitEnum(c, wasm.Opcode.global_set);
            //    emitLebU32(c, stack_global);
            //}

            emitEnum(c, wasm.Opcode.@"return");
            emitEnum(c, wasm.Opcode.end);
        }
    }
}

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

pub const GenerateErrorData = union(enum) {
    todo,
};

fn fail(c: *Compiler, data: GenerateErrorData) error{GenerateError} {
    c.error_data = .{ .generate = .{ .data = data } };
    return error.GenerateError;
}

const LocalMap = struct {
    arg_start: u32,
    local_start: u32,
    frame_ptr: u32,
};

fn emitExpr(c: *Compiler, f: wir.FunData, local_map: LocalMap, expr_data: wir.ExprData) void {
    _ = f;
    switch (expr_data) {
        .i32 => |i| {
            emitEnum(c, wasm.Opcode.i32_const);
            emitLebI32(c, i);
        },
        .local_get => |local| {
            emitEnum(c, wasm.Opcode.local_get);
            emitLebU32(c, local_map.local_start + @as(u32, @intCast(local.id)));
        },
        .local_set => |local| {
            emitEnum(c, wasm.Opcode.local_set);
            emitLebU32(c, local_map.local_start + @as(u32, @intCast(local.id)));
        },
        //.store => |store| {
        //    // TODO need base on stack before value :(
        //    emitEnum(c, switch (store.address.repr) {
        //        .i32 => wasm.Opcode.i32_store,
        //        else => panic("No store opcode exists for repr: {}", .{store.address.repr}),
        //    });
        //    emitLebU32(c, 0); // alignment
        //    emitLebU32(c, store.address.offset);
        //},
        .drop => {
            emitEnum(c, wasm.Opcode.drop);
        },
        .@"return" => {
            // Do nothing here - leave the value on the stack and it will be returned after frame pop.
            // TODO Once we have non-trivial control flow we'll need to break to the outermost block.
        },
        else => panic("TODO generate: {}", .{expr_data}),
    }
}

//fn emitNode(c: *Compiler, s: SpecializationData, node: Node) void {
//    const node_data = s.node_data.get(node);
//    switch (node_data) {
//        .value => |value| {
//            switch (value) {
//                .i32 => |i| {
//                    emitEnum(c, wasm.Opcode.i32_const);
//                    emitLebI32(c, i);
//                },
//                .string, .@"struct", .@"union", .repr => panic("Unexpected {}", .{node_data}),
//            }
//        },
//        .struct_init => panic("Unexpected {}", .{node_data}),
//        .@"return" => {
//            // Do nothing here - leave the value on the stack and it will be returned after frame pop.
//        },
//        .call => |call| {
//            emitEnum(c, wasm.Opcode.call);
//            emitLebU32(c, @intCast(call.specialization.?.id));
//        },
//        .get => panic("Unexpected {}", .{node_data}),
//        .arg_get => |arg_get| {
//            emitEnum(c, wasm.Opcode.local_get);
//            emitLebU32(c, @intCast(arg_get.arg.id));
//        },
//        .local_get => |local_get| {
//            emitEnum(c, wasm.Opcode.local_get);
//            emitLebU32(c, @intCast(s.in_repr.count() + local_get.local.id));
//        },
//        .local_set => |local_set| {
//            emitEnum(c, wasm.Opcode.local_set);
//            emitLebU32(c, @intCast(s.in_repr.count() + local_set.local.id));
//        },
//        .shadow_ptr => |shadow| {
//            var offset: usize = 0;
//            for (s.shadow_repr.items()[0..shadow.id]) |repr_before| {
//                offset += repr_before.sizeOf();
//            }
//            emitEnum(c, wasm.Opcode.global_get);
//            emitLebU32(c, stack_global);
//            emitEnum(c, wasm.Opcode.i32_const);
//            emitLebU32(c, @intCast(offset));
//            emitEnum(c, wasm.Opcode.i32_add);
//        },
//        .get_repr_data => {
//            panic("TODO unions {}", .{node_data});
//            //return Repr.reprData();
//        },
//        .add => {
//            emitEnum(c, wasm.Opcode.i32_add);
//        },
//        .load => |load| {
//            const valtype = try valtypeFromRepr(c, load.repr);
//            emitEnum(c, switch (valtype) {
//                .i32 => wasm.Opcode.i32_load,
//                else => panic("TODO {}", .{valtype}),
//            });
//            emitLebU32(c, 0); // alignment
//            emitLebU32(c, 0); // offset
//        },
//        .store => |store| {
//            const valtype = try valtypeFromRepr(c, s.node_repr.get(store.value));
//            emitEnum(c, switch (valtype) {
//                .i32 => wasm.Opcode.i32_store,
//                else => panic("TODO {}", .{valtype}),
//            });
//            emitLebU32(c, 0); // alignment
//            emitLebU32(c, 0); // offset
//        },
//        .copy => {
//            emitEnum(c, wasm.Opcode.misc_prefix);
//            emitLebU32(c, @intFromEnum(wasm.MiscOpcode.memory_copy));
//            emitLebU32(c, 0); // from memory
//            emitLebU32(c, 0); // to memory
//        },
//        .stack_top => {
//            emitEnum(c, wasm.Opcode.i32_const);
//            emitLebU32(c, stack_top);
//        },
//    }
//}

fn emitByte(c: *Compiler, byte: u8) void {
    c.wasm.append(byte) catch oom();
}

fn emitEnum(c: *Compiler, e: anytype) void {
    switch (@TypeOf(e)) {
        wasm.Valtype, wasm.ExternalKind, wasm.Section, wasm.Opcode => {},
        else => @compileError(@typeName(@TypeOf(e))),
    }
    c.wasm.append(@intFromEnum(e)) catch oom();
}

fn emitBytes(c: *Compiler, bs: []const u8) void {
    c.wasm.appendSlice(bs) catch oom();
}

fn emitLebU32(c: *Compiler, i: u32) void {
    // https://webassembly.github.io/spec/core/binary/values.html#integers
    var n = i;
    while (true) {
        const is_last_chunk = n == 0;
        const chunk = @as(u8, @truncate(n & 0b0111_1111));
        n = n >> 7;
        const encoded_chunk = if (is_last_chunk) chunk else chunk | 0b1000_0000;
        c.wasm.append(encoded_chunk) catch oom();
        if (is_last_chunk) break;
    }
}

fn emitLebI32(c: *Compiler, i: i32) void {
    emitLebU32(c, @bitCast(i));
}

fn emitLebU64(c: *Compiler, i: u64) void {
    // https://webassembly.github.io/spec/core/binary/values.html#integers
    var n = i;
    while (true) {
        const is_last_chunk = n == 0;
        const chunk = @as(u8, @truncate(n & 0b0111_1111));
        n = n >> 7;
        const encoded_chunk = if (is_last_chunk) chunk else chunk | 0b1000_0000;
        c.wasm.append(encoded_chunk) catch oom();
        if (is_last_chunk) break;
    }
}

fn emitLebI64(c: *Compiler, i: i64) void {
    emitLebU64(c, @bitCast(i));
}

fn emitName(c: *Compiler, string: []const u8) void {
    emitLebU32(c, @intCast(string.len));
    emitBytes(c, string);
}

const bytesForU32Max = 5; // ceil(32/7)

const ByteCountLater = struct { index: usize };

// Write zeroes here and fix it up later.
fn emitByteCountLater(c: *Compiler) ByteCountLater {
    const index = c.wasm.items.len;
    c.wasm.appendNTimes(0, bytesForU32Max) catch oom();
    return .{ .index = index };
}

// Fix up some zeroes that we wrote earlier.
fn emitByteCount(c: *Compiler, byte_count_later: ByteCountLater) void {
    const ix = byte_count_later.index;
    const byte_count = c.wasm.items.len - ix - bytesForU32Max;

    // Same as emitLebU32, but overwriting existing zeroes.
    var n = byte_count;
    var offset: usize = 0;
    while (n >= (1 << 7)) {
        c.wasm.items[ix + offset] = @as(u8, @truncate(n)) | (1 << 7);
        n = n >> 7;
        offset += 1;
    }
    c.wasm.items[ix + offset] = @truncate(n);

    // Fix up trailing zeroes so the whole thing decodes as one number.
    while (offset < bytesForU32Max - 1) {
        c.wasm.items[ix + offset] |= 1 << 7;
        offset += 1;
    }
}

fn emitSectionStart(c: *Compiler, section: wasm.Section) ByteCountLater {
    emitEnum(c, section);
    return emitByteCountLater(c);
}

fn emitSectionEnd(c: *Compiler, byte_count_later: ByteCountLater) void {
    emitByteCount(c, byte_count_later);
}
