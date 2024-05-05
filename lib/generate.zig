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
    for (0..c.tir_fun_data.count()) |tir_fun_id| {
        try generateFun(c, .{ .id = tir_fun_id });
    }

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
        emitLebU32(c, @intCast(c.tir_fun_main.?.id));

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

            //var locals_count: u32 = 0;

            //const arg_start = locals_count;
            //locals_count += @intCast(c.fun_type_data.get(f.fun_type).arg_types.len);

            //const local_start = locals_count;
            //locals_count += @intCast(f.local_data.count());

            //const frame_ptr = locals_count;
            //locals_count += 1;

            //const local_map = LocalMap{
            //    .arg_start = arg_start,
            //    .local_start = local_start,
            //    .frame_ptr = frame_ptr,
            //};

            // Locals
            //emitLebU32(c, @intCast(locals_count));
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

            c.wasm.appendSlice(f.wasm.items) catch oom();

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

fn generateFun(c: *Compiler, fun: tir.Fun) error{GenerateError}!void {
    const tir_f = c.tir_fun_data.get(fun);

    var arg_types = ArrayList(wasm.Valtype).init(c.allocator);
    var return_types = ArrayList(wasm.Valtype).init(c.allocator);
    if (!tir_f.key.closure_repr.isEmptyStruct() or
        !tir_f.key.arg_repr.isEmptyStruct())
        return fail(c, .todo);
    return_types.append(wasmAbi(tir_f.return_repr.one)) catch oom();
    const fun_type_data = wir.FunTypeData{
        .arg_types = arg_types.toOwnedSlice() catch oom(),
        .return_types = return_types.toOwnedSlice() catch oom(),
    };
    var fun_type: ?wir.FunType = c.fun_type_memo.get(fun_type_data);
    if (fun_type == null) {
        fun_type = c.fun_type_data.append(fun_type_data);
        c.fun_type_memo.put(fun_type_data, fun_type.?) catch oom();
    }

    _ = c.wir_fun_data.append(wir.FunData.init(c.allocator, fun_type.?));
    const f = c.wir_fun_data.getPtr(fun);

    _ = f;

    //for (tir_f.local_data.items()) |local_data| {
    //    const repr = local_data.repr.one;
    //    switch (lowerRepr(repr)) {
    //        .primitive => |typ| {
    //            const local = f.local_data.append(.{ .type = typ });
    //            _ = f.local_from_tir.append(local);
    //        },
    //        .shadow => {
    //            _ = f.local_from_tir.append(null);
    //        },
    //    }
    //}

    //assert(c.wir_address_stack.items.len == 0);

    ////std.debug.print("---\n", .{});
    //for (tir_f.expr_data.items(), tir_f.expr_repr.items(), tir_f.expr_address.items(), 0..) |expr_data, repr, tir_address, expr_id| {
    //    //std.debug.print("{} {}\n", .{ expr_data, c.wir_address_stack.items.len });
    //    switch (expr_data) {
    //        .i32 => |i| {
    //            _ = f.expr_data.append(.{ .i32 = i });
    //            c.wir_address_stack.append(null) catch oom();
    //        },
    //        .struct_init => {
    //            const address = shadowPush(c, f, tir_address.?, repr.?);
    //            const value_reprs = repr.?.@"struct".reprs;
    //            var i: usize = value_reprs.len;
    //            while (i > 0) : (i -= 1) {
    //                const value_repr = value_reprs[i - 1];
    //                const value_offset = repr.?.@"struct".offsetOf(i - 1);
    //                store(c, f, .{
    //                    .base = address.base,
    //                    .offset = @intCast(address.offset + value_offset),
    //                    .repr = value_repr,
    //                });
    //            }
    //            c.wir_address_stack.append(address) catch oom();
    //        },
    //        .local_get => |tir_local| {
    //            const local_repr = tir_f.local_data.get(tir_local).repr.one;
    //            switch (lowerRepr(local_repr)) {
    //                .primitive => {
    //                    _ = f.expr_data.append(.{ .local_get = f.local_from_tir.get(tir_local).? });
    //                    c.wir_address_stack.append(null) catch oom();
    //                },
    //                .shadow => {
    //                    return fail(c, .todo);
    //                },
    //            }
    //        },
    //        .local_let => |tir_local| {
    //            const local_repr = tir_f.local_data.get(tir_local).repr.one;
    //            switch (lowerRepr(local_repr)) {
    //                .primitive => {
    //                    assert(c.wir_address_stack.pop() == null);
    //                    _ = f.expr_data.append(.{ .local_set = f.local_from_tir.get(tir_local).? });
    //                },
    //                .shadow => {
    //                    return fail(c, .todo);
    //                },
    //            }
    //        },
    //        .object_get => |object_get| {
    //            const object_address = c.wir_address_stack.pop().?;
    //            const object_repr = object_address.repr.@"struct"; // TODO other object reprs
    //            const i = object_repr.get(object_get.key).?;
    //            const offset = object_repr.offsetOf(i);
    //            c.wir_address_stack.append(.{
    //                .base = object_address.base,
    //                .offset = object_address.offset + @as(u32, @intCast(offset)),
    //                .repr = object_repr.reprs[i],
    //            }) catch oom();
    //        },
    //        .drop => {
    //            const dropped_address = c.wir_address_stack.pop();
    //            if (dropped_address == null) {
    //                _ = f.expr_data.append(.drop);
    //            }
    //        },
    //        .block_begin => {
    //            f.block_stack.append(.{
    //                .block_begin = .{ .id = expr_id },
    //                .shadow_offset_next = f.shadow_offset_next,
    //                .shadow_address_index = f.shadow_address_stack.items.len,
    //            }) catch oom();
    //        },
    //        .block_end => |block_end| {
    //            const block = f.block_stack.pop();
    //            assert(expr_id - block_end.expr_count == block.block_begin.id);
    //            f.shadow_offset_next = block.shadow_offset_next;
    //            f.shadow_address_stack.shrinkRetainingCapacity(block.shadow_address_index);
    //        },
    //        .@"return" => {
    //            if (c.wir_address_stack.pop()) |from_address| {
    //                copy(c, f, from_address, .{ .base = .@"return", .offset = 0, .repr = tir_f.return_repr.one });
    //            }
    //            _ = f.expr_data.append(.@"return");
    //        },
    //        else => {
    //            //std.debug.print("{}\n", .{expr_data});
    //            return fail(c, .todo);
    //        },
    //    }
    //}

    //assert(c.wir_address_stack.items.len == 0);
}

const WasmRepr = enum {
    primitive,
    heap,
};

fn wasmRepr(repr: Repr) WasmRepr {
    return switch (repr) {
        .i32 => .primitive,
        .string, .@"struct", .@"union", .fun, .only, .repr => .heap,
    };
}

fn wasmAbi(repr: Repr) wasm.Valtype {
    return switch (repr) {
        .i32 => .i32,
        // Pointer to heap.
        .string, .@"struct", .@"union", .fun, .only, .repr => .i32,
    };
}

fn emitByte(c: anytype, byte: u8) void {
    c.wasm.append(byte) catch oom();
}

fn emitEnum(c: anytype, e: anytype) void {
    switch (@TypeOf(e)) {
        wasm.Valtype, wasm.ExternalKind, wasm.Section, wasm.Opcode => {},
        else => @compileError(@typeName(@TypeOf(e))),
    }
    c.wasm.append(@intFromEnum(e)) catch oom();
}

fn emitBytes(c: anytype, bs: []const u8) void {
    c.wasm.appendSlice(bs) catch oom();
}

fn emitLebU32(c: anytype, i: u32) void {
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

fn emitLebI32(c: anytype, i: i32) void {
    emitLebU32(c, @bitCast(i));
}

fn emitLebU64(c: anytype, i: u64) void {
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

fn emitLebI64(c: anytype, i: i64) void {
    emitLebU64(c, @bitCast(i));
}

fn emitName(c: anytype, string: []const u8) void {
    emitLebU32(c, @intCast(string.len));
    emitBytes(c, string);
}

const bytesForU32Max = 5; // ceil(32/7)

const ByteCountLater = struct { index: usize };

// Write zeroes here and fix it up later.
fn emitByteCountLater(c: anytype) ByteCountLater {
    const index = c.wasm.items.len;
    c.wasm.appendNTimes(0, bytesForU32Max) catch oom();
    return .{ .index = index };
}

// Fix up some zeroes that we wrote earlier.
fn emitByteCount(c: anytype, byte_count_later: ByteCountLater) void {
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

fn emitSectionStart(c: anytype, section: wasm.Section) ByteCountLater {
    emitEnum(c, section);
    return emitByteCountLater(c);
}

fn emitSectionEnd(c: anytype, byte_count_later: ByteCountLater) void {
    emitByteCount(c, byte_count_later);
}

pub const GenerateErrorData = union(enum) {
    todo,
};

fn fail(c: *Compiler, data: GenerateErrorData) error{GenerateError} {
    c.error_data = .{ .generate = .{ .data = data } };
    return error.GenerateError;
}
