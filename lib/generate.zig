const std = @import("std");
const panic = std.debug.panic;
const assert = std.debug.assert;
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const wasm = std.wasm;

const zest = @import("./zest.zig");
const oom = zest.oom;
const Compiler = zest.Compiler;
const Repr = zest.Repr;
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
