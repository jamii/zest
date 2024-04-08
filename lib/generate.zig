const std = @import("std");
const panic = std.debug.panic;
const assert = std.debug.assert;
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const wasm = std.wasm;

const zest = @import("./zest.zig");
const oom = zest.oom;
const Compiler = zest.Compiler;
const Specialization = zest.Specialization;
const SpecializationData = zest.SpecializationData;
const Node = zest.Node;
const NodeData = zest.NodeData;
const Repr = zest.Repr;

const stack_global = 0;
const stack_top = 1 * wasm.page_size;

pub fn generate(c: *Compiler) error{GenerateError}!void {
    emitBytes(c, &wasm.magic);
    emitBytes(c, &wasm.version);

    {
        const section = emitSectionStart(c, wasm.Section.type);
        defer emitSectionEnd(c, section);

        emitLebU32(c, @intCast(c.specialization_data.count()));
        for (c.specialization_data.items()) |specialization_data| {
            emitByte(c, wasm.function_type);

            // Param types
            emitLebU32(c, @intCast(specialization_data.in_repr.count()));
            for (specialization_data.in_repr.items()) |in_repr| {
                emitEnum(c, try valtypeFromRepr(c, in_repr));
            }

            // Result types
            emitLebU32(c, 1);
            emitEnum(c, try valtypeFromRepr(c, specialization_data.out_repr));
        }
    }

    {
        const section = emitSectionStart(c, wasm.Section.function);
        defer emitSectionEnd(c, section);

        emitLebU32(c, @intCast(c.specialization_data.count()));
        for (0..c.specialization_data.count()) |specialization_id| {
            // Specialization i has type i
            emitLebU32(c, @intCast(specialization_id));
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
        emitLebU32(c, @intCast(c.specialization_main.?.id));

        emitName(c, "memory");
        emitEnum(c, wasm.ExternalKind.memory);
        emitLebU32(c, 0);
    }

    {
        const section = emitSectionStart(c, wasm.Section.code);
        defer emitSectionEnd(c, section);

        emitLebU32(c, @intCast(c.specialization_data.count()));
        for (c.specialization_data.items()) |s| {
            const start = emitByteCountLater(c);
            defer emitByteCount(c, start);

            // Locals
            emitLebU32(c, @intCast(s.local_repr.count()));
            for (s.local_repr.items()) |repr| {
                emitLebU32(c, 1);
                emitEnum(c, try valtypeFromRepr(c, repr));
            }

            var shadow_size: usize = 0;
            for (s.shadow_repr.items()) |repr| {
                shadow_size += repr.sizeOf();
            }

            // Frame push
            emitEnum(c, wasm.Opcode.global_get);
            emitLebU32(c, stack_global);
            emitEnum(c, wasm.Opcode.i32_const);
            emitLebU32(c, @intCast(shadow_size));
            emitEnum(c, wasm.Opcode.i32_sub);
            emitEnum(c, wasm.Opcode.global_set);
            emitLebU32(c, stack_global);

            // Body
            var node_next = s.node_first;
            while (node_next) |node| {
                emitNode(c, s, node);
                node_next = s.node_next.get(node);
            }

            // TODO this needs to happen before return!
            // Frame pop
            emitEnum(c, wasm.Opcode.global_get);
            emitLebU32(c, stack_global);
            emitEnum(c, wasm.Opcode.i32_const);
            emitLebU32(c, @intCast(shadow_size));
            emitEnum(c, wasm.Opcode.i32_add);
            emitEnum(c, wasm.Opcode.global_set);
            emitLebU32(c, stack_global);

            emitEnum(c, wasm.Opcode.end);
        }
    }
}

fn valtypeFromRepr(c: *Compiler, repr: Repr) !wasm.Valtype {
    _ = c;
    return switch (repr) {
        .i32 => .i32,
        .string, .@"struct", .@"union" => panic("Unexpected {}", .{repr}),
    };
}

fn emitNode(c: *Compiler, s: SpecializationData, node: Node) void {
    const node_data = s.node_data.get(node);
    switch (node_data) {
        .value => |value| {
            switch (value) {
                .i32 => |i| {
                    emitEnum(c, wasm.Opcode.i32_const);
                    emitLebI32(c, i);
                },
                .string, .@"struct", .@"union" => panic("Unexpected {}", .{node_data}),
            }
        },
        .struct_init => panic("Unexpected {}", .{node_data}),
        .@"return" => {
            emitEnum(c, wasm.Opcode.@"return");
        },
        .call => |call| {
            emitEnum(c, wasm.Opcode.call);
            emitLebU32(c, @intCast(call.specialization.?.id));
        },
        .intrinsic => |intrinsic| {
            switch (intrinsic) {
                .i32_add => {
                    emitEnum(c, wasm.Opcode.i32_add);
                },
            }
        },
        .get => panic("Unexpected {}", .{node_data}),
        .arg_get => |arg_get| {
            emitEnum(c, wasm.Opcode.local_get);
            emitLebU32(c, @intCast(arg_get.arg.id));
        },
        .local_get => |local_get| {
            emitEnum(c, wasm.Opcode.local_get);
            emitLebU32(c, @intCast(s.in_repr.count() + local_get.local.id));
        },
        .local_set => |local_set| {
            emitEnum(c, wasm.Opcode.local_set);
            emitLebU32(c, @intCast(s.in_repr.count() + local_set.local.id));
        },
        .shadow_ptr => |shadow| {
            var offset: usize = 0;
            for (s.shadow_repr.items()[0..shadow.id]) |repr_before| {
                offset += repr_before.sizeOf();
            }
            emitEnum(c, wasm.Opcode.global_get);
            emitLebU32(c, stack_global);
            emitEnum(c, wasm.Opcode.i32_const);
            emitLebU32(c, @intCast(offset));
            emitEnum(c, wasm.Opcode.i32_add);
        },
        .load => |load| {
            const valtype = try valtypeFromRepr(c, load.repr);
            emitEnum(c, switch (valtype) {
                .i32 => wasm.Opcode.i32_load,
                else => panic("TODO {}", .{valtype}),
            });
            emitLebU32(c, 0); // alignment
            emitLebU32(c, 0); // offset
        },
        .store => |store| {
            const valtype = try valtypeFromRepr(c, store.repr);
            emitEnum(c, switch (valtype) {
                .i32 => wasm.Opcode.i32_store,
                else => panic("TODO {}", .{valtype}),
            });
            emitLebU32(c, 0); // alignment
            emitLebU32(c, 0); // offset
        },
        .copy => |copy| {
            emitEnum(c, wasm.Opcode.i32_const);
            emitLebU32(c, @intCast(copy.byte_count));

            emitEnum(c, wasm.Opcode.misc_prefix);
            emitLebU32(c, @intFromEnum(wasm.MiscOpcode.memory_copy));
            emitLebU32(c, 0); // from memory
            emitLebU32(c, 0); // to memory
        },
    }
}

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
