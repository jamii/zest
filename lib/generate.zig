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
            if (specialization_data.out_repr) |out_repr| {
                emitLebU32(c, 1);
                emitEnum(c, try valtypeFromRepr(c, out_repr));
            } else {
                emitLebU32(c, 0);
            }
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
        // No minimum.
        emitLebU32(c, 0);
    }

    {
        const section = emitSectionStart(c, wasm.Section.@"export");
        defer emitSectionEnd(c, section);

        // Number of exports.
        emitLebU32(c, 2);

        emitName(c, "main");
        emitEnum(c, wasm.ExternalKind.function);
        emitLebU32(c, 0);

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

            var node_next = s.node_first;
            while (node_next) |node| {
                emitNodeData(c, s.node_data.get(node));
                node_next = s.node_next.get(node);
            }

            emitEnd(c);
        }
    }
}

fn valtypeFromRepr(c: *Compiler, repr: Repr) !wasm.Valtype {
    _ = c;
    return switch (repr) {
        .i32 => .i32,
    };
}

fn emitNodeData(c: *Compiler, node_data: NodeData) void {
    switch (node_data) {
        .value => |value| {
            switch (value) {
                .i32 => |i| {
                    emitEnum(c, wasm.Opcode.i32_const);
                    emitLebI32(c, i);
                },
            }
        },
        .local_get => |local| {
            emitEnum(c, wasm.Opcode.local_get);
            emitLebU32(c, @intCast(local.id));
        },
        .local_set => |local_set| {
            emitEnum(c, wasm.Opcode.local_set);
            emitLebU32(c, @intCast(local_set.local.id));
        },
        .@"return" => {
            emitEnum(c, wasm.Opcode.@"return");
        },
        .call => |call| {
            emitEnum(c, wasm.Opcode.call);
            emitLebU32(c, @intCast(call.specialization.?.id));
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

fn emitEnd(c: *Compiler) void {
    emitByte(c, 0x0B);
}
