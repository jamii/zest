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

            // Locals
            emitLebU32(c, @intCast(f.local_data.count()));
            for (f.local_data.items()) |l| {
                emitLebU32(c, 1);
                emitEnum(c, l.type);
            }

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

    // Get fun_type.
    var arg_types = ArrayList(wasm.Valtype).init(c.allocator);
    var return_types = ArrayList(wasm.Valtype).init(c.allocator);
    if (!tir_f.key.closure_repr.isEmptyStruct() or
        !tir_f.key.arg_repr.isEmptyStruct())
        return fail(c, .todo);
    var return_arg_count: u32 = 0;
    switch (wasmRepr(tir_f.return_repr.one)) {
        .primitive => {},
        .heap => {
            return_arg_count += 1;
            arg_types.append(.i32) catch oom();
        },
    }
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

    _ = c.wir_fun_data.append(wir.FunData.init(c.allocator, fun_type.?, return_arg_count));
    const f = c.wir_fun_data.getPtr(fun);

    // Map begin exprs to their end.
    assert(c.begin_stack.items.len == 0);
    defer assert(c.begin_stack.items.len == 0);
    assert(c.begin_end.count() == 0);
    defer c.begin_end.data.shrinkRetainingCapacity(0);
    c.begin_end.appendNTimes(.{ .id = 0 }, tir_f.expr_data.count());
    for (tir_f.expr_data.items(), 0..) |expr_data, expr_id| {
        if (expr_data == .begin) {
            c.begin_stack.append(.{ .id = expr_id }) catch oom();
        }
        if (expr_data.isEnd()) {
            const begin = c.begin_stack.pop();
            c.begin_end.getPtr(begin).* = .{ .id = expr_id };
        }
    }

    // Map tir locals to wasm locals or shadow stack.
    assert(c.local_address.count() == 0);
    defer c.local_address.data.shrinkRetainingCapacity(0);
    for (tir_f.local_data.items()) |local_data| {
        const repr = local_data.repr.one;
        switch (wasmRepr(repr)) {
            .primitive => {
                const local = f.local_data.append(.{ .type = wasmAbi(repr) });
                _ = c.local_address.append(.{ .direct = .{ .local = local } });
            },
            .heap => {
                _ = c.local_address.append(.{
                    .direct = .shadow,
                    .indirect = .{
                        // We'll set offset when we hit local_let.
                        .offset = 0,
                        .repr = repr,
                    },
                });
            },
        }
    }

    // TODO Uncomment asserts once todo is gone.
    assert(c.shadow_offset_stack.items.len == 0);
    //defer assert(c.shadow_offset_stack.items.len == 0);
    assert(c.block_stack.items.len == 0);
    //defer assert(c.block_stack.items.len == 0);
    assert(c.input_stack.items.len == 0);
    //defer assert(c.input_stack.items.len == 0);
    assert(c.output_stack.items.len == 0);
    //defer assert(c.output_stack.items.len == 0);

    for (tir_f.expr_data.items(), tir_f.expr_repr.items(), 0..) |expr_data, repr, expr_id| {
        if (expr_data == .begin) {
            const end_expr = c.begin_end.get(.{ .id = expr_id });
            const end_expr_data = tir_f.expr_data.get(end_expr);
            const end_repr = tir_f.expr_repr.get(end_expr);
            try generateExpr(c, f, tir_f, end_expr_data, end_repr, .begin);
        } else {
            try generateExpr(c, f, tir_f, expr_data, repr, .end);
        }
    }

    if (f.shadow_offset_max > 0) {
        // Add a variable for the shadow.
        _ = f.local_data.append(.{ .type = .i32 });
    }
}

fn generateExpr(
    c: *Compiler,
    f: *wir.FunData,
    tir_f: tir.FunData,
    expr_data: tir.ExprData,
    repr: ?Repr,
    direction: enum { begin, end },
) error{GenerateError}!void {
    //std.debug.print("{} {}\n", .{ expr_data, direction });
    switch (expr_data) {
        .i32 => |i| {
            _ = c.output_stack.pop();
            emitEnum(f, wasm.Opcode.i32_const);
            emitLebI32(f, i);
            c.input_stack.append(.{
                .direct = .stack,
                .indirect = addressIndirect(repr.?),
            }) catch oom();
        },
        .closure => {
            _ = c.output_stack.pop();
            c.input_stack.append(.{
                .direct = .closure,
                .indirect = addressIndirect(repr.?),
            }) catch oom();
        },
        .arg => {
            _ = c.output_stack.pop();
            c.input_stack.append(.{
                .direct = .arg,
                .indirect = addressIndirect(repr.?),
            }) catch oom();
        },
        .local_get => |local| {
            _ = c.output_stack.pop();
            c.input_stack.append(c.local_address.get(local)) catch oom();
        },

        .begin => unreachable,

        .object_get => |object_get| {
            switch (direction) {
                .begin => c.output_stack.append(null) catch oom(),
                .end => {
                    const input = c.input_stack.pop();
                    const input_repr = input.indirect.?.repr.@"struct";
                    const i = input_repr.get(object_get.key).?;
                    const offset = @as(u32, @intCast(input_repr.offsetOf(i)));
                    c.input_stack.append(.{
                        .direct = input.direct,
                        .indirect = .{
                            .offset = input.indirect.?.offset + offset,
                            .repr = input_repr.reprs[i],
                        },
                    }) catch oom();
                },
            }
        },
        .drop => {
            const output = wir.Address{ .direct = .nowhere };
            switch (direction) {
                .begin => c.output_stack.append(output) catch oom(),
                .end => {
                    const input = c.input_stack.pop();
                    copy(c, f, input, output);
                },
            }
        },
        .block => {},
        .@"return" => {
            const output: wir.Address = switch (wasmRepr(tir_f.return_repr.one)) {
                .primitive => .{
                    .direct = .stack,
                },
                .heap => .{
                    .direct = .@"return",
                    .indirect = .{
                        .offset = 0,
                        .repr = tir_f.return_repr.one,
                    },
                },
            };
            switch (direction) {
                .begin => {
                    c.output_stack.append(output) catch oom();
                },
                .end => {
                    const input = c.input_stack.pop();
                    copy(c, f, input, output);
                },
            }
        },

        else => {
            //std.debug.print("TODO generate {}\n", .{expr_data});
            return fail(c, .todo);
        },
    }
}

fn copy(c: *Compiler, f: *wir.FunData, from: wir.Address, to: wir.Address) void {
    if (deepEqual(from, to)) return;
    if (from.indirect == null and to.indirect == null) {
        load(c, f, from);
        store(c, f, to);
    } else {
        panic("TODO", .{});
    }
}

fn load(c: *Compiler, f: *wir.FunData, from: wir.Address) void {
    loadDirect(c, f, from.direct);
    if (from.indirect) |indirect| {
        _ = indirect;
        panic("TODO", .{});
    }
}

fn loadDirect(c: *Compiler, f: *wir.FunData, from: wir.AddressDirect) void {
    _ = c;
    switch (from) {
        .closure, .arg, .@"return", .local, .shadow => {
            emitEnum(f, wasm.Opcode.local_get);
            emitLebU32(f, wasmLocal(f, from));
        },
        .stack => {},
        .nowhere => panic("Can't load from nowhere", .{}),
    }
}

fn store(c: *Compiler, f: *wir.FunData, to: wir.Address) void {
    _ = c;
    if (to.indirect) |indirect| {
        _ = indirect;
        // Expect base address on stack already :(
        panic("TODO", .{});
    } else {
        switch (to.direct) {
            .closure, .arg, .@"return", .local, .shadow => {
                emitEnum(f, wasm.Opcode.local_set);
                emitLebU32(f, wasmLocal(f, to.direct));
            },
            .stack => {},
            .nowhere => {
                emitEnum(f, wasm.Opcode.drop);
            },
        }
    }
}

fn addressIndirect(repr: Repr) ?wir.AddressIndirect {
    return switch (wasmRepr(repr)) {
        .primitive => null,
        .heap => .{
            .offset = 0,
            .repr = repr,
        },
    };
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

fn wasmLocal(f: *const wir.FunData, address: wir.AddressDirect) u32 {
    return switch (address) {
        .closure => 0,
        .arg => 1,
        .@"return" => 1 + f.return_arg_count,
        .local => |local| 1 + f.return_arg_count + @as(u32, @intCast(local.id)),
        .shadow => 1 + f.return_arg_count + @as(u32, @intCast(f.local_data.count())),
        .stack, .nowhere => panic("Not a local: {}", .{address}),
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
