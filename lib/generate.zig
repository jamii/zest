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

const global_shadow = 0;
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

            // Frame push
            if (f.shadow_offset_max > 0) {
                emitEnum(c, wasm.Opcode.global_get);
                emitLebU32(c, global_shadow);
                emitEnum(c, wasm.Opcode.i32_const);
                emitLebU32(c, @intCast(f.shadow_offset_max));
                emitEnum(c, wasm.Opcode.i32_sub);
                if (f.is_leaf) {
                    // Don't need to set global_shadow in leaf functions.
                    // TODO Could be more aggressive and not set global_shadow in non-leaf functions if all their callees don't use shadow.
                    emitEnum(c, wasm.Opcode.local_set);
                    emitLebU32(c, wasmLocal(c, &f, .shadow));
                } else {
                    emitEnum(c, wasm.Opcode.local_tee);
                    emitLebU32(c, wasmLocal(c, &f, .shadow));
                    emitEnum(c, wasm.Opcode.global_set);
                    emitLebU32(c, global_shadow);
                }
            }

            c.wasm.appendSlice(f.wasm.items) catch oom();

            // Frame pop
            if (f.shadow_offset_max > 0 and !f.is_leaf) {
                emitEnum(c, wasm.Opcode.local_get);
                emitLebU32(c, wasmLocal(c, &f, .shadow));
                emitEnum(c, wasm.Opcode.i32_const);
                emitLebU32(c, @intCast(f.shadow_offset_max));
                emitEnum(c, wasm.Opcode.i32_add);
                emitEnum(c, wasm.Opcode.global_set);
                emitLebU32(c, global_shadow);
            }

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
    arg_types.append(wasmAbi(tir_f.key.closure_repr)) catch oom();
    arg_types.append(wasmAbi(tir_f.key.arg_repr)) catch oom();
    switch (wasmRepr(tir_f.return_repr.one)) {
        .primitive => |valtype| return_types.append(valtype) catch oom(),
        .heap => arg_types.append(.i32) catch oom(),
    }
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

    // Prepare to track addresses for tir locals.
    assert(c.local_address.count() == 0);
    defer c.local_address.data.shrinkRetainingCapacity(0);
    c.local_address.appendNTimes(null, tir_f.local_data.count());

    // TODO Uncomment asserts once todo is gone.
    assert(c.block_stack.items.len == 0);
    //defer assert(c.block_stack.items.len == 0);
    assert(c.address_stack.items.len == 0);
    //defer assert(c.address_stack.items.len == 0);
    assert(c.hint_stack.items.len == 0);
    //defer assert(c.hint_stack.items.len == 0);

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
}

fn generateExpr(
    c: *Compiler,
    f: *wir.FunData,
    tir_f: tir.FunData,
    expr_data: tir.ExprData,
    repr: ?Repr,
    direction: enum { begin, end },
) error{GenerateError}!void {
    switch (expr_data) {
        .i32 => |i| {
            _ = c.hint_stack.pop();
            c.address_stack.append(.{ .direct = .{ .i32 = i } }) catch oom();
        },
        .closure => {
            _ = c.hint_stack.pop();
            c.address_stack.append(.{
                .direct = .closure,
                .indirect = addressIndirect(repr.?),
            }) catch oom();
        },
        .arg => {
            _ = c.hint_stack.pop();
            c.address_stack.append(.{
                .direct = .arg,
                .indirect = addressIndirect(repr.?),
            }) catch oom();
        },
        .local_get => |local| {
            _ = c.hint_stack.pop();
            const address = c.local_address.get(local).?;
            c.address_stack.append(address) catch oom();
        },

        .begin => unreachable,
        .nop => {},

        .struct_init => {
            const struct_repr = repr.?.@"struct";
            switch (direction) {
                .begin => {
                    const hint_maybe = c.hint_stack.pop();
                    var i: usize = struct_repr.reprs.len;
                    while (i > 0) : (i -= 1) {
                        const value_repr = struct_repr.reprs[i - 1];
                        const offset = @as(u32, @intCast(struct_repr.offsetOf(i - 1)));
                        const value_hint = if (hint_maybe) |hint| wir.Address{
                            .direct = hint.direct,
                            .indirect = .{
                                .offset = hint.indirect.?.offset + offset,
                                .repr = value_repr,
                            },
                        } else null;
                        c.hint_stack.append(value_hint) catch oom();
                    }
                },
                .end => {
                    const values = c.allocator.alloc(wir.Address, struct_repr.reprs.len) catch oom();
                    var i: usize = struct_repr.reprs.len;
                    while (i > 0) : (i -= 1) {
                        values[i - 1] = c.address_stack.pop();
                    }
                    c.address_stack.append(.{ .direct = .{ .@"struct" = .{
                        .repr = struct_repr,
                        .values = values,
                    } } }) catch oom();
                },
            }
        },
        .fun_init => {
            switch (direction) {
                .begin => {
                    var hint = c.hint_stack.pop();
                    if (hint != null) {
                        hint.?.indirect.?.repr = .{ .@"struct" = repr.?.fun.closure };
                    }
                    c.hint_stack.append(hint) catch oom();
                },
                .end => {
                    var output = c.address_stack.pop();
                    if (output.direct == .@"struct") {
                        c.address_stack.append(.{ .direct = .{
                            .fun = .{
                                .repr = repr.?.fun,
                                .closure = c.box(output),
                            },
                        } }) catch oom();
                    } else {
                        output.indirect.?.repr = repr.?;
                        c.address_stack.append(output) catch oom();
                    }
                },
            }
        },
        .local_let => |local| {
            switch (direction) {
                .begin => c.hint_stack.append(null) catch oom(),
                .end => {
                    const input = stackToLocal(c, f, c.address_stack.pop());
                    // TODO If input fits in a local, might want to load eagerly rather than at use site, which might be eg in a loop.
                    c.local_address.getPtr(local).* = input;
                },
            }
        },
        .object_get => |object_get| {
            switch (direction) {
                .begin => {
                    _ = c.hint_stack.pop();
                    c.hint_stack.append(null) catch oom();
                },
                .end => {
                    const input = c.address_stack.pop();
                    if (input.indirect) |indirect| {
                        const input_repr = indirect.repr.@"struct";
                        const i = input_repr.get(object_get.key).?;
                        const offset = @as(u32, @intCast(input_repr.offsetOf(i)));
                        const output = wir.Address{
                            .direct = input.direct,
                            .indirect = .{
                                .offset = indirect.offset + offset,
                                .repr = input_repr.reprs[i],
                            },
                        };
                        c.address_stack.append(output) catch oom();
                    } else {
                        // Only one direct address that can handle structs.
                        assert(input.direct == .@"struct");
                        const i = input.direct.@"struct".repr.get(object_get.key).?;
                        var j: usize = input.direct.@"struct".values.len - 1;
                        while (j > i) : (j -= 1) {
                            const ignored = input.direct.@"struct".values[j];
                            if (ignored.direct == .stack)
                                emitEnum(f, wasm.Opcode.drop);
                        }
                        const output = input.direct.@"struct".values[i];
                        c.address_stack.append(output) catch oom();
                    }
                },
            }
        },
        .call => |fun| {
            f.is_leaf = false;
            switch (direction) {
                .begin => {
                    c.hint_stack.append(null) catch oom(); // arg
                    c.hint_stack.append(null) catch oom(); // closure
                },
                .end => {
                    const output = c.hint_stack.pop() orelse
                        switch (wasmRepr(repr.?)) {
                        .primitive => |valtype| wir.Address{ .direct = .{ .stack = valtype } },
                        .heap => shadowPush(c, f, repr.?),
                    };
                    const arg = stackToLocal(c, f, c.address_stack.pop());
                    const closure = c.address_stack.pop();
                    loadPtr(c, f, closure);
                    loadPtr(c, f, arg);
                    switch (wasmRepr(repr.?)) {
                        .primitive => {},
                        .heap => loadPtr(c, f, output),
                    }
                    emitEnum(f, wasm.Opcode.call);
                    emitLebU32(f, @intCast(fun.id));
                    c.address_stack.append(output) catch oom();
                },
            }
        },
        .drop => {
            switch (direction) {
                .begin => c.hint_stack.append(null) catch oom(),
                .end => {
                    const input = c.address_stack.pop();
                    if (input.direct == .stack)
                        emitEnum(f, wasm.Opcode.drop);
                },
            }
        },
        .block => {
            // TODO stack reset
        },
        .@"return" => {
            const output: wir.Address = switch (wasmRepr(tir_f.return_repr.one)) {
                .primitive => |valtype| .{
                    .direct = .{ .stack = valtype },
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
                .begin => c.hint_stack.append(output) catch oom(),
                .end => {
                    const input = c.address_stack.pop();
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

fn shadowPush(c: *Compiler, f: *wir.FunData, repr: Repr) wir.Address {
    _ = c;
    const offset = f.shadow_offset_next;
    f.shadow_offset_next += repr.sizeOf();
    if (f.shadow_offset_next > 0 and f.shadow_offset_max == 0) {
        assert(f.local_shadow == null);
        f.local_shadow = f.local_data.append(.{ .type = .i32 });
    }
    f.shadow_offset_max = @max(f.shadow_offset_max, f.shadow_offset_next);
    return .{
        .direct = .shadow,
        .indirect = .{
            .offset = @intCast(offset),
            .repr = repr,
        },
    };
}

fn copy(c: *Compiler, f: *wir.FunData, from: wir.Address, to: wir.Address) void {
    // Nop
    if (from.equal(to))
        return;

    // Copy
    if (from.indirect != null and to.indirect != null) {
        // TODO If repr.sizeOf() is in [1, 2, 4, 8] do a load and store instead.
        const repr = from.indirect.?.repr;
        assert(repr.equal(to.indirect.?.repr));
        loadPtr(c, f, to);
        loadPtr(c, f, from);
        emitEnum(f, wasm.Opcode.i32_const);
        emitLebU32(f, @intCast(repr.sizeOf()));
        emitEnum(f, wasm.Opcode.misc_prefix);
        emitLebU32(f, wasm.miscOpcode(wasm.MiscOpcode.memory_copy));
        emitLebU32(f, 0); // memory from
        emitLebU32(f, 0); // memory to
        return;
    }

    // Constant to heap.
    if (from.isValue() and to.indirect != null) {
        switch (from.direct) {
            .i32 => {
                loadDirect(c, f, to.direct);
                load(c, f, from);
                emitEnum(f, wasm.Opcode.i32_store);
                emitLebU32(f, 0); // align
                emitLebU32(f, to.indirect.?.offset);
            },
            .@"struct" => |@"struct"| {
                var i: usize = @"struct".repr.reprs.len;
                while (i > 0) : (i -= 1) {
                    const repr = @"struct".repr.reprs[i - 1];
                    const value_from = @"struct".values[i - 1];
                    const offset = @"struct".repr.offsetOf(i - 1);
                    copy(c, f, value_from, .{
                        .direct = to.direct,
                        .indirect = .{
                            .offset = to.indirect.?.offset + @as(u32, @intCast(offset)),
                            .repr = repr,
                        },
                    });
                }
            },
            .fun => |fun| {
                var closure_to = to;
                closure_to.indirect.?.repr = .{ .@"struct" = closure_to.indirect.?.repr.fun.closure };
                copy(c, f, fun.closure.*, closure_to);
            },
            .closure, .arg, .@"return", .local, .shadow, .stack => unreachable,
        }
        return;
    }

    // Otherwise copy via stack.
    load(c, f, from);
    store(c, f, to);
}

fn loadDirect(c: *Compiler, f: *wir.FunData, from: wir.AddressDirect) void {
    switch (from) {
        .closure, .arg, .@"return", .local, .shadow => {
            emitEnum(f, wasm.Opcode.local_get);
            emitLebU32(f, wasmLocal(c, f, from));
        },
        .stack => {},
        .i32 => |i| {
            emitEnum(f, wasm.Opcode.i32_const);
            emitLebI32(f, i);
        },
        .@"struct", .fun => panic("Can't load from {}", .{from}),
    }
}

fn load(c: *Compiler, f: *wir.FunData, from: wir.Address) void {
    loadDirect(c, f, from.direct);
    if (from.indirect) |indirect| {
        switch (indirect.repr) {
            .i32 => {
                emitEnum(f, wasm.Opcode.i32_load);
                emitLebU32(f, 0); // align
                emitLebU32(f, indirect.offset);
            },
            else => panic("Can't load repr {}", .{indirect.repr}),
        }
    }
}

fn store(c: *Compiler, f: *wir.FunData, to: wir.Address) void {
    if (to.indirect) |indirect| {
        switch (indirect.repr) {
            .i32 => {
                const shuffler = getShuffler(f, .i32);
                emitEnum(f, wasm.Opcode.local_set);
                emitLebU32(f, wasmLocal(c, f, .{ .local = shuffler }));
                loadDirect(c, f, to.direct);
                emitEnum(f, wasm.Opcode.local_get);
                emitLebU32(f, wasmLocal(c, f, .{ .local = shuffler }));
                emitEnum(f, wasm.Opcode.i32_store);
                emitLebU32(f, 0); // align
                emitLebU32(f, indirect.offset);
            },
            else => panic("Can't store repr {}", .{indirect.repr}),
        }
    } else {
        switch (to.direct) {
            .closure, .arg, .@"return", .local, .shadow => {
                emitEnum(f, wasm.Opcode.local_set);
                emitLebU32(f, wasmLocal(c, f, to.direct));
            },
            .stack => {},
            .i32, .@"struct", .fun => panic("Can't store to {}", .{to}),
        }
    }
}

fn getShuffler(f: *wir.FunData, typ: wasm.Valtype) wir.Local {
    const shuffler = f.local_shufflers.getPtr(typ);
    if (shuffler.* == null) shuffler.* = f.local_data.append(.{ .type = typ });
    return shuffler.*.?;
}

fn loadPtr(c: *Compiler, f: *wir.FunData, address: wir.Address) void {
    if (address.indirect != null and address.indirect.?.repr.sizeOf() == 0) {
        emitEnum(f, wasm.Opcode.i32_const);
        emitLebU32(f, 0);
        return;
    }

    if (address.isValue()) {
        const tmp = shadowPush(c, f, switch (address.direct) {
            .i32 => .i32,
            .@"struct" => |@"struct"| .{ .@"struct" = @"struct".repr },
            .fun => |fun| .{ .fun = fun.repr },
            .closure, .arg, .@"return", .local, .shadow, .stack => unreachable,
        });
        copy(c, f, address, tmp);
        loadPtr(c, f, tmp);
        return;
    }

    assert(address.indirect != null);

    loadDirect(c, f, address.direct);
    if (address.indirect.?.offset > 0) {
        emitEnum(f, wasm.Opcode.i32_const);
        emitLebU32(f, address.indirect.?.offset);
        emitEnum(f, wasm.Opcode.i32_add);
    }
}

fn stackToLocal(c: *Compiler, f: *wir.FunData, address: wir.Address) wir.Address {
    switch (address.direct) {
        .stack => |valtype| {
            const local = f.local_data.append(.{ .type = valtype });
            emitEnum(f, wasm.Opcode.local_set);
            emitLebU32(f, wasmLocal(c, f, .{ .local = local }));
            return .{
                .direct = .{ .local = local },
                .indirect = address.indirect,
            };
        },
        .@"struct" => |@"struct"| {
            const values = c.allocator.alloc(wir.Address, @"struct".values.len) catch oom();
            var i: usize = @"struct".values.len;
            while (i > 0) : (i -= 1) {
                values[i - 1] = stackToLocal(c, f, @"struct".values[i - 1]);
            }
            return .{
                .direct = .{ .@"struct" = .{ .repr = @"struct".repr, .values = values } },
                .indirect = address.indirect,
            };
        },
        .fun => |fun| {
            const closure = c.box(stackToLocal(c, f, fun.closure.*));
            return .{
                .direct = .{ .fun = .{ .repr = fun.repr, .closure = closure } },
                .indirect = address.indirect,
            };
        },
        .closure, .arg, .@"return", .local, .shadow, .i32 => {
            return address;
        },
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

const WasmRepr = union(enum) {
    primitive: wasm.Valtype,
    heap,
};

fn wasmRepr(repr: Repr) WasmRepr {
    return switch (repr) {
        .i32 => .{ .primitive = wasmAbi(repr) },
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

fn wasmLocal(c: *Compiler, f: *const wir.FunData, address: wir.AddressDirect) u32 {
    return switch (address) {
        .closure => 0,
        .arg => 1,
        .@"return" => 2,
        .local => |local| @intCast(c.fun_type_data.get(f.fun_type).arg_types.len + local.id),
        .shadow => @intCast(c.fun_type_data.get(f.fun_type).arg_types.len + f.local_shadow.?.id),
        .stack, .i32, .@"struct", .fun => panic("Not a local: {}", .{address}),
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
