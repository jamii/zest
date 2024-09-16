const std = @import("std");
const panic = std.debug.panic;
const assert = std.debug.assert;
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const wasm = std.wasm;

const zest = @import("./zest.zig");
const oom = zest.oom;
const deepEqual = zest.deepEqual;
const treePart = zest.treePart;
const Compiler = zest.Compiler;
const Repr = zest.Repr;
const Value = zest.Value;
const tir = zest.tir;
const wir = zest.wir;

const global_shadow = 0;
const global_heap_start = 1;
const stack_pages = 128;
const stack_top = stack_pages * wasm.page_size; // 8mb

const imports = .{
    .{
        .name = "print_u32",
        .arg_types = &[_]wasm.Valtype{.i32},
        .return_types = &.{},
    },
    .{
        .name = "print_i64",
        .arg_types = &[_]wasm.Valtype{.i64},
        .return_types = &.{},
    },
    .{
        .name = "print_string",
        .arg_types = &[_]wasm.Valtype{ .i32, .i32 },
        .return_types = &.{},
    },
};

pub fn generate(c: *Compiler) error{GenerateError}!void {

    // This must be kept in sync with ./runtime.zs
    const allocator_byte_count = n: {
        const wasm_page_byte_count_log = 16;
        const class_min_byte_count_log = 3;
        const class_count = 32 - class_min_byte_count_log;
        const class_small_count = wasm_page_byte_count_log - class_min_byte_count_log;
        // Need space for:
        //   alloc_free_ptr: array[class_count, u32]
        //   alloc_next_ptr: array[class_small_count, u32]
        break :n (class_count + class_small_count) * @sizeOf(u32);
    };

    for (0.., c.tir_fun_data.items()) |tir_fun_id, tir_f| {
        const tir_fun = tir.Fun{ .id = tir_fun_id };
        const dir_f = c.dir_fun_data.get(tir_f.key.fun);
        if (!dir_f.@"inline") {

            // Get fun_type.
            var arg_types = ArrayList(wasm.Valtype).init(c.allocator);
            var return_types = ArrayList(wasm.Valtype).init(c.allocator);
            if (!tir_f.key.closure_repr.isEmptyStruct()) {
                arg_types.append(wasmAbi(tir_f.key.closure_repr)) catch oom();
            }
            for (tir_f.key.arg_reprs) |arg_repr| {
                arg_types.append(wasmAbi(arg_repr)) catch oom();
            }
            switch (wasmRepr(tir_f.return_repr.one)) {
                .primitive => |valtype| return_types.append(valtype) catch oom(),
                .heap => if (tir_f.return_repr.one.sizeOf() > 0) arg_types.append(.i32) catch oom(),
            }
            const fun_type = memoFunType(c, .{
                .arg_types = arg_types.toOwnedSlice() catch oom(),
                .return_types = return_types.toOwnedSlice() catch oom(),
            });

            // Make wir fun.
            const wir_fun = c.wir_fun_data.append(wir.FunData.init(c.allocator, tir_fun, fun_type));
            c.wir_fun_by_tir.put(tir_fun, wir_fun) catch oom();
        }
    }

    var import_types: [imports.len]wir.FunType = undefined;
    inline for (imports, 0..) |import, i| {
        import_types[i] = memoFunType(c, .{
            .arg_types = c.dupe(wasm.Valtype, import.arg_types),
            .return_types = c.dupe(wasm.Valtype, import.return_types),
        });
    }

    for (c.wir_fun_data.items()) |*f| {
        try genFun(c, f);
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
        const section = emitSectionStart(c, wasm.Section.import);
        defer emitSectionEnd(c, section);

        emitLebU32(c, imports.len);
        inline for (imports, 0..) |import, i| {
            emitName(c, "env");
            emitName(c, import.name);
            emitEnum(c, wasm.ExternalKind.function);
            emitLebU32(c, @intCast(import_types[i].id));
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
        // At minimum enough space for stack and constant data.
        emitLebU32(c, std.math.divCeil(u32, stack_top + @as(u32, @intCast(c.constant_data.items.len + allocator_byte_count)), wasm.page_size) catch unreachable);
    }

    {
        const section = emitSectionStart(c, wasm.Section.global);
        defer emitSectionEnd(c, section);

        // Number of globals.
        emitLebU32(c, 2);

        // global_shadow
        emitEnum(c, wasm.Valtype.i32);
        emitByte(c, 0x01); // mutable
        emitU32Const(c, stack_top);
        emitEnum(c, wasm.Opcode.end);

        // global_heap_start
        emitEnum(c, wasm.Valtype.i32);
        emitByte(c, 0x00); // const
        emitU32Const(c, stack_top + @as(u32, @intCast(c.constant_data.items.len + allocator_byte_count)));
        emitEnum(c, wasm.Opcode.end);
    }

    {
        const section = emitSectionStart(c, wasm.Section.@"export");
        defer emitSectionEnd(c, section);

        // Number of exports.
        emitLebU32(c, 2);

        emitName(c, "main");
        emitEnum(c, wasm.ExternalKind.function);
        emitLebU32(c, @intCast(imports.len + c.wir_fun_by_tir.get(c.tir_fun_main.?).?.id));

        emitName(c, "memory");
        emitEnum(c, wasm.ExternalKind.memory);
        emitLebU32(c, 0);
    }

    if (c.constant_data.items.len > 0) {
        const section = emitSectionStart(c, wasm.Section.data_count);
        defer emitSectionEnd(c, section);

        emitLebU32(c, 1); // 1 data section
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
                emitEnum(c, wasmRepr(l.repr).primitive);
            }

            // Frame push
            if (f.shadow_offset_max > 0) {
                emitEnum(c, wasm.Opcode.global_get);
                emitLebU32(c, global_shadow);
                emitU32Const(c, @intCast(f.shadow_offset_max));
                emitEnum(c, wasm.Opcode.i32_sub);
                if (f.is_leaf) {
                    // Don't need to set global_shadow in leaf functions.
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
                emitU32Const(c, @intCast(f.shadow_offset_max));
                emitEnum(c, wasm.Opcode.i32_add);
                emitEnum(c, wasm.Opcode.global_set);
                emitLebU32(c, global_shadow);
            }

            emitEnum(c, wasm.Opcode.end);
        }
    }

    if (c.constant_data.items.len > 0) {
        const section = emitSectionStart(c, wasm.Section.data);
        defer emitSectionEnd(c, section);

        emitLebU32(c, 1); // 1 data section

        emitLebU32(c, 0); // active
        emitU32Const(c, stack_top); // write to memory after stack_top
        emitEnum(c, wasm.Opcode.end);
        emitLebU32(c, @intCast(c.constant_data.items.len));
        c.wasm.appendSlice(c.constant_data.items) catch oom();
    }
}

fn genFun(c: *Compiler, f: *wir.FunData) error{GenerateError}!void {
    const tir_f = c.tir_fun_data.get(f.tir_fun);

    // Prepare to track addresses for tir locals.
    assert(c.local_walue.count() == 0);
    defer c.local_walue.data.shrinkRetainingCapacity(0);
    c.local_walue.appendNTimes(null, tir_f.local_data.count());

    c.tir_expr_next.id = 0;
    _ = try genExprOrNull(c, f, tir_f, .nowhere);
}

fn genExpr(
    c: *Compiler,
    f: *wir.FunData,
    tir_f: tir.FunData,
    dest: wir.Destination,
) error{GenerateError}!wir.Walue {
    return (try genExprOrNull(c, f, tir_f, dest)).?;
}

fn genExprOrNull(
    c: *Compiler,
    f: *wir.FunData,
    tir_f: tir.FunData,
    dest: wir.Destination,
) error{GenerateError}!?wir.Walue {
    const result_maybe = try genExprInner(c, f, tir_f, dest);
    if (result_maybe) |result| {
        switch (dest) {
            .nowhere => {
                return dropStack(c, f, result);
            },
            .anywhere => {
                return result;
            },
            .stack => {
                const repr = walueRepr(c, f, result);
                switch (wasmRepr(repr)) {
                    .primitive => {
                        load(c, f, result);
                        return .{ .stack = repr };
                    },
                    .heap => {
                        loadPtrTo(c, f, result);
                        return .{ .value_at = .{
                            .ptr = c.box(wir.Walue{ .stack = .u32 }),
                            .repr = repr,
                        } };
                    },
                }
            },
            .value_at => |ptr| {
                store(c, f, result, ptr.*);
                return .{ .value_at = .{ .ptr = ptr, .repr = walueRepr(c, f, result) } };
            },
        }
    } else {
        return null;
    }
}

fn genExprInner(
    c: *Compiler,
    f: *wir.FunData,
    tir_f: tir.FunData,
    dest: wir.Destination,
) error{GenerateError}!?wir.Walue {
    const expr_data = take(c, tir_f);
    switch (expr_data) {
        .i64 => |i| {
            return .{ .i64 = i };
        },
        .string => |string| {
            return .{ .string = string };
        },
        .only => |value| {
            _ = try genExpr(c, f, tir_f, .nowhere);
            return .{ .only = .{ .value = value } };
        },
        .closure => {
            if (c.inlining) |inlining| {
                return inlining.closure;
            } else {
                return .{ .value_at = .{
                    .ptr = c.box(wir.Walue{ .closure = {} }),
                    .repr = tir_f.key.closure_repr,
                } };
            }
        },
        .arg => |arg| {
            if (c.inlining) |inlining| {
                return inlining.arg;
            } else {
                const repr = tir_f.key.arg_reprs[arg.id];
                return switch (wasmRepr(repr)) {
                    .primitive => .{ .arg = arg },
                    .heap => .{ .value_at = .{
                        .ptr = c.box(wir.Walue{ .arg = arg }),
                        .repr = repr,
                    } },
                };
            }
        },
        .local_get => |local| {
            const local_offset = if (c.inlining) |inlining| inlining.local_offset else 0;
            return c.local_walue.get(.{ .id = local.id + local_offset }).?;
        },
        .struct_init => |struct_repr| {
            // TODO Can pass dest if we do alias analysis.
            const child_dest: wir.Destination = if (struct_repr.keys.len <= 1) dest else if (dest == .nowhere) .nowhere else .anywhere;
            const values = c.allocator.alloc(wir.Walue, struct_repr.keys.len) catch oom();
            for (values) |*value| {
                value.* = spillStack(c, f, try genExpr(c, f, tir_f, child_dest));
            }
            return .{ .@"struct" = .{
                .repr = struct_repr,
                .values = values,
            } };
        },
        .union_init => |union_init| {
            const child_dest = switch (dest) {
                .value_at => |ptr| wir.Destination{
                    .value_at = c.box(wir.Walue{ .add = .{
                        .walue = ptr,
                        .offset = union_init.repr.tagSizeOf(),
                    } }),
                },
                else => dest,
            };
            const arg = try genExpr(c, f, tir_f, child_dest);
            return .{ .@"union" = .{
                .repr = union_init.repr,
                .tag = union_init.tag,
                .value = c.box(arg),
            } };
        },
        .local_let => |local| {
            var value = spillStack(c, f, try genExpr(c, f, tir_f, .anywhere));
            switch (value) {
                .value_at => |value_at| {
                    switch (wasmRepr(value_at.repr)) {
                        .heap => {},
                        .primitive => {
                            // We could leave this on the heap, but it's usually better to eagerly load the value so the wasm backend can see that it's constant. A wasted load if the local is never used though.
                            const wir_local = f.local_data.append(.{ .repr = value_at.repr });
                            const tmp = wir.Walue{ .local = wir_local };
                            load(c, f, value);
                            emitEnum(f, wasm.Opcode.local_set);
                            emitLebU32(f, wasmLocal(c, f, tmp));
                            value = tmp;
                        },
                    }
                },
                .@"struct", .fun => {
                    // If we later pass this struct/fun to a function call then we need to be able to point to it, so pessimistically store it on the stack.
                    if (!tir_f.local_data.get(local).is_tmp) {
                        const value_repr = walueRepr(c, f, value);
                        const ptr = copyToShadow(c, f, value, value_repr);
                        value = .{ .value_at = .{ .ptr = c.box(ptr), .repr = value_repr } };
                    }
                },
                else => {},
            }
            const local_offset = if (c.inlining) |inlining| inlining.local_offset else 0;
            c.local_walue.getPtr(.{ .id = local.id + local_offset }).* = value;
            return null;
        },
        .object_get => |object_get| {
            const object = try genExpr(c, f, tir_f, if (dest == .nowhere) .nowhere else .anywhere);
            return genObjectGet(c, f, object, object_get.index);
        },
        .ref_init => |repr| {
            const ref = shadowPush(c, f, repr);
            _ = try genExpr(c, f, tir_f, .{ .value_at = c.box(ref) });
            return ref;
        },
        .ref_get => |ref_get| {
            const ref = try genExpr(c, f, tir_f, if (dest == .nowhere) .nowhere else .anywhere);
            switch (ref_get) {
                .struct_offset => |offset| {
                    return .{ .add = .{
                        .walue = c.box(ref),
                        .offset = offset,
                    } };
                },
                .union_tag => |tag| {
                    const ref_spilled = spillStack(c, f, ref);
                    assertTag(c, f, ref_spilled, tag);
                    return .{
                        .add = .{
                            .walue = c.box(ref_spilled),
                            .offset = @sizeOf(u32), // TODO Use ReprUnion.tagSizeOf
                        },
                    };
                },
            }
        },
        .ref_set => {
            const ref = try genExpr(c, f, tir_f, .anywhere);
            _ = try genExpr(c, f, tir_f, .{ .value_at = c.box(ref) });
            return null;
        },
        .ref_deref => |repr| {
            const ref = try genExpr(c, f, tir_f, if (dest == .nowhere) .nowhere else .anywhere);
            const value = wir.Walue{ .value_at = .{ .ptr = c.box(ref), .repr = repr } };
            switch (dest) {
                .nowhere, .stack, .value_at => {
                    return value;
                },
                .anywhere => {
                    // Must make a copy to avoid passing around an aliased walue
                    return copy(c, f, value.value_at);
                },
            }
        },
        .call => |tir_fun| {
            f.is_leaf = false;
            const callee_tir_f = c.tir_fun_data.get(tir_fun);
            if (c.dir_fun_data.get(callee_tir_f.key.fun).@"inline") {
                const closure = spillStack(c, f, try genExpr(c, f, tir_f, .anywhere));
                const arg = spillStack(c, f, try genExpr(c, f, tir_f, .anywhere));
                return genInlineCall(c, f, callee_tir_f, dest, closure, arg);
            } else {
                if (callee_tir_f.key.closure_repr.isEmptyStruct()) {
                    _ = try genExpr(c, f, tir_f, .nowhere);
                } else {
                    const closure = try genExpr(c, f, tir_f, .anywhere);
                    loadPtrTo(c, f, closure);
                }
                for (0..callee_tir_f.key.arg_reprs.len) |_| {
                    _ = try genExpr(c, f, tir_f, .stack);
                }
                const output_repr = callee_tir_f.return_repr.one;
                const output = switch (wasmRepr(output_repr)) {
                    .primitive => wir.Walue{ .stack = output_repr },
                    .heap => wir.Walue{ .value_at = .{
                        .ptr = c.box(if (dest == .value_at) dest.value_at.* else shadowPush(c, f, output_repr)),
                        .repr = output_repr,
                    } },
                };
                switch (wasmRepr(output_repr)) {
                    .primitive => {},
                    .heap => if (output_repr.sizeOf() > 0) loadPtrTo(c, f, output),
                }
                emitEnum(f, wasm.Opcode.call);
                emitLebU32(f, @intCast(imports.len + c.wir_fun_by_tir.get(tir_fun).?.id));
                return output;
            }
        },
        .call_builtin => |builtin| {
            if (dest == .nowhere and !builtin.hasSideEffects()) {
                for (0..builtin.argCount()) |_| {
                    _ = try genExpr(c, f, tir_f, .nowhere);
                }
                return switch (builtin) {
                    .add_u32, .subtract_u32, .multiply_u32, .remainder_u32, .clz_u32, .i64_to_u32 => .{ .u32 = 0 },
                    .equal_u32, .not_equal_u32, .less_than_u32, .less_than_or_equal_u32, .more_than_u32, .more_than_or_equal_u32, .equal_i64, .not_equal_i64, .less_than_i64, .less_than_or_equal_i64, .more_than_i64, .more_than_or_equal_i64, .negate_i64, .add_i64, .subtract_i64, .multiply_i64, .remainder_i64, .union_has_key => .{ .i64 = 0 },
                    .memory_size, .heap_start, .size_of, .bit_shift_left_u32 => .{ .u32 = 0 },
                    .memory_grow, .memory_fill, .memory_copy, .load, .store, .print_u32, .print_i64, .print_string, .panic => unreachable,
                };
            }
            switch (builtin) {
                .negate_i64 => {
                    emitEnum(f, wasm.Opcode.i64_const);
                    emitLebI64(f, 0);
                    _ = try genExpr(c, f, tir_f, .stack);
                    emitEnum(f, wasm.Opcode.i64_sub);
                    return .{ .stack = .i64 };
                },
                .store => {
                    const address = try genExpr(c, f, tir_f, .anywhere);
                    const value = try genExpr(c, f, tir_f, .anywhere);
                    store(c, f, value, address);
                    return wir.Walue.emptyStruct();
                },
                .union_has_key => |index| {
                    const object = try genExpr(c, f, tir_f, .anywhere);
                    switch (object) {
                        .@"union" => |@"union"| {
                            return .{ .i64 = if (@"union".tag == index) 1 else 0 };
                        },
                        .value_at => |value_at| {
                            load(c, f, .{ .value_at = .{ .ptr = value_at.ptr, .repr = .u32 } });
                            load(c, f, .{ .u32 = index });
                            emitEnum(f, wasm.Opcode.i32_eq);
                            emitEnum(f, wasm.Opcode.i64_extend_i32_u);
                            return .{ .stack = .i64 };
                        },
                        else => panic("Can't represent union with {}", .{object}),
                    }
                },
                .i64_to_u32 => {
                    const arg = try genExpr(c, f, tir_f, .anywhere);
                    if (arg == .i64) {
                        return .{ .u32 = @intCast(arg.i64) };
                    } else {
                        // TODO Check for under/overflow.
                        load(c, f, arg);
                        emitEnum(f, wasm.Opcode.i32_wrap_i64);
                        return .{ .stack = .u32 };
                    }
                },
                else => {},
            }
            for (0..builtin.argCount()) |_| {
                _ = try genExpr(c, f, tir_f, .stack);
            }
            switch (builtin) {
                .equal_u32 => {
                    emitEnum(f, wasm.Opcode.i32_eq);
                    emitEnum(f, wasm.Opcode.i64_extend_i32_u);
                    return .{ .stack = .i64 };
                },
                .equal_i64 => {
                    emitEnum(f, wasm.Opcode.i64_eq);
                    emitEnum(f, wasm.Opcode.i64_extend_i32_u);
                    return .{ .stack = .i64 };
                },
                .not_equal_u32 => {
                    emitEnum(f, wasm.Opcode.i32_ne);
                    emitEnum(f, wasm.Opcode.i64_extend_i32_u);
                    return .{ .stack = .i64 };
                },
                .not_equal_i64 => {
                    emitEnum(f, wasm.Opcode.i64_ne);
                    emitEnum(f, wasm.Opcode.i64_extend_i32_u);
                    return .{ .stack = .i64 };
                },
                .less_than_u32 => {
                    emitEnum(f, wasm.Opcode.i32_lt_u);
                    emitEnum(f, wasm.Opcode.i64_extend_i32_u);
                    return .{ .stack = .i64 };
                },
                .less_than_i64 => {
                    emitEnum(f, wasm.Opcode.i64_lt_s);
                    emitEnum(f, wasm.Opcode.i64_extend_i32_u);
                    return .{ .stack = .i64 };
                },
                .less_than_or_equal_u32 => {
                    emitEnum(f, wasm.Opcode.i32_le_u);
                    emitEnum(f, wasm.Opcode.i64_extend_i32_u);
                    return .{ .stack = .i64 };
                },
                .less_than_or_equal_i64 => {
                    emitEnum(f, wasm.Opcode.i64_le_s);
                    emitEnum(f, wasm.Opcode.i64_extend_i32_u);
                    return .{ .stack = .i64 };
                },
                .more_than_u32 => {
                    emitEnum(f, wasm.Opcode.i32_gt_u);
                    emitEnum(f, wasm.Opcode.i64_extend_i32_u);
                    return .{ .stack = .i64 };
                },
                .more_than_i64 => {
                    emitEnum(f, wasm.Opcode.i64_gt_s);
                    emitEnum(f, wasm.Opcode.i64_extend_i32_u);
                    return .{ .stack = .i64 };
                },
                .more_than_or_equal_u32 => {
                    emitEnum(f, wasm.Opcode.i32_ge_u);
                    emitEnum(f, wasm.Opcode.i64_extend_i32_u);
                    return .{ .stack = .i64 };
                },
                .more_than_or_equal_i64 => {
                    emitEnum(f, wasm.Opcode.i64_ge_s);
                    emitEnum(f, wasm.Opcode.i64_extend_i32_u);
                    return .{ .stack = .i64 };
                },
                .add_u32 => {
                    emitEnum(f, wasm.Opcode.i32_add);
                    return .{ .stack = .u32 };
                },
                .add_i64 => {
                    emitEnum(f, wasm.Opcode.i64_add);
                    return .{ .stack = .i64 };
                },
                .subtract_u32 => {
                    emitEnum(f, wasm.Opcode.i32_sub);
                    return .{ .stack = .u32 };
                },
                .subtract_i64 => {
                    emitEnum(f, wasm.Opcode.i64_sub);
                    return .{ .stack = .i64 };
                },
                .multiply_u32 => {
                    emitEnum(f, wasm.Opcode.i32_mul);
                    return .{ .stack = .u32 };
                },
                .multiply_i64 => {
                    emitEnum(f, wasm.Opcode.i64_mul);
                    return .{ .stack = .i64 };
                },
                .remainder_u32 => {
                    emitEnum(f, wasm.Opcode.i32_rem_u);
                    return .{ .stack = .u32 };
                },
                .remainder_i64 => {
                    emitEnum(f, wasm.Opcode.i64_rem_s);
                    return .{ .stack = .i64 };
                },
                .bit_shift_left_u32 => {
                    emitEnum(f, wasm.Opcode.i32_shl);
                    return .{ .stack = .u32 };
                },
                .clz_u32 => {
                    emitEnum(f, wasm.Opcode.i32_clz);
                    return .{ .stack = .u32 };
                },
                .memory_size => {
                    emitEnum(f, wasm.Opcode.memory_size);
                    emitLebU32(f, 0); // memory
                    return .{ .stack = .u32 };
                },
                .memory_grow => {
                    emitEnum(f, wasm.Opcode.memory_grow);
                    emitLebU32(f, 0); // memory
                    return .{ .stack = .u32 };
                },
                .memory_fill => {
                    emitEnum(f, wasm.Opcode.misc_prefix);
                    emitLebU32(f, wasm.miscOpcode(wasm.MiscOpcode.memory_fill));
                    emitLebU32(f, 0); // memory
                    return wir.Walue.emptyStruct();
                },
                .memory_copy => {
                    emitEnum(f, wasm.Opcode.misc_prefix);
                    emitLebU32(f, wasm.miscOpcode(wasm.MiscOpcode.memory_copy));
                    emitLebU32(f, 0); // memory from
                    emitLebU32(f, 0); // memory to
                    return wir.Walue.emptyStruct();
                },
                .heap_start => {
                    // TODO Could add heap_start to Walue to avoid an extra local here.
                    emitEnum(f, wasm.Opcode.global_get);
                    emitLebU32(f, global_heap_start);
                    return .{ .stack = .u32 };
                },
                .size_of => |size_of| {
                    return .{ .u32 = size_of };
                },
                .load => |repr| {
                    const value = wir.Walue{ .value_at = .{
                        .ptr = c.box(wir.Walue{ .stack = .u32 }),
                        .repr = repr,
                    } };
                    switch (dest) {
                        .nowhere, .stack, .value_at => {
                            return value;
                        },
                        .anywhere => {
                            // Must make a copy to avoid passing around an aliased walue
                            return copy(c, f, value.value_at);
                        },
                    }
                },
                .print_u32 => {
                    emitEnum(f, wasm.Opcode.call);
                    const import_ix: usize = 0;
                    assert(std.mem.eql(u8, imports[import_ix].name, "print_u32"));
                    emitLebU32(f, import_ix);
                    return wir.Walue.emptyStruct();
                },
                .print_i64 => {
                    emitEnum(f, wasm.Opcode.call);
                    const import_ix: usize = 1;
                    assert(std.mem.eql(u8, imports[import_ix].name, "print_i64"));
                    emitLebU32(f, import_ix);
                    return wir.Walue.emptyStruct();
                },
                .print_string => {
                    // TODO Codegen is poor because we don't have a Walue.string_innards or WasmRepr.primitives yet.
                    var ptr_ptr = spillStack(c, f, wir.Walue{ .stack = .u32 });
                    var len_ptr = wir.Walue{ .add = .{ .walue = &ptr_ptr, .offset = @sizeOf(u32) } };
                    load(c, f, .{ .value_at = .{ .ptr = &ptr_ptr, .repr = .u32 } });
                    load(c, f, .{ .value_at = .{ .ptr = &len_ptr, .repr = .u32 } });
                    emitEnum(f, wasm.Opcode.call);
                    const import_ix: usize = 2;
                    assert(std.mem.eql(u8, imports[import_ix].name, "print_string"));
                    emitLebU32(f, import_ix);
                    return wir.Walue.emptyStruct();
                },
                .panic => {
                    emitEnum(f, wasm.Opcode.@"unreachable");
                    return wir.Walue.emptyUnion();
                },
                .negate_i64, .store, .union_has_key, .i64_to_u32 => unreachable, // handled above
            }
        },
        .each_struct => |callee_tir_funs| {
            const @"struct" = spillStack(c, f, try genExpr(c, f, tir_f, .anywhere));
            const closure = spillStack(c, f, try genExpr(c, f, tir_f, .anywhere));
            const struct_repr = walueRepr(c, f, @"struct").@"struct";
            for (0.., struct_repr.keys, callee_tir_funs) |index, key, callee_tir_fun| {
                const callee_tir_f = c.tir_fun_data.get(callee_tir_fun);
                const val = spillStack(c, f, genObjectGet(c, f, @"struct", index));
                const arg = wir.Walue{ .@"struct" = .{
                    .repr = callee_tir_f.key.arg_reprs[0].@"struct",
                    .values = c.dupe(wir.Walue, &.{ .{ .only = .{ .value = c.box(key) } }, val }),
                } };
                _ = try genInlineCall(c, f, callee_tir_f, .nowhere, closure, arg);
            }
            return wir.Walue.emptyStruct();
        },
        .from_only => |value| {
            _ = try genExpr(c, f, tir_f, .nowhere);
            switch (value.*) {
                .i64 => |i| return .{ .i64 = i },
                else => return fail(c, .todo),
            }
        },
        .block => |block| {
            // TODO reset shadow
            if (block.count == 0) return wir.Walue.emptyStruct();
            for (0..block.count - 1) |_| {
                _ = try genExprOrNull(c, f, tir_f, .nowhere);
            }
            const value = try genExprOrNull(c, f, tir_f, dest);
            return value orelse wir.Walue.emptyStruct();
        },
        .@"if" => |repr| {
            const cond = try genExpr(c, f, tir_f, .anywhere);

            if (cond == .i64 or cond == .only) {
                if ((cond == .i64 and cond.i64 == 0) or (cond == .only and cond.only.value.*.i64 == 0)) {
                    skipTree(c, tir_f);
                    const value = try genExpr(c, f, tir_f, dest);
                    return value;
                } else {
                    const value = try genExpr(c, f, tir_f, dest);
                    skipTree(c, tir_f);
                    return value;
                }
            }

            const branch_dest: wir.Destination = if (dest != .anywhere) dest else
            // Need to pick a specific dest so that both branches end up in the same place.
            switch (wasmRepr(repr)) {
                .primitive => .stack,
                .heap => .{ .value_at = c.box(shadowPush(c, f, repr)) },
            };

            load(c, f, cond);
            emitEnum(f, wasm.Opcode.i32_wrap_i64);
            emitEnum(f, wasm.Opcode.@"if");
            switch (branch_dest) {
                .nowhere, .value_at => {
                    emitByte(f, wasm.block_empty);
                },
                .stack => {
                    emitEnum(f, wasmRepr(repr).primitive);
                },
                .anywhere => unreachable,
            }
            const then = try genExpr(c, f, tir_f, branch_dest);
            emitEnum(f, wasm.Opcode.@"else");
            const @"else" = try genExpr(c, f, tir_f, branch_dest);
            emitEnum(f, wasm.Opcode.end);

            if (branch_dest != .nowhere)
                assert(then.equal(@"else"));

            return then;
        },
        .@"while" => {
            emitEnum(f, wasm.Opcode.block);
            emitByte(f, wasm.block_empty);

            emitEnum(f, wasm.Opcode.loop);
            emitByte(f, wasm.block_empty);

            const cond = try genExpr(c, f, tir_f, .anywhere);
            if ((cond == .i64 and cond.i64 == 0) or (cond == .only and cond.only.value.*.i64 == 0)) {
                skipTree(c, tir_f);
                emitEnum(f, wasm.Opcode.end);
                emitEnum(f, wasm.Opcode.end);
                return wir.Walue.emptyStruct();
            } else if (cond == .i64 and cond.i64 == 1) {
                // Don't bother with branch.
            } else {
                load(c, f, cond);
                emitEnum(f, wasm.Opcode.i64_eqz);
                emitEnum(f, wasm.Opcode.br_if);
                emitLebU32(f, 1);
            }

            _ = try genExpr(c, f, tir_f, .nowhere);
            emitEnum(f, wasm.Opcode.br);
            emitLebU32(f, 0);
            emitEnum(f, wasm.Opcode.end);
            emitEnum(f, wasm.Opcode.end);

            return wir.Walue.emptyStruct();
        },
        .@"return" => {
            if (c.inlining) |_| {
                const result = try genExpr(c, f, tir_f, .anywhere);
                return result;
            } else {
                const result_dest: wir.Destination = switch (wasmRepr(tir_f.return_repr.one)) {
                    .primitive => .stack,
                    .heap => .{ .value_at = c.box(wir.Walue{ .@"return" = {} }) },
                };
                _ = try genExpr(c, f, tir_f, result_dest);
                return null;
            }
        },
        else => {
            return fail(c, .todo);
        },
    }
}

// TODO This is a hack - should move inlining into a separate pass.
fn genInlineCall(
    c: *Compiler,
    f: *wir.FunData,
    callee_tir_f: tir.FunData,
    dest: wir.Destination,
    closure: wir.Walue,
    arg: wir.Walue,
) error{GenerateError}!?wir.Walue {
    assert(callee_tir_f.key.arg_reprs.len == 1);
    assert(c.dir_fun_data.get(callee_tir_f.key.fun).@"inline");

    const inlining = c.inlining;
    c.inlining = .{
        .closure = closure,
        .arg = arg,
        .local_offset = c.local_walue.count(),
    };
    defer c.inlining = inlining;

    c.local_walue.appendNTimes(null, callee_tir_f.local_data.count());
    defer c.local_walue.data.shrinkRetainingCapacity(c.inlining.?.local_offset);

    const expr_next = c.tir_expr_next;
    c.tir_expr_next = .{ .id = 0 };
    defer c.tir_expr_next = expr_next;

    const result = try genExpr(c, f, callee_tir_f, dest);
    return result;
}

fn genObjectGet(
    c: *Compiler,
    f: *wir.FunData,
    object: wir.Walue,
    index: usize,
) wir.Walue {
    return switch (object) {
        .value_at => |value_at| {
            switch (value_at.repr) {
                .@"struct" => |@"struct"| {
                    const offset = @"struct".offsetOf(index);
                    const repr = @"struct".reprs[index];
                    return .{ .value_at = .{
                        .ptr = c.box(wir.Walue{ .add = .{
                            .walue = value_at.ptr,
                            .offset = @intCast(offset),
                        } }),
                        .repr = repr,
                    } };
                },
                .@"union" => |@"union"| {
                    const ptr_spilled = spillStack(c, f, value_at.ptr.*);
                    assertTag(c, f, ptr_spilled, @intCast(index));
                    const offset = @"union".tagSizeOf();
                    const repr = @"union".reprs[index];
                    return .{ .value_at = .{
                        .ptr = c.box(wir.Walue{ .add = .{
                            .walue = c.box(ptr_spilled),
                            .offset = @intCast(offset),
                        } }),
                        .repr = repr,
                    } };
                },
                else => panic("Can't represent object with {}", .{object}),
            }
        },
        .@"struct" => |@"struct"| {
            return @"struct".values[index];
        },
        .@"union" => |@"union"| {
            if (@"union".tag != null and @"union".tag.? == index) {
                return @"union".value.?.*;
            } else {
                emitEnum(f, wasm.Opcode.@"unreachable");
                // TODO Is this typesafe? Do we need a poison Walue?
                return .{ .value_at = .{
                    .ptr = c.box(wir.Walue{ .u32 = 0 }),
                    .repr = walueRepr(c, f, object).@"union".reprs[index],
                } };
            }
        },
        else => panic("Can't represent object with {}", .{object}),
    };
}

fn assertTag(c: *Compiler, f: *wir.FunData, ptr: wir.Walue, index: u32) void {
    assert(ptr != .stack);
    emitEnum(f, wasm.Opcode.block);
    emitByte(f, wasm.block_empty);
    load(c, f, .{ .value_at = .{ .ptr = c.box(ptr), .repr = .u32 } });
    load(c, f, .{ .u32 = index });
    emitEnum(f, wasm.Opcode.i32_eq);
    emitEnum(f, wasm.Opcode.br_if);
    emitLebU32(f, 0);
    emitEnum(f, wasm.Opcode.@"unreachable");
    emitEnum(f, wasm.Opcode.end);
}

fn shadowPush(c: *Compiler, f: *wir.FunData, repr: Repr) wir.Walue {
    const offset = f.shadow_offset_next;
    f.shadow_offset_next += repr.sizeOf();
    if (f.shadow_offset_next > 0 and f.local_shadow == null)
        f.local_shadow = f.local_data.append(.{ .repr = .u32 });
    f.shadow_offset_max = @max(f.shadow_offset_max, f.shadow_offset_next);
    return .{ .add = .{
        .walue = c.box(wir.Walue{ .shadow = {} }),
        .offset = @intCast(offset),
    } };
}

fn asAdd(c: *Compiler, walue: wir.Walue) std.meta.FieldType(wir.Walue, .add) {
    var remaining = walue;
    var offset: u32 = 0;
    while (remaining == .add) {
        offset += remaining.add.offset;
        remaining = remaining.add.walue.*;
    }
    return .{
        .walue = c.box(remaining),
        .offset = offset,
    };
}

fn store(c: *Compiler, f: *wir.FunData, from_value: wir.Walue, to_ptr: wir.Walue) void {
    switch (from_value) {
        .closure, .arg, .@"return", .local, .shadow => {
            storePrimitive(c, f, from_value, to_ptr, wasmAbi(walueRepr(c, f, from_value)));
        },
        .stack => |repr| {
            const local = getShuffler(f, repr);
            emitEnum(f, wasm.Opcode.local_set);
            emitLebU32(f, wasmLocal(c, f, .{ .local = local }));
            storePrimitive(c, f, .{ .local = local }, to_ptr, wasmAbi(repr));
        },
        .u32, .add => {
            storePrimitive(c, f, from_value, to_ptr, .i32);
        },
        .i64 => {
            storePrimitive(c, f, from_value, to_ptr, .i64);
        },
        .string => |string| {
            const ptr = ptrToConstant(c, .{ .string = string });
            var values = [_]wir.Walue{ .{ .u32 = ptr }, .{ .u32 = @intCast(string.len) } };
            const innards = wir.Walue{ .@"struct" = .{
                .repr = c.string_innards,
                .values = &values,
            } };
            store(c, f, innards, to_ptr);
        },
        .@"struct" => |@"struct"| {
            const to_ptr_spilled = spillStack(c, f, to_ptr);
            for (@"struct".values, 0..) |value, i| {
                const offset = @"struct".repr.offsetOf(i);
                const to_field_ptr = wir.Walue{ .add = .{
                    .walue = c.box(to_ptr_spilled),
                    .offset = @intCast(offset),
                } };
                store(c, f, value, to_field_ptr);
            }
        },
        .@"union" => |@"union"| {
            if (@"union".tag == null) return;
            storePrimitive(c, f, .{ .u32 = @"union".tag.? }, to_ptr, .i32);
            store(c, f, @"union".value.?.*, .{ .add = .{ .walue = c.box(to_ptr), .offset = walueRepr(c, f, from_value).@"union".tagSizeOf() } });
        },
        .fun => |fun| {
            store(c, f, fun.closure.*, to_ptr);
        },
        .only => {
            // Zero-sized, nothing to do
        },
        .value_at => |value_at| {
            const from_ptr = value_at.ptr.*;

            if (from_ptr.equal(to_ptr))
                return;

            const from_ptr_spilled = spillStack(c, f, from_ptr);

            const byte_count = value_at.repr.sizeOf();
            if (byte_count > 64) {
                load(c, f, to_ptr);
                load(c, f, from_ptr_spilled);
                emitU32Const(f, @intCast(byte_count));
                emitEnum(f, wasm.Opcode.misc_prefix);
                emitLebU32(f, wasm.miscOpcode(wasm.MiscOpcode.memory_copy));
                emitLebU32(f, 0); // memory from
                emitLebU32(f, 0); // memory to
            } else {
                var offset: u32 = 0;
                const to_add = asAdd(c, to_ptr);
                const from_add = asAdd(c, from_ptr_spilled);
                while (offset < byte_count) {
                    const remaining = byte_count - offset;
                    inline for (.{
                        .{ 8, wasm.Opcode.i64_load, wasm.Opcode.i64_store },
                        .{ 4, wasm.Opcode.i32_load, wasm.Opcode.i32_store },
                        .{ 2, wasm.Opcode.i32_load16_u, wasm.Opcode.i32_store16 },
                        .{ 1, wasm.Opcode.i32_load8_u, wasm.Opcode.i32_store8 },
                    }) |params| {
                        const chunk, const load_op, const store_op = params;
                        if (remaining >= chunk) {
                            load(c, f, to_add.walue.*);
                            load(c, f, from_add.walue.*);
                            emitEnum(f, load_op);
                            emitLebU32(f, 0); // align
                            emitLebU32(f, from_add.offset + offset);
                            emitEnum(f, store_op);
                            emitLebU32(f, 0); // align
                            emitLebU32(f, to_add.offset + offset);
                            offset += chunk;
                            break;
                        }
                    } else unreachable;
                }
            }
        },
    }
}

fn storePrimitive(c: *Compiler, f: *wir.FunData, from_value: wir.Walue, to_ptr: wir.Walue, valtype: wasm.Valtype) void {
    assert(from_value != .stack);
    const to_add = asAdd(c, to_ptr);
    load(c, f, to_add.walue.*);
    load(c, f, from_value);
    emitEnum(f, switch (valtype) {
        .i32 => wasm.Opcode.i32_store,
        .i64 => wasm.Opcode.i64_store,
        else => panic("Unimplemented", .{}),
    });
    emitLebU32(f, 0); // align
    emitLebU32(f, to_add.offset);
}

fn load(c: *Compiler, f: *wir.FunData, from_value: wir.Walue) void {
    switch (from_value) {
        .closure, .arg, .@"return", .local, .shadow => {
            emitEnum(f, wasm.Opcode.local_get);
            emitLebU32(f, wasmLocal(c, f, from_value));
        },
        .stack => {},
        .u32 => |i| {
            emitU32Const(f, i);
        },
        .i64 => |i| {
            emitEnum(f, wasm.Opcode.i64_const);
            emitLebI64(f, i);
        },
        .string, .@"struct", .@"union", .fun, .only => panic("Can't load from {}", .{from_value}),
        .value_at => |value_at| {
            const from_add = asAdd(c, value_at.ptr.*);
            load(c, f, from_add.walue.*);
            emitEnum(f, switch (value_at.repr) {
                .u32, .ref => wasm.Opcode.i32_load,
                .i64 => wasm.Opcode.i64_load,
                else => panic("Can't load repr {}", .{value_at.repr}),
            });
            emitLebU32(f, 0); // align
            emitLebU32(f, from_add.offset);
        },
        .add => |add| {
            const from_add = asAdd(c, from_value);
            load(c, f, from_add.walue.*);
            if (add.offset != 0) {
                emitU32Const(f, add.offset);
                emitEnum(f, wasm.Opcode.i32_add);
            }
        },
    }
}

fn loadPtrTo(c: *Compiler, f: *wir.FunData, from_value: wir.Walue) void {
    switch (from_value) {
        .closure, .arg, .@"return", .local, .shadow => {
            panic("Can't point to local: {}", .{from_value});
        },
        .stack, .u32, .i64, .string, .@"struct", .@"union", .fun, .only => {
            const repr = walueRepr(c, f, from_value);
            const ptr = copyToShadow(c, f, from_value, repr);
            load(c, f, ptr);
        },
        .value_at => |value_at| {
            if (value_at.repr.sizeOf() == 0) {
                emitU32Const(f, 0);
            } else {
                load(c, f, value_at.ptr.*);
            }
        },
        .add => panic("Unimplemented", .{}),
    }
}

fn copy(c: *Compiler, f: *wir.FunData, value_at: std.meta.FieldType(wir.Walue, .value_at)) wir.Walue {
    switch (wasmRepr(value_at.repr)) {
        .primitive => {
            load(c, f, .{ .value_at = value_at });
            return .{ .stack = value_at.repr };
        },
        .heap => {
            const ptr = copyToShadow(c, f, .{ .value_at = value_at }, value_at.repr);
            return .{ .value_at = .{ .ptr = c.box(ptr), .repr = value_at.repr } };
        },
    }
}

fn copyToShadow(c: *Compiler, f: *wir.FunData, value: wir.Walue, repr: Repr) wir.Walue {
    if (repr.sizeOf() == 0) {
        return .{ .u32 = 0 };
    } else {
        const tmp = shadowPush(c, f, repr);
        store(c, f, value, tmp);
        return tmp;
    }
}

fn getShuffler(f: *wir.FunData, repr: Repr) wir.Local {
    const valtype = wasmRepr(repr).primitive;
    const shuffler = f.local_shufflers.getPtr(valtype);
    if (shuffler.* == null) shuffler.* = f.local_data.append(.{ .repr = repr });
    return shuffler.*.?;
}

fn dropStack(c: *Compiler, f: *wir.FunData, walue: wir.Walue) wir.Walue {
    switch (walue) {
        .closure, .arg, .@"return", .local, .shadow, .u32, .i64, .string, .@"struct", .@"union", .fun, .only => return walue,
        .stack => |repr| {
            emitEnum(f, wasm.Opcode.drop);
            return switch (repr) {
                .u32, .ref => .{ .u32 = 0 },
                .i64 => .{ .i64 = 0 },
                else => panic("TODO {}", .{repr}),
            };
        },
        .value_at => |value_at| {
            return .{ .value_at = .{
                .ptr = c.box(dropStack(c, f, value_at.ptr.*)),
                .repr = value_at.repr,
            } };
        },
        .add => |add| {
            return .{ .add = .{
                .walue = c.box(dropStack(c, f, add.walue.*)),
                .offset = add.offset,
            } };
        },
    }
}

fn spillStack(c: *Compiler, f: *wir.FunData, walue: wir.Walue) wir.Walue {
    switch (walue) {
        .closure, .arg, .@"return", .local, .shadow, .u32, .i64, .string, .only => {
            return walue;
        },
        .stack => |repr| {
            const local = f.local_data.append(.{ .repr = repr });
            emitEnum(f, wasm.Opcode.local_set);
            emitLebU32(f, wasmLocal(c, f, .{ .local = local }));
            return .{ .local = local };
        },
        .@"struct",
        .@"union",
        .fun,
        => {
            // Not allowed to contain .stack
            return walue;
        },
        .value_at => |value_at| {
            return .{ .value_at = .{
                .ptr = c.box(spillStack(c, f, value_at.ptr.*)),
                .repr = value_at.repr,
            } };
        },
        .add => |add| {
            return .{ .add = .{
                .walue = c.box(spillStack(c, f, add.walue.*)),
                .offset = add.offset,
            } };
        },
    }
}

fn memoFunType(c: *Compiler, fun_type_data: wir.FunTypeData) wir.FunType {
    if (c.fun_type_memo.get(fun_type_data)) |fun_type| {
        return fun_type;
    } else {
        const fun_type = c.fun_type_data.append(fun_type_data);
        c.fun_type_memo.put(fun_type_data, fun_type) catch oom();
        return fun_type;
    }
}

fn ptrToConstant(c: *Compiler, value: Value) u32 {
    if (c.constant_memo.get(value)) |offset| {
        return stack_top + offset;
    } else {
        const offset = @as(u32, @intCast(c.constant_data.items.len));
        switch (value) {
            .string => |string| c.constant_data.appendSlice(string) catch oom(),
            else => panic("TODO {}", .{value}),
        }
        c.constant_memo.put(value, offset) catch oom();
        return stack_top + offset;
    }
}

const WasmRepr = union(enum) {
    primitive: wasm.Valtype,
    heap,
};

fn wasmRepr(repr: Repr) WasmRepr {
    return switch (repr) {
        .u32, .i64, .ref => .{ .primitive = wasmAbi(repr) },
        .string, .@"struct", .@"union", .fun, .only, .repr, .repr_kind => .heap,
    };
}

fn wasmAbi(repr: Repr) wasm.Valtype {
    return switch (repr) {
        .u32 => .i32,
        .i64 => .i64,
        // Pointers.
        .string, .@"struct", .@"union", .fun, .only, .ref, .repr, .repr_kind => .i32,
    };
}

fn wasmLocal(c: *Compiler, f: *const wir.FunData, walue: wir.Walue) u32 {
    return switch (walue) {
        .closure => 0,
        .arg => |arg| @intCast(arg.id + @as(usize, if (c.tir_fun_data.get(f.tir_fun).key.closure_repr.isEmptyStruct()) 0 else 1)),
        .@"return" => @intCast(c.fun_type_data.get(f.fun_type).arg_types.len - 1),
        .local => |local| @intCast(c.fun_type_data.get(f.fun_type).arg_types.len + local.id),
        .shadow => @intCast(c.fun_type_data.get(f.fun_type).arg_types.len + f.local_shadow.?.id),
        .stack, .u32, .i64, .string, .@"struct", .@"union", .fun, .only, .value_at, .add => panic("Not a local: {}", .{walue}),
    };
}

fn walueRepr(c: *Compiler, f: *const wir.FunData, walue: wir.Walue) Repr {
    return switch (walue) {
        .arg => |arg| c.tir_fun_data.get(f.tir_fun).key.arg_reprs[arg.id],
        .closure, .@"return", .shadow, .u32, .add => .u32,
        .i64 => .i64,
        .string => .string,
        .stack => |repr| repr,
        .local => |local| f.local_data.get(local).repr,
        .@"struct" => |@"struct"| .{ .@"struct" = @"struct".repr },
        .@"union" => |@"union"| .{ .@"union" = @"union".repr },
        .fun => |fun| .{ .fun = fun.repr },
        .only => |only| .{ .only = only.value },
        .value_at => |value_at| value_at.repr,
    };
}

fn take(c: *Compiler, tir_f: tir.FunData) tir.ExprData {
    const expr_data = tir_f.expr_data_pre.get(c.tir_expr_next);
    c.tir_expr_next.id += 1;
    return expr_data;
}

fn skipTree(c: *Compiler, tir_f: tir.FunData) void {
    var children_remaining: usize = 1;
    while (children_remaining > 0) {
        children_remaining += take(c, tir_f).childCount(c);
        children_remaining -= 1;
    }
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

fn emitU32Const(c: anytype, i: u32) void {
    emitEnum(c, wasm.Opcode.i32_const);
    emitLebI(c, @as(i32, @bitCast(i)));
}

fn emitBytes(c: anytype, bs: []const u8) void {
    c.wasm.appendSlice(bs) catch oom();
}

/// Don't use this directly.
fn emitLebU(c: anytype, i: anytype) void {
    // https://webassembly.github.io/spec/core/binary/values.html#integers
    var n = i;
    while (true) {
        const chunk = @as(u8, @truncate(n & 0b0111_1111));
        n >>= 7;
        if (n == 0) {
            c.wasm.append(chunk) catch oom();
            break;
        } else {
            c.wasm.append(chunk | 0b1000_0000) catch oom();
        }
    }
}

/// Don't use this directly.
fn emitLebI(c: anytype, i: anytype) void {
    // https://webassembly.github.io/spec/core/binary/values.html#integers
    const U = std.meta.Int(.unsigned, @typeInfo(@TypeOf(i)).Int.bits);
    var n = i;
    while (true) {
        const chunk = @as(u8, @truncate(@as(U, @bitCast(n))));
        n >>= 6;
        if (n == 0 or n == -1) {
            c.wasm.append(chunk & 0b0111_1111) catch oom();
            break;
        } else {
            n >>= 1;
            c.wasm.append(chunk | 0b1000_0000) catch oom();
        }
    }
}

fn emitLebU32(c: anytype, i: u32) void {
    emitLebU(c, i);
}

fn emitLebI32(c: anytype, i: i32) void {
    emitLebI(c, i);
}

fn emitLebU64(c: anytype, i: u64) void {
    emitLebU(c, i);
}

fn emitLebI64(c: anytype, i: i64) void {
    emitLebI(c, i);
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
