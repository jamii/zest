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
        try genFun(c, .{ .id = tir_fun_id });
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

            emitEnum(c, wasm.Opcode.end);
        }
    }
}

fn genFun(c: *Compiler, fun: tir.Fun) error{GenerateError}!void {
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

    // Make wir_fun.
    _ = c.wir_fun_data.append(wir.FunData.init(c.allocator, fun_type.?));
    const f = c.wir_fun_data.getPtr(fun);

    // Map begin exprs to their end.
    assert(c.begin_stack.items.len == 0);
    defer assert(c.begin_stack.items.len == 0);
    assert(c.begin_end.count() == 0);
    defer c.begin_end.data.shrinkRetainingCapacity(0);
    c.begin_end.appendNTimes(.{ .id = 0 }, tir_f.expr_data.count());
    for (tir_f.expr_data.items(), 0..) |expr_data, expr_id| {
        const expr = tir.Expr{ .id = expr_id };
        switch (expr_data.treePart()) {
            .branch_begin => {
                c.begin_stack.append(.{ .id = expr_id }) catch oom();
            },
            .branch_end => {
                const begin = c.begin_stack.pop();
                c.begin_end.getPtr(begin).* = expr;
                c.begin_end.getPtr(expr).* = begin;
            },
            .leaf => {
                c.begin_end.getPtr(expr).* = expr;
            },
            .branch_begin_without_end => unreachable,
        }
    }

    // Prepare to track addresses for tir locals.
    assert(c.local_walue.count() == 0);
    defer c.local_walue.data.shrinkRetainingCapacity(0);
    c.local_walue.appendNTimes(null, tir_f.local_data.count());

    c.tir_expr_next.id = 0;
    assert(c.block_stack.items.len == 0);
    // TODO Uncomment asserts once todo is gone.
    //defer assert(c.block_stack.items.len == 0);

    while (c.tir_expr_next.id + 1 < tir_f.expr_data.count()) {
        _ = try genExprNextOrNull(c, f, tir_f, .nowhere);
    }
}

fn genExprNext(
    c: *Compiler,
    f: *wir.FunData,
    tir_f: tir.FunData,
    hint: wir.Hint,
) error{GenerateError}!wir.Walue {
    return (try genExprNextOrNull(c, f, tir_f, hint)).?;
}

fn genExprNextOrNull(
    c: *Compiler,
    f: *wir.FunData,
    tir_f: tir.FunData,
    hint: wir.Hint,
) error{GenerateError}!?wir.Walue {
    const expr = c.begin_end.get(c.tir_expr_next);
    c.tir_expr_next.id += 1;

    const expr_data = tir_f.expr_data.get(expr);
    const repr = tir_f.expr_repr.get(expr);

    const result_maybe = try genExpr(c, f, tir_f, hint, expr_data, repr);
    if (expr_data.treePart() == .branch_end)
        skipOverBegin(c, f, tir_f);

    if (result_maybe) |result| {
        switch (hint) {
            .nowhere => {
                return dropStack(c, f, result);
            },
            .anywhere => {
                return result;
            },
            .stack => {
                load(c, f, result);
                return .{ .stack = wasmRepr(walueRepr(c, f, result)).primitive };
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

fn genExpr(
    c: *Compiler,
    f: *wir.FunData,
    tir_f: tir.FunData,
    hint: wir.Hint,
    expr_data: tir.ExprData,
    repr: ?Repr,
) error{GenerateError}!?wir.Walue {
    switch (expr_data) {
        .i32 => |i| {
            return .{ .i32 = i };
        },
        .closure => {
            return .{ .value_at = .{
                .ptr = c.box(wir.Walue{ .closure = {} }),
                .repr = repr.?,
            } };
        },
        .arg => {
            return .{ .value_at = .{
                .ptr = c.box(wir.Walue{ .arg = {} }),
                .repr = repr.?,
            } };
        },
        .local_get => |local| {
            return c.local_walue.get(local).?;
        },
        .struct_init => {
            const struct_repr = repr.?.@"struct";
            const values = c.allocator.alloc(wir.Walue, struct_repr.reprs.len) catch oom();
            for (values) |*value| {
                // TODO Can pass hint if we do alias analysis.
                value.* = spillStack(c, f, try genExprNext(c, f, tir_f, if (hint == .nowhere) .nowhere else .anywhere));
            }
            return .{ .@"struct" = .{
                .repr = struct_repr,
                .values = values,
            } };
        },
        .fun_init => {
            // TODO Should we change the repr in hint.value_at?
            const closure = try genExprNext(c, f, tir_f, hint);
            return if (closure == .@"struct")
                .{ .fun = .{
                    .repr = repr.?.fun,
                    .closure = c.box(closure),
                } }
            else
                closure;
        },
        .local_let => |local| {
            var value = spillStack(c, f, try genExprNext(c, f, tir_f, .anywhere));
            switch (value) {
                .value_at => |value_at| {
                    switch (wasmRepr(value_at.repr)) {
                        .heap => {},
                        .primitive => |valtype| {
                            // We could leave this on the heap, but it's usually better to eagerly load the value so the wasm backend can see that it's constant. A wasted load if the local is never used though.
                            const wir_local = f.local_data.append(.{ .type = valtype });
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
            c.local_walue.getPtr(local).* = value;
            return null;
        },
        .object_get => |object_get| {
            const object = try genExprNext(c, f, tir_f, if (hint == .nowhere) .nowhere else .anywhere);
            switch (object) {
                .value_at => |value_at| {
                    return .{ .value_at = .{
                        .ptr = c.box(wir.Walue{ .add = .{
                            .walue = value_at.ptr,
                            .offset = object_get.offset,
                        } }),
                        .repr = repr.?,
                    } };
                },
                .@"struct" => |@"struct"| {
                    return @"struct".values[object_get.index];
                },
                else => panic("Can't represent struct with {}", .{object}),
            }
        },
        .ref_init => {
            const ref = shadowPush(c, f, repr.?.ref.*);
            _ = try genExprNext(c, f, tir_f, .{ .value_at = c.box(ref) });
            return ref;
        },
        .ref_get => |ref_get| {
            const ref = try genExprNext(c, f, tir_f, if (hint == .nowhere) .nowhere else .anywhere);
            return .{ .add = .{
                .walue = c.box(ref),
                .offset = ref_get.offset,
            } };
        },
        .ref_set => {
            const ref = try genExprNext(c, f, tir_f, .anywhere);
            _ = try genExprNext(c, f, tir_f, .{ .value_at = c.box(ref) });
            return null;
        },
        .ref_deref => {
            const ref = try genExprNext(c, f, tir_f, if (hint == .nowhere) .nowhere else .anywhere);
            const value = wir.Walue{ .value_at = .{ .ptr = c.box(ref), .repr = repr.? } };
            switch (hint) {
                .nowhere, .stack, .value_at => {
                    return value;
                },
                .anywhere => {
                    // Must make a copy to avoid passing around an aliased walue
                    return copy(c, f, value.value_at);
                },
            }
        },
        .call => |fun| {
            f.is_leaf = false;
            const closure = try genExprNext(c, f, tir_f, .anywhere);
            const arg = try genExprNext(c, f, tir_f, .anywhere);
            const output = switch (wasmRepr(repr.?)) {
                .primitive => |valtype| wir.Walue{ .stack = valtype },
                .heap => wir.Walue{ .value_at = .{
                    .ptr = c.box(if (hint == .value_at) hint.value_at.* else shadowPush(c, f, repr.?)),
                    .repr = repr.?,
                } },
            };
            loadPtrTo(c, f, arg);
            loadPtrTo(c, f, closure);
            switch (wasmRepr(repr.?)) {
                .primitive => {},
                .heap => loadPtrTo(c, f, output),
            }
            emitEnum(f, wasm.Opcode.call);
            emitLebU32(f, @intCast(fun.id));
            return output;
        },
        .call_builtin => |builtin| {
            if (hint == .nowhere) {
                _ = try genExprNext(c, f, tir_f, .nowhere);
                return .{ .i32 = 0 };
            }
            switch (builtin) {
                .equal_i32 => {
                    const args = try genExprNext(c, f, tir_f, .anywhere);
                    const arg0 = args.@"struct".values[0];
                    const arg1 = args.@"struct".values[1];
                    if (arg0 == .i32 and arg1 == .i32) {
                        return .{ .i32 = if (arg0.i32 == arg1.i32) 1 else 0 };
                    } else {
                        load(c, f, arg0);
                        load(c, f, arg1);
                        emitEnum(f, wasm.Opcode.i32_eq);
                        return .{ .stack = .i32 };
                    }
                },
                .less_than_i32 => {
                    const args = try genExprNext(c, f, tir_f, .anywhere);
                    const arg0 = args.@"struct".values[0];
                    const arg1 = args.@"struct".values[1];
                    if (arg0 == .i32 and arg1 == .i32) {
                        return .{ .i32 = if (arg0.i32 < arg1.i32) 1 else 0 };
                    } else {
                        load(c, f, arg0);
                        load(c, f, arg1);
                        emitEnum(f, wasm.Opcode.i32_lt_s);
                        return .{ .stack = .i32 };
                    }
                },
                .less_than_or_equal_i32 => {
                    const args = try genExprNext(c, f, tir_f, .anywhere);
                    const arg0 = args.@"struct".values[0];
                    const arg1 = args.@"struct".values[1];
                    if (arg0 == .i32 and arg1 == .i32) {
                        return .{ .i32 = if (arg0.i32 <= arg1.i32) 1 else 0 };
                    } else {
                        load(c, f, arg0);
                        load(c, f, arg1);
                        emitEnum(f, wasm.Opcode.i32_le_s);
                        return .{ .stack = .i32 };
                    }
                },
                .more_than_i32 => {
                    const args = try genExprNext(c, f, tir_f, .anywhere);
                    const arg0 = args.@"struct".values[0];
                    const arg1 = args.@"struct".values[1];
                    if (arg0 == .i32 and arg1 == .i32) {
                        return .{ .i32 = if (arg0.i32 > arg1.i32) 1 else 0 };
                    } else {
                        load(c, f, arg0);
                        load(c, f, arg1);
                        emitEnum(f, wasm.Opcode.i32_gt_s);
                        return .{ .stack = .i32 };
                    }
                },
                .more_than_or_equal_i32 => {
                    const args = try genExprNext(c, f, tir_f, .anywhere);
                    const arg0 = args.@"struct".values[0];
                    const arg1 = args.@"struct".values[1];
                    if (arg0 == .i32 and arg1 == .i32) {
                        return .{ .i32 = if (arg0.i32 >= arg1.i32) 1 else 0 };
                    } else {
                        load(c, f, arg0);
                        load(c, f, arg1);
                        emitEnum(f, wasm.Opcode.i32_ge_s);
                        return .{ .stack = .i32 };
                    }
                },
                .add_i32 => {
                    const args = try genExprNext(c, f, tir_f, .anywhere);
                    const arg0 = args.@"struct".values[0];
                    const arg1 = args.@"struct".values[1];
                    if (arg0 == .i32 and arg1 == .i32) {
                        return .{ .i32 = arg0.i32 + arg1.i32 };
                    } else {
                        load(c, f, arg0);
                        load(c, f, arg1);
                        emitEnum(f, wasm.Opcode.i32_add);
                        return .{ .stack = .i32 };
                    }
                },
                .subtract_i32 => {
                    const args = try genExprNext(c, f, tir_f, .anywhere);
                    const arg0 = args.@"struct".values[0];
                    const arg1 = args.@"struct".values[1];
                    if (arg0 == .i32 and arg1 == .i32) {
                        return .{ .i32 = arg0.i32 - arg1.i32 };
                    } else {
                        load(c, f, arg0);
                        load(c, f, arg1);
                        emitEnum(f, wasm.Opcode.i32_sub);
                        return .{ .stack = .i32 };
                    }
                },
                .multiply_i32 => {
                    const args = try genExprNext(c, f, tir_f, .anywhere);
                    const arg0 = args.@"struct".values[0];
                    const arg1 = args.@"struct".values[1];
                    if (arg0 == .i32 and arg1 == .i32) {
                        return .{ .i32 = arg0.i32 * arg1.i32 };
                    } else {
                        load(c, f, arg0);
                        load(c, f, arg1);
                        emitEnum(f, wasm.Opcode.i32_mul);
                        return .{ .stack = .i32 };
                    }
                },
            }
        },
        .block => |count| {
            // TODO reset shadow
            var value: ?wir.Walue = null;
            var remaining = count;
            while (tir_f.expr_data.get(c.begin_end.get(c.tir_expr_next)) != .begin) {
                value = try genExprNextOrNull(c, f, tir_f, if (remaining == 1) hint else .nowhere);
                if (value != null) remaining -= 1;
            }
            return value orelse wir.Walue.emptyStruct();
        },
        .@"return" => {
            const result_hint: wir.Hint = switch (wasmRepr(tir_f.return_repr.one)) {
                .primitive => .stack,
                .heap => .{ .value_at = c.box(wir.Walue{ .@"return" = {} }) },
            };
            _ = try genExprNext(c, f, tir_f, result_hint);
            return null;
        },
        .nop => {
            const value = try genExprNext(c, f, tir_f, hint);
            return value;
        },
        .if_end => {
            _ = try genExprNext(c, f, tir_f, .stack);

            const branch_hint: wir.Hint = if (hint != .anywhere) hint else
            // Need to pick a specific hint so that both branches end up the same.
            switch (wasmRepr(repr.?)) {
                .primitive => .stack,
                .heap => .{ .value_at = c.box(shadowPush(c, f, repr.?)) },
            };

            emitEnum(f, wasm.Opcode.@"if");
            switch (branch_hint) {
                .nowhere, .value_at => {
                    emitByte(f, wasm.block_empty);
                },
                .stack => {
                    emitEnum(f, wasmRepr(repr.?).primitive);
                },
                .anywhere => unreachable,
            }

            skipOver(c, f, tir_f, .if_then);
            const then = try genExprNext(c, f, tir_f, branch_hint);

            emitEnum(f, wasm.Opcode.@"else");

            skipOver(c, f, tir_f, .if_else);
            const @"else" = try genExprNext(c, f, tir_f, branch_hint);

            emitEnum(f, wasm.Opcode.end);

            if (branch_hint != .nowhere)
                assert(then.equal(@"else"));

            return then;
        },
        .while_end => {
            emitEnum(f, wasm.Opcode.block);
            emitByte(f, wasm.block_empty);

            emitEnum(f, wasm.Opcode.loop);
            emitByte(f, wasm.block_empty);

            _ = try genExprNext(c, f, tir_f, .stack);
            emitEnum(f, wasm.Opcode.i32_eqz);
            emitEnum(f, wasm.Opcode.br_if);
            emitLebU32(f, 1);

            skipOver(c, f, tir_f, .while_body);

            _ = try genExprNext(c, f, tir_f, .nowhere);
            emitEnum(f, wasm.Opcode.br);
            emitLebU32(f, 0);

            emitEnum(f, wasm.Opcode.end);
            emitEnum(f, wasm.Opcode.end);

            return null;
        },
        .begin, .if_then, .if_else, .if_begin, .while_body, .while_begin => {
            panic("This should have been skipped: {}", .{expr_data});
        },
        else => {
            std.debug.print("TODO generate {}\n", .{expr_data});
            return fail(c, .todo);
        },
    }
}

fn skipOver(
    c: *Compiler,
    f: *wir.FunData,
    tir_f: tir.FunData,
    expr_tag: std.meta.Tag(tir.ExprData),
) void {
    _ = f;

    const expr = c.begin_end.get(c.tir_expr_next);
    c.tir_expr_next.id += 1;

    const expr_data = tir_f.expr_data.get(expr);
    assert(std.meta.activeTag(expr_data) == expr_tag);
}

fn skipOverBegin(
    c: *Compiler,
    f: *wir.FunData,
    tir_f: tir.FunData,
) void {
    _ = f;

    const expr = c.begin_end.get(c.tir_expr_next);
    c.tir_expr_next.id += 1;

    const expr_data = tir_f.expr_data.get(expr);
    assert(expr_data.treePart() == .branch_begin);
}

fn shadowPush(c: *Compiler, f: *wir.FunData, repr: Repr) wir.Walue {
    const offset = f.shadow_offset_next;
    f.shadow_offset_next += repr.sizeOf();
    if (f.shadow_offset_next > 0 and f.local_shadow == null)
        f.local_shadow = f.local_data.append(.{ .type = .i32 });
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
        .closure, .arg, .@"return", .local, .shadow, .stack => {
            var from_local = from_value;
            const valtype = switch (from_local) {
                .closure, .arg, .@"return", .shadow => .i32,
                .local => |local| f.local_data.get(local).type,
                .stack => |valtype| valtype: {
                    from_local = .{ .local = getShuffler(f, valtype) };
                    emitEnum(f, wasm.Opcode.local_set);
                    emitLebU32(f, wasmLocal(c, f, from_local));
                    break :valtype valtype;
                },
                else => unreachable,
            };
            storePrimitive(c, f, from_local, to_ptr, valtype);
        },
        .i32, .add => {
            storePrimitive(c, f, from_value, to_ptr, .i32);
        },
        .@"struct" => |@"struct"| {
            for (@"struct".values, 0..) |value, i| {
                const offset = @"struct".repr.offsetOf(i);
                const to_field_ptr = wir.Walue{ .add = .{
                    .walue = c.box(to_ptr),
                    .offset = @intCast(offset),
                } };
                store(c, f, value, to_field_ptr);
            }
        },
        .fun => |fun| {
            store(c, f, fun.closure.*, to_ptr);
        },
        .value_at => |value_at| {
            const from_ptr = value_at.ptr.*;

            if (from_ptr.equal(to_ptr))
                return;

            const byte_count = value_at.repr.sizeOf();
            switch (byte_count) {
                0 => {},
                4 => {
                    storePrimitive(c, f, .{ .value_at = .{ .ptr = value_at.ptr, .repr = .i32 } }, to_ptr, .i32);
                },
                else => {
                    load(c, f, to_ptr);
                    load(c, f, from_ptr);
                    emitEnum(f, wasm.Opcode.i32_const);
                    emitLebU32(f, @intCast(byte_count));
                    emitEnum(f, wasm.Opcode.misc_prefix);
                    emitLebU32(f, wasm.miscOpcode(wasm.MiscOpcode.memory_copy));
                    emitLebU32(f, 0); // memory from
                    emitLebU32(f, 0); // memory to
                },
            }
        },
    }
}

fn storePrimitive(c: *Compiler, f: *wir.FunData, from_value: wir.Walue, to_ptr: wir.Walue, valtype: wasm.Valtype) void {
    const to_add = asAdd(c, to_ptr);
    load(c, f, to_add.walue.*);
    load(c, f, from_value);
    switch (valtype) {
        .i32 => {
            emitEnum(f, wasm.Opcode.i32_store);
            emitLebU32(f, 0); // align
            emitLebU32(f, to_add.offset);
        },
        else => panic("Unimplemented", .{}),
    }
}

fn load(c: *Compiler, f: *wir.FunData, from_value: wir.Walue) void {
    switch (from_value) {
        .closure, .arg, .@"return", .local, .shadow => {
            emitEnum(f, wasm.Opcode.local_get);
            emitLebU32(f, wasmLocal(c, f, from_value));
        },
        .stack => {},
        .i32 => |i| {
            emitEnum(f, wasm.Opcode.i32_const);
            emitLebI32(f, i);
        },
        .@"struct", .fun => panic("Can't load from {}", .{from_value}),
        .value_at => |value_at| {
            const from_add = asAdd(c, value_at.ptr.*);
            load(c, f, from_add.walue.*);
            switch (value_at.repr) {
                .i32, .ref => {
                    emitEnum(f, wasm.Opcode.i32_load);
                    emitLebU32(f, 0); // align
                    emitLebU32(f, from_add.offset);
                },
                else => panic("Can't load repr {}", .{value_at.repr}),
            }
        },
        .add => |add| {
            const from_add = asAdd(c, from_value);
            load(c, f, from_add.walue.*);
            if (add.offset != 0) {
                emitEnum(f, wasm.Opcode.i32_const);
                emitLebU32(f, add.offset);
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
        .stack, .i32, .@"struct", .fun => {
            const repr = walueRepr(c, f, from_value);
            const ptr = copyToShadow(c, f, from_value, repr);
            load(c, f, ptr);
        },
        .value_at => |value_at| {
            if (value_at.repr.sizeOf() == 0) {
                emitEnum(f, wasm.Opcode.i32_const);
                emitLebU32(f, 0);
            } else {
                load(c, f, value_at.ptr.*);
            }
        },
        .add => panic("Unimplemented", .{}),
    }
}

fn copy(c: *Compiler, f: *wir.FunData, value_at: std.meta.FieldType(wir.Walue, .value_at)) wir.Walue {
    switch (wasmRepr(value_at.repr)) {
        .primitive => |valtype| {
            load(c, f, .{ .value_at = value_at });
            return .{ .stack = valtype };
        },
        .heap => {
            const ptr = copyToShadow(c, f, .{ .value_at = value_at }, value_at.repr);
            return .{ .value_at = .{ .ptr = c.box(ptr), .repr = value_at.repr } };
        },
    }
}

fn copyToShadow(c: *Compiler, f: *wir.FunData, value: wir.Walue, repr: Repr) wir.Walue {
    if (repr.sizeOf() == 0) {
        return .{ .i32 = 0 };
    } else {
        const tmp = shadowPush(c, f, repr);
        store(c, f, value, tmp);
        return tmp;
    }
}

fn getShuffler(f: *wir.FunData, typ: wasm.Valtype) wir.Local {
    const shuffler = f.local_shufflers.getPtr(typ);
    if (shuffler.* == null) shuffler.* = f.local_data.append(.{ .type = typ });
    return shuffler.*.?;
}

fn dropStack(c: *Compiler, f: *wir.FunData, walue: wir.Walue) wir.Walue {
    switch (walue) {
        .closure, .arg, .@"return", .local, .shadow, .i32, .@"struct", .fun => return walue,
        .stack => |valtype| {
            emitEnum(f, wasm.Opcode.drop);
            return switch (valtype) {
                .i32 => .{ .i32 = 0 },
                else => panic("TODO {}", .{valtype}),
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
        .closure, .arg, .@"return", .local, .shadow, .i32 => {
            return walue;
        },
        .stack => |valtype| {
            const local = f.local_data.append(.{ .type = valtype });
            emitEnum(f, wasm.Opcode.local_set);
            emitLebU32(f, wasmLocal(c, f, .{ .local = local }));
            return .{ .local = local };
        },
        .@"struct", .fun => {
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

const WasmRepr = union(enum) {
    primitive: wasm.Valtype,
    heap,
};

fn wasmRepr(repr: Repr) WasmRepr {
    return switch (repr) {
        .i32, .ref => .{ .primitive = wasmAbi(repr) },
        .string, .@"struct", .@"union", .fun, .only, .repr => .heap,
    };
}

fn wasmAbi(repr: Repr) wasm.Valtype {
    return switch (repr) {
        .i32, .ref => .i32,
        // Pointer to heap.
        .string, .@"struct", .@"union", .fun, .only, .repr => .i32,
    };
}

fn wasmLocal(c: *Compiler, f: *const wir.FunData, walue: wir.Walue) u32 {
    return switch (walue) {
        .arg => 0,
        .closure => 1,
        .@"return" => 2,
        .local => |local| @intCast(c.fun_type_data.get(f.fun_type).arg_types.len + local.id),
        .shadow => @intCast(c.fun_type_data.get(f.fun_type).arg_types.len + f.local_shadow.?.id),
        .stack, .i32, .@"struct", .fun, .value_at, .add => panic("Not a local: {}", .{walue}),
    };
}

fn walueRepr(c: *Compiler, f: *const wir.FunData, walue: wir.Walue) Repr {
    _ = c;
    return switch (walue) {
        .closure, .arg, .@"return", .shadow, .i32, .add => .i32,
        .stack => |valtype| valtypeRepr(valtype),
        .local => |local| valtypeRepr(f.local_data.get(local).type),
        .@"struct" => |@"struct"| .{ .@"struct" = @"struct".repr },
        .fun => |fun| .{ .fun = fun.repr },
        .value_at => |value_at| value_at.repr,
    };
}

fn valtypeRepr(valtype: wasm.Valtype) Repr {
    return switch (valtype) {
        .i32 => .i32,
        else => panic("TODO", .{}),
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

/// Don't use this directly.
fn emitLebU(c: anytype, i: anytype) void {
    // https://webassembly.github.io/spec/core/binary/values.html#integers
    var n = i;
    while (true) {
        const chunk = @as(u8, @truncate(n));
        n >>= 7;
        if (n == 0) {
            c.wasm.append(chunk & 0b0111_1111) catch oom();
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
