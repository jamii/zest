const std = @import("std");
const panic = std.debug.panic;
const assert = std.debug.assert;
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;

const zest = @import("./zest.zig");
const oom = zest.oom;
const treePart = zest.treePart;
const Compiler = zest.Compiler;
const Value = zest.Value;
const Repr = zest.Repr;
const Builtin = zest.Builtin;
const FlatLattice = zest.FlatLattice;
const dir = zest.dir;
const tir = zest.tir;

const eval = @import("./eval.zig");

pub fn inferMain(c: *Compiler) error{ EvalError, InferError }!void {
    assert(c.tir_frame_stack.items.len == 0);
    assert(c.repr_stack.items.len == 0);
    c.tir_fun_main = pushFun(
        c,
        .{
            .fun = c.dir_fun_main.?,
            .closure_repr = Repr.emptyStruct(),
            .arg_reprs = &.{},
        },
    );
    try infer(c);
}

fn pushFun(c: *Compiler, key: tir.FunKey) tir.Fun {
    const fun = c.tir_fun_data.append(tir.FunData.init(c.allocator, key));
    const f = c.tir_fun_data.getPtr(fun);
    for (c.dir_fun_data.get(key.fun).local_data.items()) |dir_local_data| {
        _ = f.local_data.append(.{
            .repr = .zero,
            .is_tmp = dir_local_data.is_tmp,
        });
    }
    c.tir_frame_stack.append(.{ .key = key, .fun = fun, .expr = .{ .id = 0 }, .ends_remaining = 0, .mode = .infer }) catch oom();
    return fun;
}

fn infer(c: *Compiler) error{ EvalError, InferError }!void {
    while (true) {
        const direction = try inferTree(
            c,
            &c.tir_frame_stack.items[c.tir_frame_stack.items.len - 1],
        );
        switch (direction) {
            .call => continue,
            .@"return" => {},
        }
        const child_frame = c.tir_frame_stack.pop();
        const return_repr_lattice = &c.tir_fun_data.getPtr(child_frame.fun).return_repr;
        switch (return_repr_lattice.*) {
            .zero => return_repr_lattice.* = .{ .one = Repr.emptyUnion() },
            .one => {},
            .many => panic("Unreachable - should have errored earlier", .{}),
        }
        c.tir_fun_by_key.put(child_frame.key, child_frame.fun) catch oom();
        if (c.tir_frame_stack.items.len == 0)
            break;
        // Don't need to advance expr.id here - we'll revisit the call and use the cached tir_fun this time.
    }
}

pub fn inferTree(c: *Compiler, frame: *tir.Frame) error{ EvalError, InferError }!enum { call, @"return" } {
    const dir_f = c.dir_fun_data.get(frame.key.fun);
    const f = c.tir_fun_data.getPtr(frame.fun);
    while (true) {
        const expr_data = dir_f.expr_data.get(frame.expr);
        switch (expr_data) {
            .call_begin => {
                _ = emit(c, f, .call_begin, null);
            },
            .call_end => |call_end| {
                const args = c.allocator.alloc(Repr, call_end.arg_count) catch oom();
                for (0..call_end.arg_count) |i| {
                    const ix = call_end.arg_count - 1 - i;
                    args[ix] = c.repr_stack.pop();
                }
                const fun = c.repr_stack.pop();
                if (fun != .fun)
                    return fail(c, .{ .not_a_fun = fun });
                const key = tir.FunKey{
                    .fun = fun.fun.fun,
                    .closure_repr = .{ .@"struct" = fun.fun.closure },
                    .arg_reprs = args,
                };
                if (c.tir_fun_by_key.get(key)) |tir_fun| {
                    // TODO once we have recursive functions, seeing a .zero here indicates that type inference is cyclic
                    const return_repr = c.tir_fun_data.get(tir_fun).return_repr.one;
                    _ = emit(c, f, .{ .call_end = tir_fun }, return_repr);
                } else {
                    // Put inputs back on stack and switch to the called function.
                    // When we return to this expr we'll hit the cached tir_fun.
                    c.repr_stack.append(fun) catch oom();
                    c.repr_stack.appendSlice(args) catch oom();
                    _ = pushFun(c, key);
                    return .call;
                }
            },
            .stage_begin => {
                eval.pushFun(c, .{
                    .fun = frame.key.fun,
                    .expr = frame.expr,
                    .args = &.{},
                    .closure = Value.emptyStruct(),
                });
                const return_value = try eval.evalStaged(c);
                const eval_frame = eval.popFun(c);
                frame.expr = eval_frame.expr;
                assert(dir_f.expr_data.get(frame.expr) == .stage_end);
                frame.expr.id -= 1;
                c.repr_stack.append(.{ .only = c.box(return_value) }) catch oom();
            },
            .stage_end, .unstage_begin, .unstage_end => {},
            else => {
                try inferExpr(c, f, expr_data);
            },
        }
        switch (treePart(expr_data)) {
            .branch_begin => frame.ends_remaining += 1,
            .branch_end => frame.ends_remaining -= 1,
            .leaf => {},
        }
        if (frame.ends_remaining == 0) return .@"return";
        frame.expr.id += 1;
    }
}

fn inferExpr(
    c: *Compiler,
    f: *tir.FunData,
    expr_data: dir.ExprData,
) error{InferError}!void {
    switch (expr_data) {
        .i64 => |i| {
            emit(c, f, .{ .i64 = i }, .i64);
        },
        .string => |string| {
            emit(c, f, .{ .string = string }, .string);
        },
        .struct_init_end => |count| {
            const keys = c.allocator.alloc(Value, count) catch oom();
            const reprs = c.allocator.alloc(Repr, count) catch oom();
            for (0..count) |i| {
                const ix = count - 1 - i;
                reprs[ix] = c.repr_stack.pop();
                keys[ix] = try popValue(c);
            }
            const repr = Repr{ .@"struct" = .{
                .keys = keys,
                .reprs = reprs,
            } };
            emit(c, f, .{ .struct_init_end = repr.@"struct" }, repr);
        },
        .fun_init_begin => {},
        .fun_init_end => |fun_init| {
            const closure = c.repr_stack.pop();
            const repr = Repr{ .fun = .{
                .fun = fun_init.fun,
                .closure = closure.@"struct",
            } };
            c.repr_stack.append(repr) catch oom();
        },
        .arg => |arg| {
            const frame = c.tir_frame_stack.items[c.tir_frame_stack.items.len - 1];
            const repr = frame.key.arg_reprs[arg.id];
            emit(c, f, .{ .arg = .{ .id = arg.id } }, repr);
        },
        .closure => {
            const frame = c.tir_frame_stack.items[c.tir_frame_stack.items.len - 1];
            const repr = frame.key.closure_repr;
            emit(c, f, .closure, repr);
        },
        .local_get => |dir_local| {
            const local = tir.Local{ .id = dir_local.id };
            // Shouldn't be able to reach get before let.
            const repr = f.local_data.get(local).repr.one;
            emit(c, f, .{ .local_get = local }, repr);
        },
        .nop_begin => {},
        .nop_end => {},
        .local_let_end => |dir_local| {
            const value = c.repr_stack.pop();
            const local = tir.Local{ .id = dir_local.id };
            _ = try reprUnion(c, &f.local_data.getPtr(local).repr, value);
            emit(c, f, .{ .local_let_end = local }, null);
        },
        .ref_init_begin => {
            emit(c, f, .{ .ref_init_begin = Repr.i64 }, null);
            c.fixup_stack.append(f.expr_data.lastKey().?) catch oom();
        },
        .ref_init_end => {
            const value = c.repr_stack.pop();
            const repr = Repr{ .ref = c.box(value) };
            f.expr_data.getPtr(c.fixup_stack.pop()).ref_init_begin = value;
            emit(c, f, .ref_init_end, repr);
        },
        .assert_object_begin => {},
        .assert_object_end => |assert_object| {
            const value = c.repr_stack.pop();
            switch (value) {
                .@"struct" => |@"struct"| {
                    if (@"struct".keys.len != assert_object.count)
                        return fail(c, .{ .wrong_number_of_keys = .{
                            .expected = assert_object.count,
                            .actual = @"struct".keys.len,
                        } });
                },
                .@"union" => return fail(c, .todo),
                .u32, .i64, .string, .repr, .repr_kind, .fun, .only, .ref => return fail(c, .{ .expected_object = value }),
            }
            c.repr_stack.append(value) catch oom();
        },
        .assert_is_ref_begin => {},
        .assert_is_ref_end => {
            const value = c.repr_stack.pop();
            if (value != .ref)
                return fail(c, .{ .expected_is_ref = value });
            c.repr_stack.append(value) catch oom();
        },
        .assert_has_no_ref_begin => {},
        .assert_has_no_ref_end => {
            const value = c.repr_stack.pop();
            if (value.hasRef(.any))
                return fail(c, .{ .expected_has_no_ref = value });
            c.repr_stack.append(value) catch oom();
        },
        .assert_has_no_ref_visible_begin => {},
        .assert_has_no_ref_visible_end => {
            const value = c.repr_stack.pop();
            if (value.hasRef(.visible))
                return fail(c, .{ .expected_has_no_ref = value });
            c.repr_stack.append(value) catch oom();
        },
        .object_get_end => {
            const key = try popValue(c);
            const object = c.repr_stack.pop();
            const get = try objectGet(c, object, key);
            emit(c, f, .{ .object_get_end = .{ .index = get.index } }, get.repr);
        },
        .ref_get_end => {
            const key = try popValue(c);
            const ref = c.repr_stack.pop();
            const get = try objectGet(c, ref.ref.*, key);
            const repr = Repr{ .ref = c.box(get.repr) };
            emit(c, f, .{ .ref_get_end = .{ .offset = get.offset } }, repr);
        },
        .ref_set_end => {
            const value = c.repr_stack.pop();
            const ref = c.repr_stack.pop();
            if (!ref.ref.equal(value))
                return fail(c, .{ .type_error = .{
                    .expected = ref.ref.*,
                    .found = value,
                } });
            emit(c, f, .ref_set_end, null);
        },
        .ref_deref_end => {
            const ref = c.repr_stack.pop();
            const repr = ref.ref.*;
            emit(c, f, .{ .ref_deref_end = repr }, repr);
        },
        .call_builtin_begin => {
            const begin = f.expr_data.append(.{ .call_builtin_begin = .dummy });
            c.fixup_stack.append(begin) catch oom();
        },
        .call_builtin_end => |builtin| {
            const begin = c.fixup_stack.pop();
            switch (builtin) {
                .equal => {
                    const arg1 = c.repr_stack.pop();
                    const arg0 = c.repr_stack.pop();
                    if (arg0 == .i64 and arg1 == .i64) {
                        f.expr_data.getPtr(begin).call_builtin_begin = .equal_i64;
                        emit(c, f, .call_builtin_end, .i64);
                    } else if (arg0 == .u32 and arg1 == .u32) {
                        f.expr_data.getPtr(begin).call_builtin_begin = .equal_u32;
                        emit(c, f, .call_builtin_end, .i64);
                    } else {
                        return fail(c, .{ .invalid_call_builtin = .{ .builtin = builtin, .args = c.dupe(Repr, &.{ arg0, arg1 }) } });
                    }
                },
                .@"not-equal" => {
                    const arg1 = c.repr_stack.pop();
                    const arg0 = c.repr_stack.pop();
                    if (arg0 == .i64 and arg1 == .i64) {
                        f.expr_data.getPtr(begin).call_builtin_begin = .not_equal_i64;
                        emit(c, f, .call_builtin_end, .i64);
                    } else if (arg0 == .u32 and arg1 == .u32) {
                        f.expr_data.getPtr(begin).call_builtin_begin = .not_equal_u32;
                        emit(c, f, .call_builtin_end, .i64);
                    } else {
                        return fail(c, .{ .invalid_call_builtin = .{ .builtin = builtin, .args = c.dupe(Repr, &.{ arg0, arg1 }) } });
                    }
                },
                .@"less-than" => {
                    const arg1 = c.repr_stack.pop();
                    const arg0 = c.repr_stack.pop();
                    if (arg0 == .i64 and arg1 == .i64) {
                        f.expr_data.getPtr(begin).call_builtin_begin = .less_than_i64;
                        emit(c, f, .call_builtin_end, .i64);
                    } else if (arg0 == .u32 and arg1 == .u32) {
                        f.expr_data.getPtr(begin).call_builtin_begin = .less_than_u32;
                        emit(c, f, .call_builtin_end, .i64);
                    } else {
                        return fail(c, .{ .invalid_call_builtin = .{ .builtin = builtin, .args = c.dupe(Repr, &.{ arg0, arg1 }) } });
                    }
                },
                .@"less-than-or-equal" => {
                    const arg1 = c.repr_stack.pop();
                    const arg0 = c.repr_stack.pop();
                    if (arg0 == .i64 and arg1 == .i64) {
                        f.expr_data.getPtr(begin).call_builtin_begin = .less_than_or_equal_i64;
                        emit(c, f, .call_builtin_end, .i64);
                    } else if (arg0 == .u32 and arg1 == .u32) {
                        f.expr_data.getPtr(begin).call_builtin_begin = .less_than_or_equal_u32;
                        emit(c, f, .call_builtin_end, .i64);
                    } else {
                        return fail(c, .{ .invalid_call_builtin = .{ .builtin = builtin, .args = c.dupe(Repr, &.{ arg0, arg1 }) } });
                    }
                },
                .@"more-than" => {
                    const arg1 = c.repr_stack.pop();
                    const arg0 = c.repr_stack.pop();
                    if (arg0 == .i64 and arg1 == .i64) {
                        f.expr_data.getPtr(begin).call_builtin_begin = .more_than_i64;
                        emit(c, f, .call_builtin_end, .i64);
                    } else if (arg0 == .u32 and arg1 == .u32) {
                        f.expr_data.getPtr(begin).call_builtin_begin = .more_than_u32;
                        emit(c, f, .call_builtin_end, .i64);
                    } else {
                        return fail(c, .{ .invalid_call_builtin = .{ .builtin = builtin, .args = c.dupe(Repr, &.{ arg0, arg1 }) } });
                    }
                },
                .@"more-than-or-equal" => {
                    const arg1 = c.repr_stack.pop();
                    const arg0 = c.repr_stack.pop();
                    if (arg0 == .i64 and arg1 == .i64) {
                        f.expr_data.getPtr(begin).call_builtin_begin = .more_than_or_equal_i64;
                        emit(c, f, .call_builtin_end, .i64);
                    } else if (arg0 == .u32 and arg1 == .u32) {
                        f.expr_data.getPtr(begin).call_builtin_begin = .more_than_or_equal_u32;
                        emit(c, f, .call_builtin_end, .i64);
                    } else {
                        return fail(c, .{ .invalid_call_builtin = .{ .builtin = builtin, .args = c.dupe(Repr, &.{ arg0, arg1 }) } });
                    }
                },
                .add => {
                    const arg1 = c.repr_stack.pop();
                    const arg0 = c.repr_stack.pop();
                    if (arg0 == .i64 and arg1 == .i64) {
                        f.expr_data.getPtr(begin).call_builtin_begin = .add_i64;
                        emit(c, f, .call_builtin_end, .i64);
                    } else if (arg0 == .u32 and arg1 == .u32) {
                        f.expr_data.getPtr(begin).call_builtin_begin = .add_u32;
                        emit(c, f, .call_builtin_end, .u32);
                    } else {
                        return fail(c, .{ .invalid_call_builtin = .{ .builtin = builtin, .args = c.dupe(Repr, &.{ arg0, arg1 }) } });
                    }
                },
                .subtract => {
                    const arg1 = c.repr_stack.pop();
                    const arg0 = c.repr_stack.pop();
                    if (arg0 == .i64 and arg1 == .i64) {
                        f.expr_data.getPtr(begin).call_builtin_begin = .subtract_i64;
                        emit(c, f, .call_builtin_end, .i64);
                    } else if (arg0 == .u32 and arg1 == .u32) {
                        f.expr_data.getPtr(begin).call_builtin_begin = .subtract_u32;
                        emit(c, f, .call_builtin_end, .u32);
                    } else {
                        return fail(c, .{ .invalid_call_builtin = .{ .builtin = builtin, .args = c.dupe(Repr, &.{ arg0, arg1 }) } });
                    }
                },
                .multiply => {
                    const arg1 = c.repr_stack.pop();
                    const arg0 = c.repr_stack.pop();
                    if (arg0 == .i64 and arg1 == .i64) {
                        f.expr_data.getPtr(begin).call_builtin_begin = .multiply_i64;
                        emit(c, f, .call_builtin_end, .i64);
                    } else if (arg0 == .u32 and arg1 == .u32) {
                        f.expr_data.getPtr(begin).call_builtin_begin = .multiply_u32;
                        emit(c, f, .call_builtin_end, .u32);
                    } else {
                        return fail(c, .{ .invalid_call_builtin = .{ .builtin = builtin, .args = c.dupe(Repr, &.{ arg0, arg1 }) } });
                    }
                },
                .remainder => {
                    const arg1 = c.repr_stack.pop();
                    const arg0 = c.repr_stack.pop();
                    if (arg0 == .i64 and arg1 == .i64) {
                        f.expr_data.getPtr(begin).call_builtin_begin = .remainder_i64;
                        emit(c, f, .call_builtin_end, .i64);
                    } else if (arg0 == .u32 and arg1 == .u32) {
                        f.expr_data.getPtr(begin).call_builtin_begin = .remainder_u32;
                        emit(c, f, .call_builtin_end, .u32);
                    } else {
                        return fail(c, .{ .invalid_call_builtin = .{ .builtin = builtin, .args = c.dupe(Repr, &.{ arg0, arg1 }) } });
                    }
                },
                .@"bit-shift-left" => {
                    const arg1 = c.repr_stack.pop();
                    const arg0 = c.repr_stack.pop();
                    if (arg0 == .u32 and arg1 == .u32) {
                        f.expr_data.getPtr(begin).call_builtin_begin = .bit_shift_left_u32;
                        emit(c, f, .call_builtin_end, .u32);
                    } else {
                        return fail(c, .{ .invalid_call_builtin = .{ .builtin = builtin, .args = c.dupe(Repr, &.{ arg0, arg1 }) } });
                    }
                },
                .clz => {
                    const arg = c.repr_stack.pop();
                    if (arg == .u32) {
                        f.expr_data.getPtr(begin).call_builtin_begin = .clz_u32;
                        emit(c, f, .call_builtin_end, .u32);
                    } else {
                        return fail(c, .{ .invalid_call_builtin = .{ .builtin = builtin, .args = c.dupe(Repr, &.{arg}) } });
                    }
                },
                .@"memory-size" => {
                    f.expr_data.getPtr(begin).call_builtin_begin = .memory_size;
                    emit(c, f, .call_builtin_end, .u32);
                },
                .@"memory-grow" => {
                    const grow_page_count = c.repr_stack.pop();
                    if (grow_page_count != .u32)
                        return fail(c, .{ .invalid_call_builtin = .{ .builtin = builtin, .args = c.dupe(Repr, &.{grow_page_count}) } });
                    f.expr_data.getPtr(begin).call_builtin_begin = .memory_grow;
                    emit(c, f, .call_builtin_end, .u32);
                },
                .@"memory-fill" => {
                    const byte_count = c.repr_stack.pop();
                    const value = c.repr_stack.pop();
                    const to_ptr = c.repr_stack.pop();
                    if (to_ptr != .u32 or value != .u32 or byte_count != .u32)
                        return fail(c, .{ .invalid_call_builtin = .{ .builtin = builtin, .args = c.dupe(Repr, &.{ to_ptr, value, byte_count }) } });
                    f.expr_data.getPtr(begin).call_builtin_begin = .memory_fill;
                    emit(c, f, .call_builtin_end, Repr.emptyStruct());
                },
                .@"memory-copy" => {
                    const byte_count = c.repr_stack.pop();
                    const from_ptr = c.repr_stack.pop();
                    const to_ptr = c.repr_stack.pop();
                    if (to_ptr != .u32 or from_ptr != .u32 or byte_count != .u32)
                        return fail(c, .{ .invalid_call_builtin = .{ .builtin = builtin, .args = c.dupe(Repr, &.{ to_ptr, from_ptr, byte_count }) } });
                    f.expr_data.getPtr(begin).call_builtin_begin = .memory_copy;
                    emit(c, f, .call_builtin_end, Repr.emptyStruct());
                },
                .@"heap-start" => {
                    f.expr_data.getPtr(begin).call_builtin_begin = .heap_start;
                    emit(c, f, .call_builtin_end, .u32);
                },
                .load => {
                    const repr = try popValue(c);
                    const address = c.repr_stack.pop();
                    if (address != .u32 or repr != .repr)
                        return fail(c, .{ .invalid_call_builtin = .{ .builtin = builtin, .args = c.dupe(Repr, &.{ address, repr.reprOf() }) } });
                    f.expr_data.getPtr(begin).call_builtin_begin = .{ .load = repr.repr };
                    emit(c, f, .call_builtin_end, repr.repr);
                },
                .store => {
                    const value = c.repr_stack.pop();
                    const address = c.repr_stack.pop();
                    if (address != .u32)
                        return fail(c, .{ .invalid_call_builtin = .{ .builtin = builtin, .args = c.dupe(Repr, &.{ address, value }) } });
                    f.expr_data.getPtr(begin).call_builtin_begin = .store;
                    emit(c, f, .call_builtin_end, Repr.emptyStruct());
                },
                .@"size-of" => {
                    const repr = try popValue(c);
                    if (repr != .repr)
                        return fail(c, .{ .invalid_call_builtin = .{ .builtin = builtin, .args = c.dupe(Repr, &.{repr.reprOf()}) } });
                    f.expr_data.getPtr(begin).call_builtin_begin = .{ .size_of = @intCast(repr.repr.sizeOf()) };
                    emit(c, f, .call_builtin_end, .u32);
                },
                .print => {
                    const string = c.repr_stack.pop();
                    if (string != .string)
                        return fail(c, .{ .invalid_call_builtin = .{ .builtin = builtin, .args = c.dupe(Repr, &.{string}) } });
                    f.expr_data.getPtr(begin).call_builtin_begin = .print_string;
                    emit(c, f, .call_builtin_end, .u32);
                },
                .panic => {
                    f.expr_data.getPtr(begin).call_builtin_begin = .panic;
                    // TODO Use empty union to indicate that this doesn't return.
                    emit(c, f, .call_builtin_end, Repr.emptyStruct());
                },
                else => return fail(c, .todo),
            }
        },
        .make_begin => {
            emit(c, f, .make_begin, null);
        },
        .make_end => {
            const args = c.repr_stack.pop();
            const head = try popValue(c);
            switch (head) {
                .repr => |to_repr| {
                    if (args.@"struct".keys.len != 1 or
                        args.@"struct".keys[0] != .i64 or
                        args.@"struct".keys[0].i64 != 0)
                        return fail(c, .{ .cannot_make = .{ .head = head, .args = args } });
                    const from_repr = args.@"struct".reprs[0];
                    if (from_repr.equal(to_repr)) {
                        emit(c, f, .{ .make_end = .nop }, to_repr);
                    } else if (from_repr == .i64 and to_repr == .u32) {
                        // TODO We should only allow this cast when from is a constant walue.
                        emit(c, f, .{ .make_end = .i64_to_u32 }, to_repr);
                    } else if (to_repr == .@"union" and from_repr == .@"struct") {
                        if (from_repr.@"struct".keys.len != 1)
                            return fail(c, .{ .type_error = .{ .expected = to_repr, .found = from_repr } });
                        const key = from_repr.@"struct".keys[0];
                        const repr = from_repr.@"struct".reprs[0];
                        const tag = to_repr.@"union".get(key) orelse
                            return fail(c, .{ .type_error = .{ .expected = to_repr, .found = from_repr } });
                        if (!repr.equal(to_repr.@"union".reprs[tag]))
                            return fail(c, .{ .type_error = .{ .expected = to_repr, .found = from_repr } });
                        emit(c, f, .{ .make_end = .{ .union_init = .{ .repr = to_repr.@"union", .tag = @intCast(tag) } } }, to_repr);
                    } else {
                        return fail(c, .{ .type_error = .{ .expected = to_repr, .found = from_repr } });
                    }
                },
                .repr_kind => panic("TODO {}", .{head}),
                else => return fail(c, .{ .cannot_make_head = .{ .head = head } }),
            }
        },
        .block_begin => {
            c.block_value_count_stack.append(c.repr_stack.items.len) catch oom();
            emit(c, f, .block_begin, null);
        },
        .block_last => {
            const value_count_at_begin = c.block_value_count_stack.items[c.block_value_count_stack.items.len - 1];
            for (value_count_at_begin..c.repr_stack.items.len) |_| {
                _ = c.repr_stack.pop();
            }
            emit(c, f, .block_last, null);
        },
        .block_end => {
            const value_count_at_begin = c.block_value_count_stack.pop();
            switch (c.repr_stack.items.len - value_count_at_begin) {
                0 => emit(c, f, .block_end, Repr.emptyStruct()),
                1 => emit(c, f, .block_end, c.repr_stack.pop()),
                else => panic("More than one value returned from block", .{}),
            }
        },
        .return_end => {
            const value = c.repr_stack.pop();
            _ = try reprUnion(c, &f.return_repr, value);
            emit(c, f, .return_end, null);
        },
        .if_begin => {
            emit(c, f, .{ .if_begin = Repr.i64 }, null);
            c.fixup_stack.append(f.expr_data.lastKey().?) catch oom();
        },
        .if_then => {
            emit(c, f, .if_then, null);
        },
        .if_else => {
            emit(c, f, .if_else, null);
        },
        .if_end => {
            const @"else" = c.repr_stack.pop();
            const then = c.repr_stack.pop();
            const cond = c.repr_stack.pop();
            if (cond != .i64)
                return fail(c, .{ .not_a_bool = cond });
            if (!then.equal(@"else"))
                return fail(c, .{ .type_error = .{ .expected = then, .found = @"else" } });
            f.expr_data.getPtr(c.fixup_stack.pop()).if_begin = then;
            emit(c, f, .if_end, then);
        },
        .while_begin => {
            emit(c, f, .while_begin, null);
        },
        .while_body => {
            emit(c, f, .while_body, null);
        },
        .while_end => {
            _ = c.repr_stack.pop();
            const cond = c.repr_stack.pop();
            if (cond != .i64)
                return fail(c, .{ .not_a_bool = cond });
            emit(c, f, .while_end, Repr.emptyStruct());
        },
        .call_begin, .call_end, .stage_begin, .stage_end, .unstage_begin, .unstage_end => panic("Should be handled in inferTree, not inferExpr", .{}),
        inline else => |_, tag| {
            if (comptime std.mem.endsWith(u8, @tagName(tag), "_begin")) {
                emit(c, f, @unionInit(tir.ExprData, @tagName(tag), {}), null);
            } else {
                return fail(c, .todo);
            }
        },
    }
}

fn reprUnion(c: *Compiler, lattice: *FlatLattice(Repr), found_repr: Repr) !Repr {
    switch (lattice.*) {
        .zero => {
            lattice.* = .{ .one = found_repr };
            return found_repr;
        },
        .one => |expected_repr| {
            if (expected_repr.equal(found_repr)) {
                return found_repr;
            } else {
                lattice.* = .{ .many = expected_repr };
                return fail(c, .{ .type_error = .{ .expected = expected_repr, .found = found_repr } });
            }
        },
        .many => |expected_repr| {
            return fail(c, .{ .type_error = .{ .expected = expected_repr, .found = found_repr } });
        },
    }
}

fn objectGet(c: *Compiler, object: Repr, key: Value) error{InferError}!struct { index: usize, repr: Repr, offset: u32 } {
    switch (object) {
        .u32, .i64, .string, .repr, .repr_kind, .fun, .only => return fail(c, .{ .expected_object = object }),
        .@"struct" => |@"struct"| {
            const ix = @"struct".get(key) orelse
                return fail(c, .{ .key_not_found = .{ .object = object, .key = key } });
            return .{
                .index = ix,
                .repr = @"struct".reprs[ix],
                .offset = @intCast(@"struct".offsetOf(ix)),
            };
        },
        .@"union" => return fail(c, .todo),
        .ref => unreachable, // always passes through ref_deref first
    }
}

fn popValue(c: *Compiler) error{InferError}!Value {
    const repr = c.repr_stack.pop();
    return repr.valueOf() orelse fail(c, .{ .value_not_staged = repr });
}

fn emit(c: *Compiler, f: *tir.FunData, expr: tir.ExprData, repr: ?Repr) void {
    const frame = c.tir_frame_stack.items[c.tir_frame_stack.items.len - 1];
    if (frame.mode == .infer)
        _ = f.expr_data.append(expr);
    if (repr != null) c.repr_stack.append(repr.?) catch oom();
}

fn fail(c: *Compiler, data: InferErrorData) error{InferError} {
    const frame = c.tir_frame_stack.items[c.tir_frame_stack.items.len - 1];
    c.error_data = .{ .infer = .{ .key = frame.key, .fun = frame.fun, .expr = frame.expr, .data = data } };
    return error.InferError;
}

pub const InferErrorData = union(enum) {
    value_not_staged: Repr,
    type_error: struct {
        expected: Repr,
        found: Repr,
    },
    wrong_number_of_keys: struct {
        expected: usize,
        actual: usize,
    },
    expected_object: Repr,
    expected_is_ref: Repr,
    expected_has_no_ref: Repr,
    key_not_found: struct {
        object: Repr,
        key: Value,
    },
    not_a_fun: Repr,
    not_a_bool: Repr,
    invalid_call_builtin: struct {
        builtin: Builtin,
        args: []Repr,
    },
    cannot_make: struct {
        head: Value,
        args: Repr,
    },
    cannot_make_head: struct {
        head: Value,
    },
    todo,
};
