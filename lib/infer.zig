const std = @import("std");
const panic = std.debug.panic;
const assert = std.debug.assert;
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;

const zest = @import("./zest.zig");
const oom = zest.oom;
const treePart = zest.treePart;
const convertPostorderToPreorder = zest.convertPostorderToPreorder;
const Compiler = zest.Compiler;
const Value = zest.Value;
const Repr = zest.Repr;
const Builtin = zest.Builtin;
const FlatLattice = zest.FlatLattice;
const dir = zest.dir;
const tir = zest.tir;

const eval = @import("./eval.zig");

pub fn infer(c: *Compiler) error{ EvalError, InferError }!void {
    c.infer_mode = .infer;
    c.tir_fun_main = try inferFun(c, .{
        .fun = c.dir_fun_main.?,
        .closure_repr = Repr.emptyStruct(),
        .arg_reprs = &.{},
    });
}

fn inferFun(c: *Compiler, key: tir.FunKey) !tir.Fun {
    if (c.tir_fun_by_key.get(key)) |fun|
        return fun;

    var f = tir.FunData.init(c.allocator, key);
    const dir_f = c.dir_fun_data.get(key.fun);
    for (dir_f.local_data.items()) |dir_local_data| {
        _ = f.local_data.append(.{
            .repr = .zero,
            .is_tmp = dir_local_data.is_tmp,
        });
    }

    const tir_fun_data_next = c.tir_fun_data_next;
    c.tir_fun_data_next = &f;
    defer c.tir_fun_data_next = tir_fun_data_next;

    _ = try inferExpr(c, &f, dir_f);
    convertPostorderToPreorder(c, tir.Expr, tir.ExprData, f.expr_data_post, &f.expr_data_pre);
    switch (f.return_repr) {
        .zero => f.return_repr = .{ .one = Repr.emptyUnion() },
        .one => {},
        .many => panic("Unreachable - should have errored earlier", .{}),
    }

    const fun = c.tir_fun_data.append(f);
    c.tir_fun_by_key.put(key, fun) catch oom();
    return fun;
}

pub fn inferExpr(
    c: *Compiler,
    f: *tir.FunData,
    dir_f: dir.FunData,
) error{ InferError, EvalError }!Repr {
    const expr_data = take(f, dir_f);
    //zest.p(.{ .infer = expr_data });
    switch (expr_data) {
        .i64 => |i| {
            emit(c, f, .{ .i64 = i });
            return .i64;
        },
        .string => |string| {
            emit(c, f, .{ .string = string });
            return .string;
        },
        .struct_init => |struct_init| {
            const keys = c.allocator.alloc(Value, struct_init.count) catch oom();
            const reprs = c.allocator.alloc(Repr, struct_init.count) catch oom();
            for (keys, reprs) |*key, *repr| {
                key.* = try unstage(c, try inferExpr(c, f, dir_f));
                repr.* = try inferExpr(c, f, dir_f);
            }
            const result = Repr{ .@"struct" = .{ .keys = keys, .reprs = reprs } };
            emit(c, f, .{ .struct_init = result.@"struct" });
            return result;
        },
        .fun_init => |fun_init| {
            const closure = try inferExpr(c, f, dir_f);
            return .{ .fun = .{
                .fun = fun_init.fun,
                .closure = closure.@"struct",
            } };
        },
        .arg => |arg| {
            emit(c, f, .{ .arg = .{ .id = arg.id } });
            return f.key.arg_reprs[arg.id];
        },
        .closure => {
            emit(c, f, .closure);
            return f.key.closure_repr;
        },
        .local_get => |dir_local| {
            const local = tir.Local{ .id = dir_local.id };
            emit(c, f, .{ .local_get = local });
            // Shouldn't be able to reach get before let.
            return f.local_data.get(local).repr.one;
        },
        .local_let => |dir_local| {
            const value = try inferExpr(c, f, dir_f);
            const local = tir.Local{ .id = dir_local.id };
            emit(c, f, .{ .local_let = local });
            _ = try reprUnion(c, &f.local_data.getPtr(local).repr, value);
            return Repr.emptyStruct();
        },
        .ref_init => {
            const value = try inferExpr(c, f, dir_f);
            emit(c, f, .{ .ref_init = value });
            return .{ .ref = c.box(value) };
        },
        .assert_object => |assert_object| {
            const value = try inferExpr(c, f, dir_f);
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
            return value;
        },
        .assert_is_ref => {
            const value = try inferExpr(c, f, dir_f);
            if (value != .ref)
                return fail(c, .{ .expected_is_ref = value });
            return value;
        },
        .assert_has_no_ref => {
            const value = try inferExpr(c, f, dir_f);
            if (value.hasRef(.any))
                return fail(c, .{ .expected_has_no_ref = value });
            return value;
        },
        .assert_has_no_ref_visible => {
            const value = try inferExpr(c, f, dir_f);
            if (value.hasRef(.visible))
                return fail(c, .{ .expected_has_no_ref = value });
            return value;
        },
        .object_get => {
            const object = try inferExpr(c, f, dir_f);
            const key = try unstage(c, try inferExpr(c, f, dir_f));
            const get = try objectGet(c, object, key);
            emit(c, f, .{ .object_get = .{ .index = get.index } });
            return get.repr;
        },
        .ref_get => {
            const ref = try inferExpr(c, f, dir_f);
            const key = try unstage(c, try inferExpr(c, f, dir_f));
            const get = try objectGet(c, ref.ref.*, key);
            emit(c, f, .{ .ref_get = switch (ref.ref.*) {
                .@"struct" => .{ .struct_offset = get.offset },
                .@"union" => .{ .union_tag = @intCast(get.index) },
                else => unreachable,
            } });
            return .{ .ref = c.box(get.repr) };
        },
        .ref_set => {
            const ref = try inferExpr(c, f, dir_f);
            const value = try inferExpr(c, f, dir_f);
            if (!ref.ref.equal(value))
                return fail(c, .{ .type_error = .{
                    .expected = ref.ref.*,
                    .found = value,
                } });
            emit(c, f, .ref_set);
            return Repr.emptyStruct();
        },
        .ref_deref => {
            const ref = try inferExpr(c, f, dir_f);
            const repr = ref.ref.*;
            emit(c, f, .{ .ref_deref = repr });
            return repr;
        },
        .call => |call| {
            const fun = try inferExpr(c, f, dir_f);
            if (fun != .fun)
                return fail(c, .{ .not_a_fun = fun });
            const args = c.allocator.alloc(Repr, call.arg_count) catch oom();
            for (args) |*arg| {
                arg.* = try inferExpr(c, f, dir_f);
            }
            const key = tir.FunKey{
                .fun = fun.fun.fun,
                .closure_repr = .{ .@"struct" = fun.fun.closure },
                .arg_reprs = args,
            };
            // TODO Once we have type asserts on return, we'll want to trust that and queue fun for later to avoid stack overflows.
            const tir_fun = try inferFun(c, key);
            emit(c, f, .{ .call = tir_fun });
            // TODO Once we have recursive functions we'll have to be careful about reentrancy here.
            return c.tir_fun_data.get(tir_fun).return_repr.one;
        },
        .call_builtin => |builtin| {
            switch (builtin) {
                .equal => {
                    const arg0 = try inferExpr(c, f, dir_f);
                    const arg1 = try inferExpr(c, f, dir_f);
                    if (arg0 == .i64 and arg1 == .i64) {
                        emit(c, f, .{ .call_builtin = .equal_i64 });
                        return .i64;
                    } else if (arg0 == .u32 and arg1 == .u32) {
                        emit(c, f, .{ .call_builtin = .equal_u32 });
                        return .i64;
                    } else {
                        return fail(c, .{ .invalid_call_builtin = .{ .builtin = builtin, .args = c.dupe(Repr, &.{ arg0, arg1 }) } });
                    }
                },
                .@"not-equal" => {
                    const arg0 = try inferExpr(c, f, dir_f);
                    const arg1 = try inferExpr(c, f, dir_f);
                    if (arg0 == .i64 and arg1 == .i64) {
                        emit(c, f, .{ .call_builtin = .not_equal_i64 });
                        return .i64;
                    } else if (arg0 == .u32 and arg1 == .u32) {
                        emit(c, f, .{ .call_builtin = .not_equal_u32 });
                        return .i64;
                    } else {
                        return fail(c, .{ .invalid_call_builtin = .{ .builtin = builtin, .args = c.dupe(Repr, &.{ arg0, arg1 }) } });
                    }
                },
                .@"less-than" => {
                    const arg0 = try inferExpr(c, f, dir_f);
                    const arg1 = try inferExpr(c, f, dir_f);
                    if (arg0 == .i64 and arg1 == .i64) {
                        emit(c, f, .{ .call_builtin = .less_than_i64 });
                        return .i64;
                    } else if (arg0 == .u32 and arg1 == .u32) {
                        emit(c, f, .{ .call_builtin = .less_than_u32 });
                        return .i64;
                    } else {
                        return fail(c, .{ .invalid_call_builtin = .{ .builtin = builtin, .args = c.dupe(Repr, &.{ arg0, arg1 }) } });
                    }
                },
                .@"less-than-or-equal" => {
                    const arg0 = try inferExpr(c, f, dir_f);
                    const arg1 = try inferExpr(c, f, dir_f);
                    if (arg0 == .i64 and arg1 == .i64) {
                        emit(c, f, .{ .call_builtin = .less_than_or_equal_i64 });
                        return .i64;
                    } else if (arg0 == .u32 and arg1 == .u32) {
                        emit(c, f, .{ .call_builtin = .less_than_or_equal_u32 });
                        return .i64;
                    } else {
                        return fail(c, .{ .invalid_call_builtin = .{ .builtin = builtin, .args = c.dupe(Repr, &.{ arg0, arg1 }) } });
                    }
                },
                .@"more-than" => {
                    const arg0 = try inferExpr(c, f, dir_f);
                    const arg1 = try inferExpr(c, f, dir_f);
                    if (arg0 == .i64 and arg1 == .i64) {
                        emit(c, f, .{ .call_builtin = .more_than_i64 });
                        return .i64;
                    } else if (arg0 == .u32 and arg1 == .u32) {
                        emit(c, f, .{ .call_builtin = .more_than_u32 });
                        return .i64;
                    } else {
                        return fail(c, .{ .invalid_call_builtin = .{ .builtin = builtin, .args = c.dupe(Repr, &.{ arg0, arg1 }) } });
                    }
                },
                .@"more-than-or-equal" => {
                    const arg0 = try inferExpr(c, f, dir_f);
                    const arg1 = try inferExpr(c, f, dir_f);
                    if (arg0 == .i64 and arg1 == .i64) {
                        emit(c, f, .{ .call_builtin = .more_than_or_equal_i64 });
                        return .i64;
                    } else if (arg0 == .u32 and arg1 == .u32) {
                        emit(c, f, .{ .call_builtin = .more_than_or_equal_u32 });
                        return .i64;
                    } else {
                        return fail(c, .{ .invalid_call_builtin = .{ .builtin = builtin, .args = c.dupe(Repr, &.{ arg0, arg1 }) } });
                    }
                },
                .negate => {
                    const arg = try inferExpr(c, f, dir_f);
                    if (arg == .i64) {
                        emit(c, f, .{ .call_builtin = .negate_i64 });
                        return .i64;
                    } else {
                        return fail(c, .{ .invalid_call_builtin = .{ .builtin = builtin, .args = c.dupe(Repr, &.{arg}) } });
                    }
                },
                .add => {
                    const arg0 = try inferExpr(c, f, dir_f);
                    const arg1 = try inferExpr(c, f, dir_f);
                    if (arg0 == .i64 and arg1 == .i64) {
                        emit(c, f, .{ .call_builtin = .add_i64 });
                        return .i64;
                    } else if (arg0 == .u32 and arg1 == .u32) {
                        emit(c, f, .{ .call_builtin = .add_u32 });
                        return .u32;
                    } else {
                        return fail(c, .{ .invalid_call_builtin = .{ .builtin = builtin, .args = c.dupe(Repr, &.{ arg0, arg1 }) } });
                    }
                },
                .subtract => {
                    const arg0 = try inferExpr(c, f, dir_f);
                    const arg1 = try inferExpr(c, f, dir_f);
                    if (arg0 == .i64 and arg1 == .i64) {
                        emit(c, f, .{ .call_builtin = .subtract_i64 });
                        return .i64;
                    } else if (arg0 == .u32 and arg1 == .u32) {
                        emit(c, f, .{ .call_builtin = .subtract_u32 });
                        return .u32;
                    } else {
                        return fail(c, .{ .invalid_call_builtin = .{ .builtin = builtin, .args = c.dupe(Repr, &.{ arg0, arg1 }) } });
                    }
                },
                .multiply => {
                    const arg0 = try inferExpr(c, f, dir_f);
                    const arg1 = try inferExpr(c, f, dir_f);
                    if (arg0 == .i64 and arg1 == .i64) {
                        emit(c, f, .{ .call_builtin = .multiply_i64 });
                        return .i64;
                    } else if (arg0 == .u32 and arg1 == .u32) {
                        emit(c, f, .{ .call_builtin = .multiply_u32 });
                        return .u32;
                    } else {
                        return fail(c, .{ .invalid_call_builtin = .{ .builtin = builtin, .args = c.dupe(Repr, &.{ arg0, arg1 }) } });
                    }
                },
                .remainder => {
                    const arg0 = try inferExpr(c, f, dir_f);
                    const arg1 = try inferExpr(c, f, dir_f);
                    if (arg0 == .i64 and arg1 == .i64) {
                        emit(c, f, .{ .call_builtin = .remainder_i64 });
                        return .i64;
                    } else if (arg0 == .u32 and arg1 == .u32) {
                        emit(c, f, .{ .call_builtin = .remainder_u32 });
                        return .u32;
                    } else {
                        return fail(c, .{ .invalid_call_builtin = .{ .builtin = builtin, .args = c.dupe(Repr, &.{ arg0, arg1 }) } });
                    }
                },
                .@"bit-shift-left" => {
                    const arg0 = try inferExpr(c, f, dir_f);
                    const arg1 = try inferExpr(c, f, dir_f);
                    if (arg0 == .u32 and arg1 == .u32) {
                        emit(c, f, .{ .call_builtin = .bit_shift_left_u32 });
                        return .u32;
                    } else {
                        return fail(c, .{ .invalid_call_builtin = .{ .builtin = builtin, .args = c.dupe(Repr, &.{ arg0, arg1 }) } });
                    }
                },
                .clz => {
                    const arg = try inferExpr(c, f, dir_f);
                    if (arg == .u32) {
                        emit(c, f, .{ .call_builtin = .clz_u32 });
                        return .u32;
                    } else {
                        return fail(c, .{ .invalid_call_builtin = .{ .builtin = builtin, .args = c.dupe(Repr, &.{arg}) } });
                    }
                },
                .@"memory-size" => {
                    emit(c, f, .{ .call_builtin = .memory_size });
                    return .u32;
                },
                .@"memory-grow" => {
                    const grow_page_count = try inferExpr(c, f, dir_f);
                    if (grow_page_count != .u32)
                        return fail(c, .{ .invalid_call_builtin = .{ .builtin = builtin, .args = c.dupe(Repr, &.{grow_page_count}) } });
                    emit(c, f, .{ .call_builtin = .memory_grow });
                    return .u32;
                },
                .@"memory-fill" => {
                    const to_ptr = try inferExpr(c, f, dir_f);
                    const value = try inferExpr(c, f, dir_f);
                    const byte_count = try inferExpr(c, f, dir_f);
                    if (to_ptr != .u32 or value != .u32 or byte_count != .u32)
                        return fail(c, .{ .invalid_call_builtin = .{ .builtin = builtin, .args = c.dupe(Repr, &.{ to_ptr, value, byte_count }) } });
                    emit(c, f, .{ .call_builtin = .memory_fill });
                    return Repr.emptyStruct();
                },
                .@"memory-copy" => {
                    const to_ptr = try inferExpr(c, f, dir_f);
                    const from_ptr = try inferExpr(c, f, dir_f);
                    const byte_count = try inferExpr(c, f, dir_f);
                    if (to_ptr != .u32 or from_ptr != .u32 or byte_count != .u32)
                        return fail(c, .{ .invalid_call_builtin = .{ .builtin = builtin, .args = c.dupe(Repr, &.{ to_ptr, from_ptr, byte_count }) } });
                    emit(c, f, .{ .call_builtin = .memory_copy });
                    return Repr.emptyStruct();
                },
                .@"heap-start" => {
                    emit(c, f, .{ .call_builtin = .heap_start });
                    return .u32;
                },
                .load => {
                    const address = try inferExpr(c, f, dir_f);
                    const repr = try unstage(c, try inferExpr(c, f, dir_f));
                    if (address != .u32 or repr != .repr)
                        return fail(c, .{ .invalid_call_builtin = .{ .builtin = builtin, .args = c.dupe(Repr, &.{ address, repr.reprOf() }) } });
                    emit(c, f, .{ .call_builtin = .{ .load = repr.repr } });
                    return repr.repr;
                },
                .store => {
                    const address = try inferExpr(c, f, dir_f);
                    const value = try inferExpr(c, f, dir_f);
                    if (address != .u32)
                        return fail(c, .{ .invalid_call_builtin = .{ .builtin = builtin, .args = c.dupe(Repr, &.{ address, value }) } });
                    emit(c, f, .{ .call_builtin = .store });
                    return Repr.emptyStruct();
                },
                .@"size-of" => {
                    const repr = try unstage(c, try inferExpr(c, f, dir_f));
                    if (repr != .repr)
                        return fail(c, .{ .invalid_call_builtin = .{ .builtin = builtin, .args = c.dupe(Repr, &.{repr.reprOf()}) } });
                    emit(c, f, .{ .call_builtin = .{ .size_of = @intCast(repr.repr.sizeOf()) } });
                    return .u32;
                },
                .print => {
                    const repr = try inferExpr(c, f, dir_f);
                    switch (repr) {
                        .u32 => emit(c, f, .{ .call_builtin = .print_u32 }),
                        .i64 => emit(c, f, .{ .call_builtin = .print_i64 }),
                        .string => emit(c, f, .{ .call_builtin = .print_string }),
                        else => return fail(c, .{ .invalid_call_builtin = .{ .builtin = builtin, .args = c.dupe(Repr, &.{repr}) } }),
                    }
                    return Repr.emptyStruct();
                },
                .panic => {
                    emit(c, f, .{ .call_builtin = .panic });
                    return Repr.emptyUnion();
                },
                .@"union-has-key" => {
                    const object = try inferExpr(c, f, dir_f);
                    const key = try unstage(c, try inferExpr(c, f, dir_f));
                    if (object != .@"union")
                        return fail(c, .{ .invalid_call_builtin = .{ .builtin = builtin, .args = c.dupe(Repr, &.{ object, key.reprOf() }) } });
                    const index = object.@"union".get(key) orelse
                        return fail(c, .{ .union_never_has_key = .{ .object = object, .key = key } });
                    emit(c, f, .{ .call_builtin = .{ .union_has_key = @intCast(index) } });
                    return .i64;
                },
                .each => {
                    const value = try inferExpr(c, f, dir_f);
                    const fun = try inferExpr(c, f, dir_f);
                    if (fun != .fun)
                        return fail(c, .{ .invalid_call_builtin = .{ .builtin = builtin, .args = c.dupe(Repr, &.{ value, fun }) } });
                    const keys, const reprs = switch (value) {
                        // TODO Would it be better to generate an each_struct/union instruction and leave it for generate?
                        .@"struct" => |@"struct"| .{ @"struct".keys, @"struct".reprs },
                        .@"union" => |@"union"| .{ @"union".keys, @"union".reprs },
                        else => return fail(c, .{ .invalid_call_builtin = .{ .builtin = builtin, .args = c.dupe(Repr, &.{ value, fun }) } }),
                    };
                    const tir_funs = c.allocator.alloc(tir.Fun, keys.len) catch oom();
                    for (tir_funs, keys, reprs) |*tir_fun, key, repr| {
                        const args_repr = Repr{ .@"struct" = .{
                            .keys = c.dupe(Value, &.{ .{ .i64 = 0 }, .{ .i64 = 1 } }),
                            .reprs = c.dupe(Repr, &.{ .{ .only = c.box(key) }, repr }),
                        } };
                        tir_fun.* = try inferFun(c, .{
                            .fun = fun.fun.fun,
                            .closure_repr = .{ .@"struct" = fun.fun.closure },
                            .arg_reprs = c.dupe(Repr, &.{args_repr}),
                        });
                    }
                    emit(c, f, switch (value) {
                        .@"struct" => .{ .each_struct = tir_funs },
                        .@"union" => .{ .each_union = tir_funs },
                        else => unreachable,
                    });
                    return Repr.emptyStruct();
                },
                .@"from-only" => {
                    const arg = try inferExpr(c, f, dir_f);
                    if (arg != .only)
                        return fail(c, .{ .invalid_call_builtin = .{ .builtin = builtin, .args = c.dupe(Repr, &.{arg}) } });
                    emit(c, f, .{ .call_builtin = .from_only });
                    return arg.only.reprOf();
                },
                else => return fail(c, .todo),
            }
        },
        .make => {
            const head = try unstage(c, try inferExpr(c, f, dir_f));
            const args = try inferExpr(c, f, dir_f);
            switch (head) {
                .repr => |to_repr| {
                    if (to_repr == .only) {
                        if (args.@"struct".keys.len != 0)
                            return fail(c, .{ .cannot_make = .{ .head = head, .args = args } });
                        emit(c, f, .{ .only = to_repr.only });
                    } else {
                        if (args.@"struct".keys.len != 1 or
                            args.@"struct".keys[0] != .i64 or
                            args.@"struct".keys[0].i64 != 0)
                            return fail(c, .{ .cannot_make = .{ .head = head, .args = args } });
                        emit(c, f, .{ .object_get = .{ .index = 0 } });
                        const from_repr = args.@"struct".reprs[0];
                        if (from_repr.equal(to_repr)) {
                            // nop
                        } else if (from_repr == .i64 and to_repr == .u32) {
                            // TODO We should only allow this cast when from is a constant walue.
                            emit(c, f, .{ .call_builtin = .i64_to_u32 });
                        } else if (to_repr == .@"union" and from_repr == .@"struct") {
                            if (from_repr.@"struct".keys.len != 1)
                                return fail(c, .{ .type_error = .{ .expected = to_repr, .found = from_repr } });
                            const key = from_repr.@"struct".keys[0];
                            const repr = from_repr.@"struct".reprs[0];
                            const tag = to_repr.@"union".get(key) orelse
                                return fail(c, .{ .type_error = .{ .expected = to_repr, .found = from_repr } });
                            if (!repr.equal(to_repr.@"union".reprs[tag]))
                                return fail(c, .{ .type_error = .{ .expected = to_repr, .found = from_repr } });
                            emit(c, f, .{ .object_get = .{ .index = 0 } });
                            emit(c, f, .{ .union_init = .{ .repr = to_repr.@"union", .tag = @intCast(tag) } });
                        } else if (from_repr == .only and from_repr.only.reprOf().equal(to_repr)) {
                            emit(c, f, .{ .call_builtin = .from_only });
                        } else {
                            return fail(c, .{ .type_error = .{ .expected = to_repr, .found = from_repr } });
                        }
                    }
                    return to_repr;
                },
                .repr_kind => panic("TODO {}", .{head}),
                else => return fail(c, .{ .cannot_make_head = .{ .head = head } }),
            }
        },
        .block => |block| {
            var result = Repr.emptyStruct();
            for (0..block.count) |_| {
                result = try inferExpr(c, f, dir_f);
            }
            emit(c, f, .{ .block = .{ .count = block.count } });
            return result;
        },
        .@"if" => {
            const cond_repr = try inferExpr(c, f, dir_f);
            const cond = cond_repr.asBoolish() orelse
                return fail(c, .{ .not_a_bool = cond_repr });
            _ = take(f, dir_f).if_then;
            const then = try inferExpr(c, f, dir_f);
            _ = take(f, dir_f).if_else;
            const @"else" = try inferExpr(c, f, dir_f);
            const repr = switch (cond) {
                .true => then,
                .false => @"else",
                .unknown => repr: {
                    if (!then.equal(@"else"))
                        return fail(c, .{ .type_error = .{ .expected = then, .found = @"else" } });
                    break :repr then;
                },
            };
            emit(c, f, .{ .@"if" = repr });
            return repr;
        },
        .@"while" => {
            _ = take(f, dir_f).while_begin;
            const cond_repr = try inferExpr(c, f, dir_f);
            _ = cond_repr.asBoolish() orelse
                return fail(c, .{ .not_a_bool = cond_repr });
            _ = take(f, dir_f).while_body;
            _ = try inferExpr(c, f, dir_f);
            emit(c, f, .@"while");
            return Repr.emptyStruct();
        },
        .@"return" => {
            const value = try inferExpr(c, f, dir_f);
            _ = try reprUnion(c, &f.return_repr, value);
            emit(c, f, .@"return");
            return Repr.emptyStruct();
        },
        .stage => {
            const infer_mode = c.infer_mode;
            c.infer_mode = .unstage;
            defer c.infer_mode = infer_mode;

            f.dir_expr_next.id -= 1; // untake .stage
            const value = try eval.evalStaged(c, dir_f, f);
            return .{ .only = c.box(value) };
        },
        .unstage => {
            _ = take(f, dir_f).unstage_begin;
            const result = try inferExpr(c, f, dir_f);
            return result;
        },
        .repr_of => {
            // TODO Need to be able to emit repr in wasm.
            return fail(c, .todo);
        },
        else => {
            return fail(c, .todo);
        },
    }
}

fn unstage(c: *Compiler, repr: Repr) !Value {
    const value = repr.valueOf() orelse
        return fail(c, .{ .value_not_staged = repr });
    return value.only.*;
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
        .@"union" => |@"union"| {
            const ix = @"union".get(key) orelse
                return fail(c, .{ .key_not_found = .{ .object = object, .key = key } });
            return .{
                .index = ix,
                .repr = @"union".reprs[ix],
                .offset = @intCast(@"union".tagSizeOf()),
            };
        },
        .ref => unreachable, // always passes through ref_deref first
    }
}

fn take(f: *tir.FunData, dir_f: dir.FunData) dir.ExprData {
    const expr_data = dir_f.expr_data_pre.get(f.dir_expr_next);
    f.dir_expr_next.id += 1;
    return expr_data;
}

fn emit(c: *Compiler, f: *tir.FunData, expr_data: ?tir.ExprData) void {
    if (c.infer_mode == .infer) {
        if (expr_data != null) {
            _ = f.expr_data_post.append(expr_data.?);
        }
    }
}

fn fail(c: *Compiler, data: InferErrorData) error{InferError} {
    c.error_data = .{ .infer = .{
        .key = c.tir_fun_data_next.?.key,
        .expr = .{ .id = c.tir_fun_data_next.?.dir_expr_next.id - 1 },
        .data = data,
    } };
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
    union_never_has_key: struct {
        object: Repr,
        key: Value,
    },
    todo,
};
