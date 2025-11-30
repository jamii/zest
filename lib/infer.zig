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
const dir = zest.dir;
const tir = zest.tir;

const eval = @import("./eval.zig");

pub fn infer(c: *Compiler) error{ EvalError, InferError }!void {
    const print_main = eval.evalRuntimeDefinition(c, "println-main");
    c.infer_mode = .infer;
    c.tir_fun_main = try inferFun(c, .{
        .fun = print_main.fun.repr.fun,
        .closure_repr = Repr.emptyStruct(),
        .arg_reprs = c.dupe(Repr, &.{Repr.emptyStruct()}),
    });
}

fn inferFun(c: *Compiler, key: tir.FunKey) !tir.Fun {
    if (c.tir_fun_by_key.get(key)) |fun|
        return fun;

    const f = c.box(tir.FunData.init(c.allocator, key));
    const fun = c.tir_fun_data.append(f);
    c.tir_fun_by_key.put(key, fun) catch oom();

    const dir_f = c.dir_fun_data.get(key.fun);
    for (dir_f.local_data.items()) |dir_local_data| {
        _ = f.local_data.append(.{
            .repr = null,
            .is_tmp = dir_local_data.is_tmp,
        });
    }

    const infer_context = c.infer_context;
    defer c.infer_context = infer_context;
    c.infer_context = .{
        .dir_expr_next = .{ .id = 0 },
        .key = key,
        .closure = .closure,
        .arg = .arg,
        .local_offset = 0,
        .@"return" = .@"return",
    };

    _ = try inferExpr(c, f, dir_f, .other);
    convertPostorderToPreorder(c, tir.Expr, tir.ExprData, f.expr_data_post, &f.expr_data_pre);
    if (f.return_repr == null)
        f.return_repr = Repr.emptyUnion();

    return fun;
}

fn inferFunInline(c: *Compiler, f: *tir.FunData, key: tir.FunKey, closure_local: tir.Local, arg_locals: []const tir.Local) !Repr {
    assert(key.arg_reprs.len == arg_locals.len);

    const local_offset = f.local_data.count();

    const dir_f = c.dir_fun_data.get(key.fun);
    for (dir_f.local_data.items()) |dir_local_data| {
        _ = f.local_data.append(.{
            .repr = null,
            .is_tmp = dir_local_data.is_tmp,
        });
    }

    const infer_context = c.infer_context;
    defer c.infer_context = infer_context;
    c.infer_context = .{
        .dir_expr_next = .{ .id = 0 },
        .key = key,
        .closure = .{ .local = closure_local },
        .arg = .{ .locals = arg_locals },
        .local_offset = local_offset,
        .@"return" = .{ .@"break" = null },
    };

    _ = try inferExpr(c, f, dir_f, .other);

    return c.infer_context.@"return".@"break" orelse Repr.emptyUnion();
}

pub fn inferExpr(
    c: *Compiler,
    f: *tir.FunData,
    dir_f: dir.FunData,
    dest: tir.Destination,
) error{ InferError, EvalError }!Repr {
    //zest.p(.{ .in, f.key.fun.id, c.infer_context.dir_expr_next.id, dir_f.expr_data_pre.get(.{ .id = c.infer_context.dir_expr_next.id }), dest });
    var repr = try inferExprInner(c, f, dir_f, dest);
    //zest.p(.{ .out, f.key.fun.id, c.infer_context.dir_expr_next.id - 1, dir_f.expr_data_pre.get(.{ .id = c.infer_context.dir_expr_next.id - 1 }), dest, repr });
    if (dest.repr) |dest_repr| {
        try convert(c, f, repr, dest_repr);
        repr = dest_repr;
    }
    try propagate(c, f, dest, repr);
    return repr;
}

fn propagate(
    c: *Compiler,
    f: *tir.FunData,
    dest: tir.Destination,
    found_repr: Repr,
) !void {
    const repr_ptr = switch (dest.location) {
        .@"return" => switch (c.infer_context.@"return") {
            .@"return" => &f.return_repr,
            .@"break" => |*return_repr| return_repr,
        },
        .local => |local| &f.local_data.getPtr(local).repr,
        .other => return,
    };
    if (repr_ptr.*) |expected_repr| {
        if (!expected_repr.equal(found_repr)) {
            return fail(c, .{ .type_error = .{ .expected = expected_repr, .found = found_repr } });
        }
    } else {
        repr_ptr.* = found_repr;
    }
}

fn inferExprInner(
    c: *Compiler,
    f: *tir.FunData,
    dir_f: dir.FunData,
    dest: tir.Destination,
) error{ InferError, EvalError }!Repr {
    const expr_data = take(c, dir_f);
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
                key.* = try unstage(c, try inferExpr(c, f, dir_f, .other));
                var value_repr: ?Repr = null;
                if (dest.repr != null and dest.repr.? == .@"struct") {
                    if (dest.repr.?.@"struct".get(key.*)) |ix| {
                        value_repr = dest.repr.?.@"struct".reprs[ix];
                    }
                }
                repr.* = try inferExpr(c, f, dir_f, .{ .repr = value_repr, .location = .other });
            }
            const result = Repr{ .@"struct" = .{ .keys = keys, .reprs = reprs } };
            emit(c, f, .{ .struct_init = result.@"struct" });
            return result;
        },
        .fun_init => |fun_init| {
            var closure_repr: ?Repr = null;
            if (dest.repr) |dest_repr| {
                if (dest_repr == .fun) {
                    closure_repr = .{ .@"struct" = dest_repr.fun.closure };
                }
            }
            const closure = try inferExpr(c, f, dir_f, .{ .repr = closure_repr, .location = .other });
            return .{ .fun = .{
                .fun = fun_init.fun,
                .closure = closure.@"struct",
            } };
        },
        .arg => |arg| {
            emit(c, f, switch (c.infer_context.arg) {
                .arg => .{ .arg = .{ .id = arg.id } },
                .locals => |locals| .{ .local_get = locals[arg.id] },
            });
            return c.infer_context.key.arg_reprs[arg.id];
        },
        .closure => {
            emit(c, f, switch (c.infer_context.closure) {
                .closure => .closure,
                .local => |local| .{ .local_get = local },
            });
            return c.infer_context.key.closure_repr;
        },
        .local_get => |dir_local| {
            const local = tir.Local{ .id = dir_local.id + c.infer_context.local_offset };
            emit(c, f, .{ .local_get = local });
            // Shouldn't be able to reach get before let.
            return f.local_data.get(local).repr.?;
        },
        .local_let => |dir_local| {
            const local = tir.Local{ .id = dir_local.id + c.infer_context.local_offset };
            _ = try inferExpr(c, f, dir_f, .{ .repr = f.local_data.get(local).repr, .location = .{ .local = local } });
            emit(c, f, .{ .local_let = local });
            return Repr.emptyStruct();
        },
        .ref_init => {
            // The ref type isn't user-visible, so there shouldn't be a way to type this expr.
            assert(dest.repr == null);
            const value = try inferExpr(c, f, dir_f, .other);
            emit(c, f, .{ .ref_init = value });
            return .{ .ref = c.box(value) };
        },
        .assert_object => |assert_object| {
            const value = try inferExpr(c, f, dir_f, dest);
            switch (value) {
                .@"struct" => |@"struct"| {
                    if (@"struct".keys.len != assert_object.count)
                        return fail(c, .{ .wrong_number_of_keys = .{
                            .expected = assert_object.count,
                            .actual = @"struct".keys.len,
                        } });
                },
                .@"union" => return fail(c, .todo),
                .list => return fail(c, .todo), // Need to emit an assert on length
                .u32, .i64, .string, .repr, .repr_kind, .fun, .only, .ref, .namespace => return fail(c, .{ .expected_object = value }),
            }
            return value;
        },
        .assert_is_ref => {
            const value = try inferExpr(c, f, dir_f, dest);
            if (value != .ref)
                return fail(c, .{ .expected_is_ref = value });
            return value;
        },
        .assert_has_no_ref => {
            const value = try inferExpr(c, f, dir_f, dest);
            if (value.hasRef(.any))
                return fail(c, .{ .expected_has_no_ref = value });
            return value;
        },
        .assert_has_no_ref_visible => {
            const value = try inferExpr(c, f, dir_f, dest);
            if (value.hasRef(.visible))
                return fail(c, .{ .expected_has_no_ref = value });
            return value;
        },
        .object_get => {
            const object = try inferExpr(c, f, dir_f, .other);
            const key = try unstage(c, try inferExpr(c, f, dir_f, .other));
            const get = try objectGet(c, object, key);
            emit(c, f, .{ .object_get = .{ .index = get.index } });
            return get.repr;
        },
        .namespace_get => {
            const namespace = try inferExpr(c, f, dir_f, .other);
            const key = try unstage(c, try inferExpr(c, f, dir_f, .other));
            if (namespace != .namespace)
                return fail(c, .{ .not_a_namespace = namespace });
            if (namespace.namespace.namespace.id >= c.namespace_data.count())
                return fail(c, .{ .unknown_namespace = namespace });
            const namespace_data = c.namespace_data.get(namespace.namespace.namespace);
            if (key != .string)
                return fail(c, .{ .definition_not_found = .{ .namespace = namespace, .key = key } });
            const definition = namespace_data.definition_by_name.get(key.string) orelse
                return fail(c, .{ .definition_not_found = .{ .namespace = namespace, .key = key } });
            const definition_data = namespace_data.definition_data.getPtr(definition);
            const value = value: {
                switch (definition_data.value) {
                    .unevaluated => {
                        definition_data.value = .evaluating;
                        eval.pushFun(c, .{
                            .fun = definition_data.fun,
                            .closure = Value.emptyStruct(),
                            .args = &.{},
                            .memo = .{
                                .namespace = namespace.namespace.namespace,
                                .definition = definition,
                            },
                        });
                        c.pure_depth += 1;

                        break :value try eval.eval(c);
                    },
                    .evaluating => return fail(c, .{ .recursive_evaluation = .{ .namespace = namespace, .key = key } }),
                    .evaluated => |value| {
                        break :value value.copy(c.allocator);
                    },
                }
            };
            // TODO This is a hacky way to emit the value.
            emit(c, f, .{ .only = c.box(value) });
            emit(c, f, .{ .call_builtin = .from_only });
            return value.reprOf();
        },
        .ref_get => {
            const ref = try inferExpr(c, f, dir_f, .other);
            const key = try unstage(c, try inferExpr(c, f, dir_f, .other));
            const get = try objectGet(c, ref.ref.*, key);
            emit(c, f, .{ .ref_get = switch (ref.ref.*) {
                .@"struct" => .{ .struct_offset = get.offset },
                .@"union" => .{ .union_tag = @intCast(get.index) },
                else => unreachable,
            } });
            return .{ .ref = c.box(get.repr) };
        },
        .ref_set => {
            const ref = try inferExpr(c, f, dir_f, .other);
            _ = try inferExpr(c, f, dir_f, .{ .repr = ref.ref.*, .location = .other });
            emit(c, f, .ref_set);
            return Repr.emptyStruct();
        },
        .ref_deref => {
            const ref = try inferExpr(c, f, dir_f, .other);
            emit(c, f, .{ .ref_deref = ref.ref.* });
            return ref.ref.*;
        },
        .call => |call| {
            const fun = try inferExpr(c, f, dir_f, .other);
            if (fun != .fun)
                return fail(c, .{ .not_a_fun = fun });

            const must_inline = c.dir_fun_data.get(fun.fun.fun).@"inline";
            var closure_local: ?tir.Local = null;
            if (must_inline) {
                closure_local = f.local_data.append(.{ .repr = .{ .@"struct" = fun.fun.closure }, .is_tmp = true });
                emit(c, f, .{ .local_let = closure_local.? });
            }

            var arg_locals: ?[]tir.Local = null;
            if (must_inline)
                arg_locals = c.allocator.alloc(tir.Local, call.arg_count) catch oom();

            const args = c.allocator.alloc(Repr, call.arg_count) catch oom();
            for (args, 0..) |*arg, i| {
                // TODO Eventually we should interleave this with the type annotations on the params, but it requires storing them separately rather than folded into the wrapper function.
                arg.* = try inferExpr(c, f, dir_f, .other);
                if (must_inline) {
                    const arg_local = f.local_data.append(.{ .repr = arg.*, .is_tmp = true });
                    arg_locals.?[i] = arg_local;
                    emit(c, f, .{ .local_let = arg_local });
                }
            }

            const key = tir.FunKey{
                .fun = fun.fun.fun,
                .closure_repr = .{ .@"struct" = fun.fun.closure },
                .arg_reprs = args,
            };

            if (must_inline) {
                const repr = try inferFunInline(c, f, key, closure_local.?, arg_locals.?);
                emit(c, f, .{ .block = .{ .count = 1 + call.arg_count + 1 } });
                return repr;
            } else {
                const tir_fun = try inferFun(c, key);
                emit(c, f, .{ .call = tir_fun });
                return c.tir_fun_data.get(tir_fun).return_repr orelse
                    fail(c, .{ .recursive_inference = .{ .key = c.infer_context.key } });
            }
        },
        .call_builtin => |builtin| {
            // TODO Should propagate type expectations between args.
            switch (builtin) {
                .equal => {
                    const arg0 = try inferExpr(c, f, dir_f, .other);
                    const arg1 = try inferExpr(c, f, dir_f, .other);
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
                    const arg0 = try inferExpr(c, f, dir_f, .other);
                    const arg1 = try inferExpr(c, f, dir_f, .other);
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
                    const arg0 = try inferExpr(c, f, dir_f, .other);
                    const arg1 = try inferExpr(c, f, dir_f, .other);
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
                    const arg0 = try inferExpr(c, f, dir_f, .other);
                    const arg1 = try inferExpr(c, f, dir_f, .other);
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
                    const arg0 = try inferExpr(c, f, dir_f, .other);
                    const arg1 = try inferExpr(c, f, dir_f, .other);
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
                    const arg0 = try inferExpr(c, f, dir_f, .other);
                    const arg1 = try inferExpr(c, f, dir_f, .other);
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
                    const arg = try inferExpr(c, f, dir_f, .other);
                    if (arg == .i64) {
                        emit(c, f, .{ .call_builtin = .negate_i64 });
                        return .i64;
                    } else {
                        return fail(c, .{ .invalid_call_builtin = .{ .builtin = builtin, .args = c.dupe(Repr, &.{arg}) } });
                    }
                },
                .add => {
                    const arg0 = try inferExpr(c, f, dir_f, .other);
                    const arg1 = try inferExpr(c, f, dir_f, .other);
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
                    const arg0 = try inferExpr(c, f, dir_f, .other);
                    const arg1 = try inferExpr(c, f, dir_f, .other);
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
                    const arg0 = try inferExpr(c, f, dir_f, .other);
                    const arg1 = try inferExpr(c, f, dir_f, .other);
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
                    const arg0 = try inferExpr(c, f, dir_f, .other);
                    const arg1 = try inferExpr(c, f, dir_f, .other);
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
                    const arg0 = try inferExpr(c, f, dir_f, .other);
                    const arg1 = try inferExpr(c, f, dir_f, .other);
                    if (arg0 == .u32 and arg1 == .u32) {
                        emit(c, f, .{ .call_builtin = .bit_shift_left_u32 });
                        return .u32;
                    } else {
                        return fail(c, .{ .invalid_call_builtin = .{ .builtin = builtin, .args = c.dupe(Repr, &.{ arg0, arg1 }) } });
                    }
                },
                .clz => {
                    const arg = try inferExpr(c, f, dir_f, .other);
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
                    const grow_page_count = try inferExpr(c, f, dir_f, .other);
                    if (grow_page_count != .u32)
                        return fail(c, .{ .invalid_call_builtin = .{ .builtin = builtin, .args = c.dupe(Repr, &.{grow_page_count}) } });
                    emit(c, f, .{ .call_builtin = .memory_grow });
                    return .u32;
                },
                .@"memory-fill" => {
                    const to_ptr = try inferExpr(c, f, dir_f, .other);
                    const value = try inferExpr(c, f, dir_f, .other);
                    const byte_count = try inferExpr(c, f, dir_f, .other);
                    if (to_ptr != .u32 or value != .u32 or byte_count != .u32)
                        return fail(c, .{ .invalid_call_builtin = .{ .builtin = builtin, .args = c.dupe(Repr, &.{ to_ptr, value, byte_count }) } });
                    emit(c, f, .{ .call_builtin = .memory_fill });
                    return Repr.emptyStruct();
                },
                .@"memory-copy" => {
                    const to_ptr = try inferExpr(c, f, dir_f, .other);
                    const from_ptr = try inferExpr(c, f, dir_f, .other);
                    const byte_count = try inferExpr(c, f, dir_f, .other);
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
                    const address = try inferExpr(c, f, dir_f, .other);
                    const repr = try unstage(c, try inferExpr(c, f, dir_f, .other));
                    if (address != .u32 or repr != .repr)
                        return fail(c, .{ .invalid_call_builtin = .{ .builtin = builtin, .args = c.dupe(Repr, &.{ address, repr.reprOf() }) } });
                    emit(c, f, .{ .call_builtin = .{ .load = repr.repr } });
                    return repr.repr;
                },
                .store => {
                    const address = try inferExpr(c, f, dir_f, .other);
                    const value = try inferExpr(c, f, dir_f, .other);
                    if (address != .u32)
                        return fail(c, .{ .invalid_call_builtin = .{ .builtin = builtin, .args = c.dupe(Repr, &.{ address, value }) } });
                    emit(c, f, .{ .call_builtin = .store });
                    return Repr.emptyStruct();
                },
                .@"size-of" => {
                    const repr = try unstage(c, try inferExpr(c, f, dir_f, .other));
                    if (repr != .repr)
                        return fail(c, .{ .invalid_call_builtin = .{ .builtin = builtin, .args = c.dupe(Repr, &.{repr.reprOf()}) } });
                    emit(c, f, .{ .call_builtin = .{ .size_of = @intCast(repr.repr.sizeOf()) } });
                    return .u32;
                },
                .print => {
                    const repr = try inferExpr(c, f, dir_f, .other);
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
                    const object = try inferExpr(c, f, dir_f, .other);
                    const key = try unstage(c, try inferExpr(c, f, dir_f, .other));
                    if (object != .@"union")
                        return fail(c, .{ .invalid_call_builtin = .{ .builtin = builtin, .args = c.dupe(Repr, &.{ object, key.reprOf() }) } });
                    const index = object.@"union".get(key) orelse
                        return fail(c, .{ .union_never_has_key = .{ .object = object, .key = key } });
                    emit(c, f, .{ .call_builtin = .{ .union_has_key = @intCast(index) } });
                    return .i64;
                },
                .each => {
                    const value = try inferExpr(c, f, dir_f, .other);
                    const value_local = f.local_data.append(.{ .repr = value, .is_tmp = true });
                    emit(c, f, .{ .local_let = value_local });

                    const fun = try inferExpr(c, f, dir_f, .other);
                    if (fun != .fun)
                        return fail(c, .{ .invalid_call_builtin = .{ .builtin = builtin, .args = c.dupe(Repr, &.{ value, fun }) } });

                    const fun_local = f.local_data.append(.{ .repr = .{ .@"struct" = fun.fun.closure }, .is_tmp = true });
                    emit(c, f, .{ .local_let = fun_local });

                    switch (value) {
                        .@"struct" => |@"struct"| {
                            for (@"struct".keys, @"struct".reprs, 0..) |key, repr, ix| {
                                const args_repr = Repr{ .@"struct" = .{
                                    .keys = c.dupe(Value, &.{ .{ .i64 = 0 }, .{ .i64 = 1 } }),
                                    .reprs = c.dupe(Repr, &.{ .{ .only = c.box(key) }, repr }),
                                } };

                                emit(c, f, .{ .struct_init = Repr.emptyStruct().@"struct" });
                                emit(c, f, .{ .only = c.box(key) });
                                emit(c, f, .{ .local_get = value_local });
                                emit(c, f, .{ .object_get = .{ .index = ix } });
                                emit(c, f, .{ .struct_init = args_repr.@"struct" });
                                const args_local = f.local_data.append(.{ .repr = args_repr, .is_tmp = true });
                                emit(c, f, .{ .local_let = args_local });

                                // TODO This will only inline the wrapper - we want to inline the whole thing.
                                _ = try inferFunInline(
                                    c,
                                    f,
                                    .{
                                        .fun = fun.fun.fun,
                                        .closure_repr = .{ .@"struct" = fun.fun.closure },
                                        .arg_reprs = c.dupe(Repr, &.{args_repr}),
                                    },
                                    fun_local,
                                    &.{args_local},
                                );

                                emit(c, f, .{ .block = .{ .count = 2 } });
                            }

                            emit(c, f, .{ .struct_init = Repr.emptyStruct().@"struct" });
                            emit(c, f, .{ .block = .{ .count = 2 + @"struct".keys.len + 1 } });
                        },
                        .@"union" => |@"union"| {
                            emit(c, f, .{ .local_get = value_local });
                            emit(c, f, .union_tag);
                            const tag_local = f.local_data.append(.{ .repr = .u32, .is_tmp = true });
                            emit(c, f, .{ .local_let = tag_local });

                            for (@"union".keys, @"union".reprs, 0..) |key, repr, ix| {
                                emit(c, f, .{ .local_get = tag_local });
                                emit(c, f, .{ .i64 = @intCast(ix) });
                                emit(c, f, .{ .call_builtin = .i64_to_u32 });
                                emit(c, f, .{ .call_builtin = .equal_u32 });

                                const args_repr = Repr{ .@"struct" = .{
                                    .keys = c.dupe(Value, &.{ .{ .i64 = 0 }, .{ .i64 = 1 } }),
                                    .reprs = c.dupe(Repr, &.{ .{ .only = c.box(key) }, repr }),
                                } };

                                emit(c, f, .{ .struct_init = Repr.emptyStruct().@"struct" });
                                emit(c, f, .{ .only = c.box(key) });
                                emit(c, f, .{ .local_get = value_local });
                                emit(c, f, .{ .object_get = .{ .index = ix } });
                                emit(c, f, .{ .struct_init = args_repr.@"struct" });
                                const args_local = f.local_data.append(.{ .repr = args_repr, .is_tmp = true });
                                emit(c, f, .{ .local_let = args_local });

                                // TODO This will only inline the wrapper - we want to inline the whole thing.
                                _ = try inferFunInline(
                                    c,
                                    f,
                                    .{
                                        .fun = fun.fun.fun,
                                        .closure_repr = .{ .@"struct" = fun.fun.closure },
                                        .arg_reprs = c.dupe(Repr, &.{args_repr}),
                                    },
                                    fun_local,
                                    &.{args_local},
                                );

                                emit(c, f, .{ .struct_init = Repr.emptyStruct().@"struct" });
                                emit(c, f, .{ .block = .{ .count = 3 } });
                            }
                            emit(c, f, .{ .call_builtin = .panic });
                            for (0..@"union".keys.len) |_| {
                                emit(c, f, .{ .@"if" = .emptyStruct() });
                            }
                            emit(c, f, .{ .block = .{ .count = 4 } });
                        },
                        else => return fail(c, .{ .invalid_call_builtin = .{ .builtin = builtin, .args = c.dupe(Repr, &.{ value, fun }) } }),
                    }

                    return Repr.emptyStruct();
                },
                .@"from-only" => {
                    const arg = try inferExpr(c, f, dir_f, .other);
                    if (arg != .only)
                        return fail(c, .{ .invalid_call_builtin = .{ .builtin = builtin, .args = c.dupe(Repr, &.{arg}) } });
                    emit(c, f, .{ .call_builtin = .from_only });
                    return arg.only.reprOf();
                },
                .main => {
                    const closure_local = f.local_data.append(.{ .repr = .emptyStruct(), .is_tmp = true });
                    const arg_local = f.local_data.append(.{ .repr = .emptyStruct(), .is_tmp = true });
                    const repr = try inferFunInline(
                        c,
                        f,
                        .{
                            .fun = c.dir_fun_main.?,
                            .closure_repr = .emptyStruct(),
                            .arg_reprs = c.dupe(Repr, &.{.emptyStruct()}),
                        },
                        closure_local,
                        &.{arg_local},
                    );
                    return repr;
                },
                else => return fail(c, .todo),
            }
        },
        .make => {
            const head = try unstage(c, try inferExpr(c, f, dir_f, .other));
            switch (head) {
                .repr => |to_repr| {
                    // Can propagate before even looking at args - important for constraining return type of recursive functions.
                    try propagate(c, f, dest, to_repr);

                    const args_repr = Repr{ .@"struct" = .{
                        .keys = c.dupe(Value, &.{.{ .i64 = 0 }}),
                        .reprs = c.dupe(Repr, &.{to_repr}),
                    } };
                    const args = try inferExpr(c, f, dir_f, .{ .repr = args_repr, .location = .other });
                    emit(c, f, .{ .object_get = .{ .index = 0 } });
                    try convert(c, f, args.@"struct".reprs[0], to_repr);
                    return to_repr;
                },
                .repr_kind => {
                    const args = try inferExpr(c, f, dir_f, .other);
                    _ = args;
                    panic("TODO {}", .{head});
                },
                else => return fail(c, .{ .cannot_make_head = .{ .head = head } }),
            }
        },
        .block => |block| {
            var result = Repr.emptyStruct();
            for (0..block.count) |i| {
                result = try inferExpr(c, f, dir_f, if (i == block.count - 1) dest else .other);
            }
            emit(c, f, .{ .block = .{ .count = block.count } });
            return result;
        },
        .@"if" => {
            const cond_repr = try inferExpr(c, f, dir_f, .other);
            const cond = cond_repr.asBoolish() orelse
                return fail(c, .{ .not_a_bool = cond_repr });
            switch (cond) {
                .false => {
                    _ = take(c, dir_f).if_then;
                    _ = skipTree(c, dir_f);
                    _ = take(c, dir_f).if_else;
                    const repr = try inferExpr(c, f, dir_f, dest);
                    emit(c, f, .{ .block = .{ .count = 2 } });
                    return repr;
                },
                .true => {
                    _ = take(c, dir_f).if_then;
                    const repr = try inferExpr(c, f, dir_f, dest);
                    _ = take(c, dir_f).if_else;
                    _ = skipTree(c, dir_f);
                    emit(c, f, .{ .block = .{ .count = 2 } });
                    return repr;
                },
                .unknown => {
                    _ = take(c, dir_f).if_then;
                    const then = try inferExpr(c, f, dir_f, dest);
                    _ = take(c, dir_f).if_else;
                    const @"else" = try inferExpr(c, f, dir_f, dest);
                    if (!then.equal(@"else"))
                        return fail(c, .{ .type_error = .{ .expected = then, .found = @"else" } });
                    emit(c, f, .{ .@"if" = then });
                    return then;
                },
            }
        },
        .@"while" => {
            _ = take(c, dir_f).while_begin;
            const cond_repr = try inferExpr(c, f, dir_f, .other);
            _ = cond_repr.asBoolish() orelse
                return fail(c, .{ .not_a_bool = cond_repr });
            _ = take(c, dir_f).while_body;
            _ = try inferExpr(c, f, dir_f, .other);
            emit(c, f, .@"while");
            return Repr.emptyStruct();
        },
        .@"return" => {
            _ = try inferExpr(c, f, dir_f, .{ .repr = f.return_repr, .location = .@"return" });
            switch (c.infer_context.@"return") {
                .@"return" => emit(c, f, .@"return"),
                // TODO Might need to break to specific label rather than just yield.
                .@"break" => {},
            }
            return Repr.emptyStruct();
        },
        .stage => {
            const infer_mode = c.infer_mode;
            c.infer_mode = .unstage;
            defer c.infer_mode = infer_mode;

            c.pure_depth += 1;
            defer c.pure_depth -= 1;

            c.infer_context.dir_expr_next.id -= 1; // untake .stage
            const value = try eval.evalStaged(c, dir_f, f);
            return .{ .only = c.box(value) };
        },
        .unstage => {
            _ = take(c, dir_f).unstage_begin;
            const result = try inferExpr(c, f, dir_f, dest);
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

fn convert(c: *Compiler, f: *tir.FunData, from_repr: Repr, to_repr: Repr) !void {
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
    } else if (to_repr == .list and from_repr == .@"struct") {
        const struct_local = f.local_data.append(.{ .repr = from_repr, .is_tmp = true });
        emit(c, f, .{ .local_let = struct_local });
        const len = from_repr.@"struct".keys.len;
        for (0..len) |to_ix| {
            for (from_repr.@"struct".keys, from_repr.@"struct".reprs, 0..) |key, repr, from_ix| {
                if (key == .i64 and key.i64 == to_ix) {
                    emit(c, f, .{ .local_get = struct_local });
                    emit(c, f, .{ .object_get = .{ .index = from_ix } });
                    try convert(c, f, repr, to_repr.list.elem.*);
                    break;
                }
            } else return fail(c, .{ .type_error = .{ .expected = to_repr, .found = from_repr } });
        }
        emit(c, f, .{ .list_init = .{ .count = len } });
        emit(c, f, .{ .block = .{ .count = 2 } });
    } else if (from_repr == .only and from_repr.only.reprOf().equal(to_repr)) {
        emit(c, f, .{ .call_builtin = .from_only });
    } else if (to_repr == .only and from_repr.isEmptyStruct()) {
        emit(c, f, .{ .only = to_repr.only });
    } else if (to_repr == .namespace and from_repr.isEmptyStruct()) {
        emit(c, f, .{ .namespace = to_repr.namespace });
    } else {
        return fail(c, .{ .type_error = .{ .expected = to_repr, .found = from_repr } });
    }
}

fn unstage(c: *Compiler, repr: Repr) !Value {
    const value = repr.valueOf() orelse
        return fail(c, .{ .value_not_staged = repr });
    return value.only.*;
}

fn objectGet(c: *Compiler, object: Repr, key: Value) error{InferError}!struct { index: usize, repr: Repr, offset: u32 } {
    switch (object) {
        .u32, .i64, .string, .repr, .repr_kind, .fun, .only, .namespace => return fail(c, .{ .expected_object = object }),
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
        .list => return fail(c, .todo), // emit a list_get
        .ref => unreachable, // always passes through ref_deref first
    }
}

fn take(c: *Compiler, dir_f: dir.FunData) dir.ExprData {
    const expr_data = dir_f.expr_data_pre.get(c.infer_context.dir_expr_next);
    c.infer_context.dir_expr_next.id += 1;
    return expr_data;
}

fn skipTree(c: *Compiler, dir_f: dir.FunData) dir.Expr {
    const start = c.infer_context.dir_expr_next;
    var children_remaining: usize = 1;
    while (children_remaining > 0) {
        children_remaining += take(c, dir_f).childCount(c);
        children_remaining -= 1;
    }
    return start;
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
        .key = c.infer_context.key,
        .expr = .{ .id = c.infer_context.dir_expr_next.id - 1 },
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
    not_a_namespace: Repr,
    unknown_namespace: Repr,
    definition_not_found: struct {
        namespace: Repr,
        key: Value,
    },
    recursive_evaluation: struct {
        namespace: Repr,
        key: Value,
    },
    recursive_inference: struct {
        key: tir.FunKey,
    },
    todo,
};
