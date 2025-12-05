const std = @import("std");
const panic = std.debug.panic;
const assert = std.debug.assert;
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;

const zest = @import("./zest.zig");
const oom = zest.oom;
const deepEqual = zest.deepEqual;
const treePart = zest.treePart;
const List = zest.List;
const Compiler = zest.Compiler;
const Value = zest.Value;
const Repr = zest.Repr;
const Builtin = zest.Builtin;
const dir = zest.dir;
const tir = zest.tir;

const infer = @import("./infer.zig");

pub fn evalMain(c: *Compiler) error{EvalError}!Value {
    assert(c.dir_frame_stack.items.len == 0);
    defer c.dir_frame_stack.shrinkRetainingCapacity(0);

    assert(c.value_stack.items.len == 0);
    defer c.value_stack.shrinkRetainingCapacity(0);

    pushFun(c, .{
        .fun = c.dir_fun_main.?,
        .closure = Value.emptyStruct(),
        .args = &.{},
    });
    return eval(c);
}

pub fn evalRuntimeDefinition(c: *Compiler, name: []const u8) Value {
    const runtime = c.namespace_by_origin.get(.runtime).?;
    const runtime_data = c.namespace_data.get(runtime);
    const definition = runtime_data.definition_by_name.get(name).?;
    const definition_data = runtime_data.definition_data.get(definition);

    switch (definition_data.value) {
        .unevaluated => {
            c.pure_depth += 1;
            pushFun(c, .{
                .fun = definition_data.fun,
                .closure = Value.emptyStruct(),
                .args = &.{},
                .memo = .{
                    .namespace = runtime,
                    .definition = definition,
                },
            });
            return eval(c) catch |err| panic("{}", .{err});
        },
        .evaluating => panic("{}", .{
            fail(c, .{ .recursive_evaluation = .{
                .namespace = .{ .namespace = .{ .namespace = runtime } },
                .key = .{ .string = name },
            } }),
        }),
        .evaluated => |value| return value,
    }
}

pub fn pushFun(c: *Compiler, frame: dir.Frame) void {
    c.dir_frame_stack.append(frame) catch oom();
    c.local_stack.appendNTimes(
        Value.emptyStruct(),
        c.dir_fun_data.get(frame.fun).local_data.count(),
    ) catch oom();
}

fn popFun(c: *Compiler) dir.Frame {
    const frame = c.dir_frame_stack.pop().?;
    c.local_stack.shrinkRetainingCapacity(
        c.local_stack.items.len -
            c.dir_fun_data.get(frame.fun).local_data.count(),
    );
    return frame;
}

pub fn evalStaged(c: *Compiler, f: dir.FunData, tir_f: *tir.FunData) error{ EvalError, InferError }!Value {
    pushFun(c, .{
        .fun = c.infer_context.key.fun,
        .expr = f.expr_data_pre.get(c.infer_context.dir_expr_next).stage.mapping,
        .args = &.{},
        .closure = Value.emptyStruct(),
    });
    var stages_nested: usize = 0;
    while (true) {
        const frame = &c.dir_frame_stack.items[c.dir_frame_stack.items.len - 1];
        const expr_data = f.expr_data_post.get(frame.expr);
        frame.expr.id += 1;
        //zest.p(.{ .eval_staged = expr_data, .values = c.value_stack.items });
        switch (expr_data) {
            .stage_begin => {
                c.pure_depth += 1;
                stages_nested += 1;
            },
            .stage => |stage| {
                c.pure_depth -= 1;
                const value = c.value_stack.pop().?;
                stages_nested -= 1;
                if (stages_nested == 0) {
                    c.infer_context.dir_expr_next = stage.mapping;
                    _ = popFun(c);
                    return value;
                } else {
                    c.value_stack.append(.{ .only = c.box(value) }) catch oom();
                }
            },
            .unstage_begin => |unstage_begin| {
                c.infer_context.dir_expr_next = unstage_begin.mapping;
                c.infer_context.dir_expr_next.id += 1;
                const repr = try infer.inferExpr(c, tir_f, f, .other);
                const value = repr.valueOf() orelse return fail(c, .{ .cannot_unstage_value = repr });
                c.value_stack.append(value) catch oom();
                frame.expr = f.expr_data_pre.get(unstage_begin.mapping).unstage_begin.mapping;
            },
            .unstage => {},
            .repr_of_begin => |repr_of_begin| {
                frame.expr.id += 1;
                c.infer_context.dir_expr_next = repr_of_begin.mapping;
                c.infer_context.dir_expr_next.id += 1;
                const repr = try infer.inferExpr(c, tir_f, f, .other);
                c.value_stack.append(.{ .repr = repr }) catch oom();
                frame.expr = f.expr_data_pre.get(repr_of_begin.mapping).repr_of_begin.mapping;
                assert(f.expr_data_post.get(frame.expr) == .repr_of);
            },
            .repr_of => {},
            .@"return", .arg, .closure => {
                return fail(c, .cannot_stage_expr);
            },
            else => {
                const action = try evalExpr(c, expr_data);
                switch (action) {
                    .next => {},
                    .call => {
                        const return_value = try eval(c);
                        c.value_stack.append(return_value) catch oom();
                    },
                }
            },
        }
    }
}

pub fn eval(c: *Compiler) error{EvalError}!Value {
    const start_frame_index = c.dir_frame_stack.items.len;
    fun: while (true) {
        while (true) {
            const frame = &c.dir_frame_stack.items[c.dir_frame_stack.items.len - 1];
            const f = c.dir_fun_data.get(frame.fun);
            const expr_data = f.expr_data_post.get(frame.expr);
            //zest.p(.{ .fun = frame.fun, .expr = expr_data, .values = c.value_stack.items });
            switch (expr_data) {
                .@"return" => {
                    const frame_evalled = popFun(c);
                    if (frame_evalled.memo) |memo| {
                        const value = c.value_stack.items[c.value_stack.items.len - 1];
                        c.namespace_data.get(memo.namespace).definition_data.getPtr(memo.definition).value = .{ .evaluated = value.copy(c.allocator) };
                        c.pure_depth -= 1;
                    }
                    if (c.dir_frame_stack.items.len < start_frame_index)
                        return c.value_stack.pop().?;
                    const frame_prev = &c.dir_frame_stack.items[c.dir_frame_stack.items.len - 1];
                    frame_prev.expr.id += 1;
                    continue :fun;
                },
                .stage_begin => {
                    c.pure_depth += 1;
                },
                .stage => {
                    c.pure_depth -= 1;
                    const value = c.value_stack.pop().?;
                    c.value_stack.append(.{ .only = c.box(value) }) catch oom();
                },
                .repr_of => {
                    const arg = c.value_stack.pop().?;
                    c.value_stack.append(.{ .repr = arg.reprOf() }) catch oom();
                },
                .repr_of_begin,
                .unstage,
                .unstage_begin,
                => {},
                else => {
                    const action = try evalExpr(c, expr_data);
                    switch (action) {
                        .next => {},
                        .call => continue :fun,
                    }
                },
            }
            c.dir_frame_stack.items[c.dir_frame_stack.items.len - 1].expr.id += 1;
        }
    }
}

const EvalAction = enum {
    next,
    call,
};

pub fn evalExpr(
    c: *Compiler,
    expr_data: dir.ExprData,
) error{EvalError}!EvalAction {
    switch (expr_data) {
        .i64 => |i| {
            c.value_stack.append(.{ .i64 = i }) catch oom();
        },
        .string => |string| {
            c.value_stack.append(.{ .string = string }) catch oom();
        },
        .repr_u32 => {
            c.value_stack.append(.{ .repr = .u32 }) catch oom();
        },
        .repr_i64 => {
            c.value_stack.append(.{ .repr = .i64 }) catch oom();
        },
        .repr_string => {
            c.value_stack.append(.{ .repr = .string }) catch oom();
        },
        .repr_any => {
            c.value_stack.append(.{ .repr = .any }) catch oom();
        },
        .repr_repr => {
            c.value_stack.append(.{ .repr = .repr }) catch oom();
        },
        .repr_repr_kind => {
            c.value_stack.append(.{ .repr = .@"repr-kind" }) catch oom();
        },
        .repr_kind_struct => {
            c.value_stack.append(.{ .@"repr-kind" = .@"struct" }) catch oom();
        },
        .repr_kind_union => {
            c.value_stack.append(.{ .@"repr-kind" = .@"union" }) catch oom();
        },
        .repr_kind_list => {
            c.value_stack.append(.{ .@"repr-kind" = .list }) catch oom();
        },
        .repr_kind_fun => {
            c.value_stack.append(.{ .@"repr-kind" = .fun }) catch oom();
        },
        .repr_kind_only => {
            c.value_stack.append(.{ .@"repr-kind" = .only }) catch oom();
        },
        .repr_kind_namespace => {
            c.value_stack.append(.{ .@"repr-kind" = .namespace }) catch oom();
        },
        .struct_init => |struct_init| {
            const keys = c.allocator.alloc(Value, struct_init.count) catch oom();
            const reprs = c.allocator.alloc(Repr, struct_init.count) catch oom();
            const values = c.allocator.alloc(Value, struct_init.count) catch oom();
            for (0..struct_init.count) |i| {
                const ix = struct_init.count - 1 - i;
                values[ix] = c.value_stack.pop().?;
                keys[ix] = c.value_stack.pop().?.only.*;
                reprs[ix] = values[ix].reprOf();
            }
            // TODO sort
            c.value_stack.append(.{ .@"struct" = .{
                .repr = .{
                    .keys = keys,
                    .reprs = reprs,
                },
                .values = values,
            } }) catch oom();
        },
        .fun_init => |fun_init| {
            const closure = c.value_stack.pop().?;
            c.value_stack.append(.{ .fun = .{
                .repr = .{
                    .fun = fun_init.fun,
                    .closure = closure.@"struct".repr,
                    .state = .valid,
                },
                .closure = closure.@"struct".values,
            } }) catch oom();
        },
        .arg => |arg| {
            const frame = c.dir_frame_stack.items[c.dir_frame_stack.items.len - 1];
            c.value_stack.append(frame.args[arg.id]) catch oom();
        },
        .closure => {
            const frame = c.dir_frame_stack.items[c.dir_frame_stack.items.len - 1];
            c.value_stack.append(frame.closure) catch oom();
        },
        .namespace => |namespace| {
            c.value_stack.append(.{ .namespace = .{ .namespace = namespace } }) catch oom();
        },
        .local_get => |local| {
            const value = c.local_stack.items[c.local_stack.items.len - 1 - local.id];
            c.value_stack.append(value) catch oom();
        },
        .local_let => |local| {
            const value = c.value_stack.pop().?;
            c.local_stack.items[c.local_stack.items.len - 1 - local.id] = value;
            c.value_stack.append(Value.emptyStruct()) catch oom();
        },
        .assert_object => |assert_object| {
            const value = c.value_stack.pop().?;
            switch (value) {
                .@"struct" => |@"struct"| {
                    if (@"struct".values.len != assert_object.count)
                        return fail(c, .{ .wrong_number_of_keys = .{
                            .expected = assert_object.count,
                            .actual = @"struct".values.len,
                        } });
                },
                .@"union" => return fail(c, .todo),
                .list => |list| {
                    if (list.elems.items.len != assert_object.count)
                        return fail(c, .{ .wrong_number_of_keys = .{
                            .expected = assert_object.count,
                            .actual = list.elems.items.len,
                        } });
                },
                .u32, .i64, .string, .repr, .@"repr-kind", .fun, .only, .any, .ref, .namespace => return fail(c, .{ .expected_object = value }),
            }
            c.value_stack.append(value) catch oom();
        },
        .assert_is_ref => {
            const value = c.value_stack.pop().?;
            if (value != .ref)
                return fail(c, .{ .expected_is_ref = value });
            c.value_stack.append(value) catch oom();
        },
        .assert_has_no_ref => {
            const value = c.value_stack.pop().?;
            if (value.reprOf().hasRef(.any))
                return fail(c, .{ .expected_has_no_ref = value });
            c.value_stack.append(value) catch oom();
        },
        .assert_has_no_ref_visible => {
            const value = c.value_stack.pop().?;
            if (value.reprOf().hasRef(.visible))
                return fail(c, .{ .expected_has_no_ref = value });
            c.value_stack.append(value) catch oom();
        },
        .object_get => {
            const key = c.value_stack.pop().?.only.*;
            const object = c.value_stack.pop().?;
            const value = object.get(key) orelse
                return fail(c, .{ .key_not_found = .{ .object = object, .key = key } });
            c.value_stack.append(value) catch oom();
        },
        .namespace_get => {
            const key = c.value_stack.pop().?.only.*;
            const namespace = c.value_stack.pop().?;
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
            switch (definition_data.value) {
                .unevaluated => {
                    definition_data.value = .evaluating;
                    pushFun(c, .{
                        .fun = definition_data.fun,
                        .closure = Value.emptyStruct(),
                        .args = &.{},
                        .memo = .{
                            .namespace = namespace.namespace.namespace,
                            .definition = definition,
                        },
                    });
                    c.pure_depth += 1;
                    return .call;
                },
                .evaluating => return fail(c, .{ .recursive_evaluation = .{ .namespace = namespace, .key = key } }),
                .evaluated => |value| c.value_stack.append(value.copy(c.allocator)) catch oom(),
            }
        },
        .ref_init => {
            const value = c.value_stack.pop().?;
            c.value_stack.append(.{ .ref = .{
                .repr = c.box(value.reprOf()),
                .value = c.box(value.copy(c.allocator)),
            } }) catch oom();
        },
        .ref_set => {
            const value = c.value_stack.pop().?;
            const ref = c.value_stack.pop().?;
            const input_repr = value.reprOf();
            if (!ref.ref.repr.equal(input_repr))
                return fail(c, .{ .type_error = .{
                    .expected = ref.ref.repr.*,
                    .found = input_repr,
                } });
            ref.ref.value.* = value.copy(c.allocator);
            c.value_stack.append(Value.emptyStruct()) catch oom();
        },
        .ref_get => {
            const key = c.value_stack.pop().?.only.*;
            const ref = c.value_stack.pop().?;
            const value_ptr = ref.ref.value.getMut(key) orelse
                return fail(c, .{ .key_not_found = .{ .object = ref.ref.value.*, .key = key } });
            c.value_stack.append(.{ .ref = .{
                .repr = c.box(value_ptr.reprOf()),
                .value = value_ptr,
            } }) catch oom();
        },
        .ref_deref => {
            const ref = c.value_stack.pop().?;
            c.value_stack.append(ref.ref.value.copy(c.allocator)) catch oom();
        },
        .call => |call| {
            const args = c.allocator.alloc(Value, call.arg_count) catch oom();
            for (0..call.arg_count) |i| {
                const ix = call.arg_count - 1 - i;
                args[ix] = c.value_stack.pop().?;
            }
            const fun = c.value_stack.pop().?;
            if (fun != .fun)
                return fail(c, .{ .not_a_fun = fun });
            if (!isValidFun(c, fun.fun.repr))
                return fail(c, .todo);
            pushFun(c, .{
                .fun = fun.fun.repr.fun,
                .closure = .{ .@"struct" = fun.fun.getClosure() },
                .args = args,
            });
            return .call;
        },
        .call_builtin => |builtin| {
            switch (builtin) {
                .equal => {
                    const arg1 = c.value_stack.pop().?;
                    const arg0 = c.value_stack.pop().?;
                    c.value_stack.append(.{ .i64 = if (arg0.equal(arg1)) 1 else 0 }) catch oom();
                },
                .@"not-equal" => {
                    const arg1 = c.value_stack.pop().?;
                    const arg0 = c.value_stack.pop().?;
                    c.value_stack.append(.{ .i64 = if (!arg0.equal(arg1)) 1 else 0 }) catch oom();
                },
                .@"less-than" => {
                    const arg1 = c.value_stack.pop().?;
                    const arg0 = c.value_stack.pop().?;
                    if (arg0 == .i64 and arg1 == .i64) {
                        c.value_stack.append(.{ .i64 = if (arg0.i64 < arg1.i64) 1 else 0 }) catch oom();
                    } else if (arg0 == .u32 and arg1 == .u32) {
                        c.value_stack.append(.{ .i64 = if (arg0.u32 < arg1.u32) 1 else 0 }) catch oom();
                    } else {
                        return fail(c, .{ .invalid_call_builtin = .{ .builtin = builtin, .args = c.dupe(Value, &.{ arg0, arg1 }) } });
                    }
                },
                .@"less-than-or-equal" => {
                    const arg1 = c.value_stack.pop().?;
                    const arg0 = c.value_stack.pop().?;
                    if (arg0 == .i64 and arg1 == .i64) {
                        c.value_stack.append(.{ .i64 = if (arg0.i64 <= arg1.i64) 1 else 0 }) catch oom();
                    } else if (arg0 == .u32 and arg1 == .u32) {
                        c.value_stack.append(.{ .i64 = if (arg0.u32 <= arg1.u32) 1 else 0 }) catch oom();
                    } else {
                        return fail(c, .{ .invalid_call_builtin = .{ .builtin = builtin, .args = c.dupe(Value, &.{ arg0, arg1 }) } });
                    }
                },
                .@"more-than" => {
                    const arg1 = c.value_stack.pop().?;
                    const arg0 = c.value_stack.pop().?;
                    if (arg0 == .i64 and arg1 == .i64) {
                        c.value_stack.append(.{ .i64 = if (arg0.i64 > arg1.i64) 1 else 0 }) catch oom();
                    } else if (arg0 == .u32 and arg1 == .u32) {
                        c.value_stack.append(.{ .i64 = if (arg0.u32 > arg1.u32) 1 else 0 }) catch oom();
                    } else {
                        return fail(c, .{ .invalid_call_builtin = .{ .builtin = builtin, .args = c.dupe(Value, &.{ arg0, arg1 }) } });
                    }
                },
                .@"more-than-or-equal" => {
                    const arg1 = c.value_stack.pop().?;
                    const arg0 = c.value_stack.pop().?;
                    if (arg0 == .i64 and arg1 == .i64) {
                        c.value_stack.append(.{ .i64 = if (arg0.i64 >= arg1.i64) 1 else 0 }) catch oom();
                    } else if (arg0 == .u32 and arg1 == .u32) {
                        c.value_stack.append(.{ .i64 = if (arg0.u32 >= arg1.u32) 1 else 0 }) catch oom();
                    } else {
                        return fail(c, .{ .invalid_call_builtin = .{ .builtin = builtin, .args = c.dupe(Value, &.{ arg0, arg1 }) } });
                    }
                },
                .negate => {
                    const arg = c.value_stack.pop().?;
                    if (arg == .i64) {
                        c.value_stack.append(.{ .i64 = -arg.i64 }) catch oom();
                    } else {
                        return fail(c, .{ .invalid_call_builtin = .{ .builtin = builtin, .args = c.dupe(Value, &.{arg}) } });
                    }
                },
                .add => {
                    const arg1 = c.value_stack.pop().?;
                    const arg0 = c.value_stack.pop().?;
                    if (arg0 == .i64 and arg1 == .i64) {
                        c.value_stack.append(.{ .i64 = arg0.i64 +% arg1.i64 }) catch oom();
                    } else if (arg0 == .u32 and arg1 == .u32) {
                        c.value_stack.append(.{ .u32 = arg0.u32 +% arg1.u32 }) catch oom();
                    } else {
                        return fail(c, .{ .invalid_call_builtin = .{ .builtin = builtin, .args = c.dupe(Value, &.{ arg0, arg1 }) } });
                    }
                },
                .subtract => {
                    const arg1 = c.value_stack.pop().?;
                    const arg0 = c.value_stack.pop().?;
                    if (arg0 == .i64 and arg1 == .i64) {
                        c.value_stack.append(.{ .i64 = arg0.i64 -% arg1.i64 }) catch oom();
                    } else if (arg0 == .u32 and arg1 == .u32) {
                        c.value_stack.append(.{ .u32 = arg0.u32 -% arg1.u32 }) catch oom();
                    } else {
                        return fail(c, .{ .invalid_call_builtin = .{ .builtin = builtin, .args = c.dupe(Value, &.{ arg0, arg1 }) } });
                    }
                },
                .multiply => {
                    const arg1 = c.value_stack.pop().?;
                    const arg0 = c.value_stack.pop().?;
                    if (arg0 == .i64 and arg1 == .i64) {
                        c.value_stack.append(.{ .i64 = arg0.i64 *% arg1.i64 }) catch oom();
                    } else if (arg0 == .u32 and arg1 == .u32) {
                        c.value_stack.append(.{ .u32 = arg0.u32 *% arg1.u32 }) catch oom();
                    } else {
                        return fail(c, .{ .invalid_call_builtin = .{ .builtin = builtin, .args = c.dupe(Value, &.{ arg0, arg1 }) } });
                    }
                },
                .remainder => {
                    const arg1 = c.value_stack.pop().?;
                    const arg0 = c.value_stack.pop().?;
                    if (arg0 == .i64 and arg1 == .i64) {
                        if (arg1.i64 == 0) return fail(c, .division_by_zero);
                        c.value_stack.append(.{ .i64 = @rem(arg0.i64, arg1.i64) }) catch oom();
                    } else if (arg0 == .u32 and arg1 == .u32) {
                        if (arg1.u32 == 0) return fail(c, .division_by_zero);
                        c.value_stack.append(.{ .u32 = @rem(arg0.u32, arg1.u32) }) catch oom();
                    } else {
                        return fail(c, .{ .invalid_call_builtin = .{ .builtin = builtin, .args = c.dupe(Value, &.{ arg0, arg1 }) } });
                    }
                },
                .@"bit-shift-left" => {
                    const arg1 = c.value_stack.pop().?;
                    const arg0 = c.value_stack.pop().?;
                    if (arg0 == .u32 and arg1 == .u32) {
                        c.value_stack.append(.{ .u32 = arg0.u32 << @truncate(arg1.u32) }) catch oom();
                    } else {
                        return fail(c, .{ .invalid_call_builtin = .{ .builtin = builtin, .args = c.dupe(Value, &.{ arg0, arg1 }) } });
                    }
                },
                .clz => {
                    const arg = c.value_stack.pop().?;
                    if (arg == .u32) {
                        c.value_stack.append(.{ .u32 = @clz(arg.u32) }) catch oom();
                    } else {
                        return fail(c, .{ .invalid_call_builtin = .{ .builtin = builtin, .args = c.dupe(Value, &.{arg}) } });
                    }
                },
                .@"memory-size" => {
                    const page_count = @divExact(c.memory.items.len, std.wasm.page_size);
                    c.value_stack.append(.{ .u32 = @intCast(page_count) }) catch oom();
                },
                .@"memory-grow" => {
                    // TODO Should we have a limit on heap size?
                    const page_count = @divExact(c.memory.items.len, std.wasm.page_size);
                    const grow_page_count = c.value_stack.pop().?;
                    if (grow_page_count != .u32)
                        return fail(c, .{ .invalid_call_builtin = .{ .builtin = builtin, .args = c.dupe(Value, &.{grow_page_count}) } });
                    c.memory.appendNTimes(0, @as(usize, @intCast(grow_page_count.u32)) * std.wasm.page_size) catch oom();
                    c.value_stack.append(.{ .u32 = @intCast(page_count) }) catch oom();
                },
                .@"memory-fill" => {
                    const byte_count = c.value_stack.pop().?;
                    const value = c.value_stack.pop().?;
                    const to_ptr = c.value_stack.pop().?;
                    if (to_ptr != .u32 or value != .u32 or byte_count != .u32)
                        return fail(c, .{ .invalid_call_builtin = .{ .builtin = builtin, .args = c.dupe(Value, &.{ to_ptr, value, byte_count }) } });
                    @memset(
                        c.memory.items[@intCast(to_ptr.u32)..][0..@intCast(byte_count.u32)],
                        @truncate(value.u32),
                    );
                    c.value_stack.append(Value.emptyStruct()) catch oom();
                },
                .@"memory-copy" => {
                    const byte_count = c.value_stack.pop().?;
                    const from_ptr = c.value_stack.pop().?;
                    const to_ptr = c.value_stack.pop().?;
                    if (to_ptr != .u32 or from_ptr != .u32 or byte_count != .u32)
                        return fail(c, .{ .invalid_call_builtin = .{ .builtin = builtin, .args = c.dupe(Value, &.{ to_ptr, from_ptr, byte_count }) } });
                    if (to_ptr.u32 < from_ptr.u32) {
                        std.mem.copyForwards(
                            u8,
                            c.memory.items[@intCast(to_ptr.u32)..],
                            c.memory.items[@intCast(from_ptr.u32)..][0..@intCast(byte_count.u32)],
                        );
                    } else {
                        std.mem.copyBackwards(
                            u8,
                            c.memory.items[@intCast(to_ptr.u32)..],
                            c.memory.items[@intCast(from_ptr.u32)..][0..@intCast(byte_count.u32)],
                        );
                    }
                    c.value_stack.append(Value.emptyStruct()) catch oom();
                },
                .@"heap-start" => {
                    // This must be kept in sync with ./runtime.zs
                    const wasm_page_byte_count_log = 16;
                    const class_min_byte_count_log = 3;
                    const class_count = 32 - class_min_byte_count_log;
                    const class_small_count = wasm_page_byte_count_log - class_min_byte_count_log;
                    // Need space for:
                    //   alloc_free_ptr: array[class_count, u32]
                    //   alloc_next_ptr: array[class_small_count, u32]
                    c.value_stack.append(.{ .u32 = class_count + class_small_count }) catch oom();
                },
                .load => {
                    const repr = c.value_stack.pop().?.only.*;
                    const address = c.value_stack.pop().?;
                    if (address != .u32 or repr != .repr)
                        return fail(c, .{ .invalid_call_builtin = .{ .builtin = builtin, .args = c.dupe(Value, &.{ address, repr }) } });
                    const address_usize = try boundsCheck(c, .load, address.u32, repr.repr);
                    const loaded = Value.load(c.allocator, c.memory.items[address_usize..], repr.repr);
                    c.value_stack.append(loaded) catch oom();
                },
                .store => {
                    const value = c.value_stack.pop().?;
                    const address = c.value_stack.pop().?;
                    if (address != .u32)
                        return fail(c, .{ .invalid_call_builtin = .{ .builtin = builtin, .args = c.dupe(Value, &.{ address, value }) } });
                    const address_usize = try boundsCheck(c, .store, address.u32, value.reprOf());
                    value.store(c.memory.items[address_usize..]);
                    c.value_stack.append(Value.emptyStruct()) catch oom();
                },
                .@"size-of" => {
                    const repr = c.value_stack.pop().?.only.*;
                    if (repr != .repr)
                        return fail(c, .{ .invalid_call_builtin = .{ .builtin = builtin, .args = c.dupe(Value, &.{repr}) } });
                    const size = repr.repr.sizeOf();
                    c.value_stack.append(.{ .u32 = @intCast(size) }) catch oom();
                },
                .print => {
                    const value = c.value_stack.pop().?;
                    if (c.pure_depth > 0)
                        return fail(c, .{ .side_effects_in_pure_eval = .print });
                    switch (value) {
                        .u32 => |u| c.printed.writer().print("{}", .{u}) catch oom(),
                        .i64 => |i| c.printed.writer().print("{}", .{i}) catch oom(),
                        .string => |string| c.printed.appendSlice(string) catch oom(),
                        else => return fail(c, .{ .invalid_call_builtin = .{ .builtin = builtin, .args = c.dupe(Value, &.{value}) } }),
                    }
                    c.value_stack.append(Value.emptyStruct()) catch oom();
                },
                .panic => {
                    return fail(c, .panic);
                },
                .@"union-has-key" => {
                    const key = c.value_stack.pop().?.only.*;
                    const object = c.value_stack.pop().?;
                    if (object != .@"union")
                        return fail(c, .{ .invalid_call_builtin = .{ .builtin = builtin, .args = c.dupe(Value, &.{ object, key }) } });
                    const index = object.@"union".repr.get(key) orelse
                        return fail(c, .{ .union_never_has_key = .{ .object = object, .key = key } });
                    c.value_stack.append(.{ .i64 = if (object.@"union".tag == index) 1 else 0 }) catch oom();
                },
                .@"from-any" => {
                    const value = c.value_stack.pop().?;
                    if (value != .any)
                        return fail(c, .{ .invalid_call_builtin = .{ .builtin = builtin, .args = c.dupe(Value, &.{value}) } });
                    c.value_stack.append(value.any.copy(c.allocator)) catch oom();
                },
                .only => {
                    // Already handled by staging annotation.
                },
                .@"from-only" => {
                    const value = c.value_stack.pop().?;
                    if (value != .only)
                        return fail(c, .{ .invalid_call_builtin = .{ .builtin = builtin, .args = c.dupe(Value, &.{value}) } });
                    c.value_stack.append(value.only.copy(c.allocator)) catch oom();
                },
                .each => panic("Unreachable - desugared to dir.ExprData.each instead", .{}),
                .main => {
                    pushFun(c, .{
                        .fun = c.dir_fun_main.?,
                        .closure = .emptyStruct(),
                        .args = c.dupe(Value, &.{.emptyStruct()}),
                    });
                    return .call;
                },
                .unmake => {
                    const arg = c.value_stack.pop().?;
                    if (arg != .repr)
                        return fail(c, .{ .invalid_call_builtin = .{ .builtin = builtin, .args = c.dupe(Value, &.{arg}) } });
                    var head: ?Value = null;
                    var args: ?Value = null;
                    switch (arg.repr) {
                        .@"struct" => |@"struct"| {
                            const reprs = c.allocator.alloc(Repr, @"struct".keys.len) catch oom();
                            for (reprs) |*repr| repr.* = .repr;
                            const values = c.allocator.alloc(Value, @"struct".keys.len) catch oom();
                            for (values, @"struct".reprs) |*value, repr| value.* = .{ .repr = repr };
                            head = Value{ .@"repr-kind" = .@"struct" };
                            args = Value{ .@"struct" = .{
                                .repr = .{
                                    .keys = @"struct".keys,
                                    .reprs = reprs,
                                },
                                .values = values,
                            } };
                        },
                        .@"union" => |@"union"| {
                            const reprs = c.allocator.alloc(Repr, @"union".keys.len) catch oom();
                            for (reprs) |*repr| repr.* = .repr;
                            const values = c.allocator.alloc(Value, @"union".keys.len) catch oom();
                            for (values, @"union".reprs) |*value, repr| value.* = .{ .repr = repr };
                            head = Value{ .@"repr-kind" = .@"union" };
                            args = Value{ .@"struct" = .{
                                .repr = .{
                                    .keys = @"union".keys,
                                    .reprs = reprs,
                                },
                                .values = values,
                            } };
                        },
                        .list => |list| {
                            head = Value{ .@"repr-kind" = .list };
                            args = Value{ .@"struct" = .{
                                .repr = .{
                                    .keys = c.dupe(Value, &.{.{ .i64 = 0 }}),
                                    .reprs = c.dupe(Repr, &.{.repr}),
                                },
                                .values = c.dupe(Value, &.{.{ .repr = list.elem.* }}),
                            } };
                        },
                        .fun => |fun| {
                            const keys = c.allocator.alloc(Value, fun.closure.keys.len + 1) catch oom();
                            keys[0] = .{ .i64 = 0 };
                            for (keys[1..], fun.closure.keys) |*key, closure_key| key.* = closure_key;
                            const reprs = c.allocator.alloc(Repr, fun.closure.keys.len + 1) catch oom();
                            reprs[0] = .i64;
                            for (reprs[1..]) |*repr| repr.* = .repr;
                            const values = c.allocator.alloc(Value, fun.closure.keys.len + 1) catch oom();
                            values[0] = .{ .i64 = @intCast(fun.fun.id) };
                            for (values[1..], fun.closure.reprs) |*value, repr| value.* = .{ .repr = repr };
                            head = Value{ .@"repr-kind" = .fun };
                            args = Value{ .@"struct" = .{
                                .repr = .{
                                    .keys = keys,
                                    .reprs = reprs,
                                },
                                .values = values,
                            } };
                        },
                        .namespace => |namespace| {
                            head = Value{ .@"repr-kind" = .namespace };
                            args = Value{ .@"struct" = .{
                                .repr = .{
                                    .keys = c.dupe(Value, &.{.{ .i64 = 0 }}),
                                    .reprs = c.dupe(Repr, &.{.i64}),
                                },
                                .values = c.dupe(Value, &.{.{ .i64 = @intCast(namespace.namespace.id) }}),
                            } };
                        },
                        .only => |only| {
                            head = Value{ .@"repr-kind" = .only };
                            args = Value{ .@"struct" = .{
                                .repr = .{
                                    .keys = c.dupe(Value, &.{.{ .i64 = 0 }}),
                                    .reprs = c.dupe(Repr, &.{only.reprOf()}),
                                },
                                .values = c.dupe(Value, &.{only.*}),
                            } };
                        },
                        else => return fail(c, .{ .invalid_call_builtin = .{ .builtin = builtin, .args = c.dupe(Value, &.{arg}) } }),
                    }
                    c.value_stack.append(.{ .@"struct" = .{
                        .repr = .{
                            .keys = c.dupe(Value, &.{ .{ .i64 = 0 }, .{ .i64 = 1 } }),
                            .reprs = c.dupe(Repr, &.{ head.?.reprOf(), args.?.reprOf() }),
                        },
                        .values = c.dupe(Value, &.{ head.?, args.? }),
                    } }) catch oom();
                },
                .closure => {
                    const arg = c.value_stack.pop().?;
                    switch (arg) {
                        .fun => |fun| {
                            c.value_stack.append(.{ .@"struct" = .{
                                .repr = fun.repr.closure,
                                .values = fun.closure,
                            } }) catch oom();
                        },
                        .namespace => {
                            c.value_stack.append(.emptyStruct()) catch oom();
                        },
                        else => return fail(c, .{ .invalid_call_builtin = .{ .builtin = builtin, .args = c.dupe(Value, &.{arg}) } }),
                    }
                },
                else => return fail(c, .todo),
            }
        },
        .make => {
            const args = c.value_stack.pop().?;
            const head = c.value_stack.pop().?.only.*;
            switch (head) {
                .repr => |to_repr| {
                    if (args.@"struct".repr.keys.len != 1 or
                        args.@"struct".repr.keys[0] != .i64 or
                        args.@"struct".repr.keys[0].i64 != 0)
                        return fail(c, .{ .cannot_make = .{ .head = head, .args = args } });
                    const from_repr = args.@"struct".repr.reprs[0];
                    const from_value = args.@"struct".values[0];
                    const to_value = try convert(c, from_value, from_repr, to_repr);
                    c.value_stack.append(to_value) catch oom();
                },
                .@"repr-kind" => |repr_kind| switch (repr_kind) {
                    .@"struct" => {
                        const reprs = c.allocator.alloc(Repr, args.@"struct".values.len) catch oom();
                        for (reprs, args.@"struct".values) |*repr, value| {
                            if (value != .repr)
                                return fail(c, .{ .cannot_make = .{ .head = head, .args = args } });
                            repr.* = value.repr;
                        }
                        c.value_stack.append(.{ .repr = .{ .@"struct" = .{ .keys = args.@"struct".repr.keys, .reprs = reprs } } }) catch oom();
                    },
                    .@"union" => {
                        const reprs = c.allocator.alloc(Repr, args.@"struct".values.len) catch oom();
                        for (reprs, args.@"struct".values) |*repr, value| {
                            if (value != .repr)
                                return fail(c, .{ .cannot_make = .{ .head = head, .args = args } });
                            repr.* = value.repr;
                        }
                        c.value_stack.append(.{ .repr = .{ .@"union" = .{ .keys = args.@"struct".repr.keys, .reprs = reprs } } }) catch oom();
                    },
                    .list => {
                        if (args.@"struct".repr.keys.len != 1 or
                            args.@"struct".repr.keys[0] != .i64 or
                            args.@"struct".repr.keys[0].i64 != 0 or
                            args.@"struct".values[0] != .repr)
                            return fail(c, .{ .cannot_make = .{ .head = head, .args = args } });
                        const repr = args.@"struct".values[0].repr;
                        c.value_stack.append(.{ .repr = .{ .list = .{ .elem = c.box(repr) } } }) catch oom();
                    },
                    .only => {
                        if (args.@"struct".repr.keys.len != 1 or
                            args.@"struct".repr.keys[0] != .i64 or
                            args.@"struct".repr.keys[0].i64 != 0)
                            return fail(c, .{ .cannot_make = .{ .head = head, .args = args } });
                        const value = args.@"struct".values[0];
                        c.value_stack.append(.{ .repr = .{ .only = c.box(value) } }) catch oom();
                    },
                    .fun => {
                        if (args.@"struct".repr.keys.len == 0 or
                            args.@"struct".repr.keys[0] != .i64 or
                            args.@"struct".repr.keys[0].i64 != 0 or
                            args.@"struct".values[0] != .i64)
                            return fail(c, .{ .cannot_make = .{ .head = head, .args = args } });
                        const id = args.@"struct".values[0].i64;
                        const reprs = c.allocator.alloc(Repr, args.@"struct".values[1..].len) catch oom();
                        for (reprs, args.@"struct".repr.keys[1..], args.@"struct".values[1..]) |*repr, key, value| {
                            if (key != .string or
                                !zest.isName(key.string) or
                                value != .repr)
                                return fail(c, .{ .cannot_make = .{ .head = head, .args = args } });
                            repr.* = value.repr;
                        }
                        c.value_stack.append(.{
                            .repr = .{ .fun = .{
                                .fun = .{ .id = @intCast(id) },
                                .closure = .{
                                    .keys = args.@"struct".repr.keys[1..],
                                    .reprs = reprs,
                                },
                                .state = .unknown,
                            } },
                        }) catch oom();
                    },
                    .namespace => {
                        if (args.@"struct".repr.keys.len != 1 or
                            args.@"struct".repr.keys[0] != .i64 or
                            args.@"struct".repr.keys[0].i64 != 0 or
                            args.@"struct".values[0] != .i64)
                            return fail(c, .{ .cannot_make = .{ .head = head, .args = args } });
                        const id_i64 = args.@"struct".values[0].i64;
                        const id = std.math.cast(usize, id_i64) orelse
                            return fail(c, .{ .cannot_make = .{ .head = head, .args = args } });
                        c.value_stack.append(.{ .repr = .{ .namespace = .{ .namespace = .{ .id = id } } } }) catch oom();
                    },
                },
                else => return fail(c, .{ .cannot_make_head = .{ .head = head } }),
            }
        },
        .block => |block| {
            if (block.count == 0) {
                c.value_stack.append(Value.emptyStruct()) catch oom();
            } else {
                const value = c.value_stack.pop().?;
                for (0..block.count - 1) |_| {
                    _ = c.value_stack.pop().?;
                }
                c.value_stack.append(value) catch oom();
            }
        },
        .if_then => {
            const cond_value = c.value_stack.pop().?;
            const cond = cond_value.asBool() orelse
                return fail(c, .{ .not_a_bool = cond_value });
            if (!cond)
                skipTree(c, .if_else, .if_then);
        },
        .if_else => {
            skipTree(c, .@"if", .if_else);
        },
        .@"if" => {},
        .while_begin => {},
        .while_body => {
            const cond_value = c.value_stack.pop().?;
            const cond = cond_value.asBool() orelse
                return fail(c, .{ .not_a_bool = cond_value });
            if (!cond) {
                c.value_stack.append(Value.emptyStruct()) catch oom();
                skipTree(c, .@"while", .while_body);
            }
        },
        .@"while" => |@"while"| {
            _ = c.value_stack.pop().?;
            const frame = &c.dir_frame_stack.items[c.dir_frame_stack.items.len - 1];
            frame.expr = @"while".begin;
        },
        .each_begin => {
            c.value_stack.append(.{ .i64 = 0 }) catch oom();
            // Dummy value
            c.value_stack.append(.emptyStruct()) catch oom();
        },
        .each_body => {
            // Dummy value, or result of call on last iteration.
            _ = c.value_stack.pop().?;
            const ix = c.value_stack.pop().?.i64;
            const fun = c.value_stack.pop().?;
            const value = c.value_stack.pop().?;
            if (fun != .fun)
                return fail(c, .{ .invalid_call_builtin = .{ .builtin = .each, .args = c.dupe(Value, &.{ value, fun }) } });

            const len = switch (value) {
                .@"struct" => |@"struct"| @"struct".values.len,
                .@"union" => 1,
                .list => |list| list.elems.items.len,
                .only => |only| switch (only.*) {
                    .@"struct" => |@"struct"| @"struct".values.len,
                    .@"union" => 1,
                    .list => |list| list.elems.items.len,
                    else => return fail(c, .{ .invalid_call_builtin = .{ .builtin = .each, .args = c.dupe(Value, &.{ value, fun }) } }),
                },
                else => return fail(c, .{ .invalid_call_builtin = .{ .builtin = .each, .args = c.dupe(Value, &.{ value, fun }) } }),
            };
            if (ix >= len) {
                c.value_stack.append(Value.emptyStruct()) catch oom();
                skipTree(c, .each, .each_body);
                return .next;
            }

            c.value_stack.append(value) catch oom();
            c.value_stack.append(fun) catch oom();
            c.value_stack.append(.{ .i64 = ix + 1 }) catch oom();

            const ix_value: Value = switch (value) {
                .@"struct", .@"union" => .{ .only = c.box(Value{ .i64 = ix }) },
                .list => .{ .i64 = ix },
                .only => |only| switch (only.*) {
                    .@"struct", .@"union" => .{ .only = c.box(Value{ .i64 = ix }) },
                    .list => .{ .only = c.box(Value{ .i64 = ix }) },
                    else => unreachable,
                },
                else => unreachable,
            };
            const key: Value = switch (value) {
                .@"struct" => |@"struct"| .{ .only = c.box(@"struct".repr.keys[@intCast(ix)]) },
                .@"union" => |@"union"| .{ .only = c.box(@"union".repr.keys[@"union".tag]) },
                .list => .{ .i64 = ix },
                .only => |only| switch (only.*) {
                    .@"struct" => |@"struct"| .{ .only = c.box(@"struct".repr.keys[@intCast(ix)]) },
                    .@"union" => |@"union"| .{ .only = c.box(@"union".repr.keys[@"union".tag]) },
                    .list => .{ .only = c.box(Value{ .i64 = ix }) },
                    else => unreachable,
                },
                else => unreachable,
            };
            const val: Value = switch (value) {
                .@"struct" => |@"struct"| @"struct".values[@intCast(ix)],
                .@"union" => |@"union"| @"union".value.*,
                .list => |list| list.elems.items[@intCast(ix)],
                .only => |only| switch (only.*) {
                    .@"struct" => |@"struct"| .{ .only = c.box(@"struct".values[@intCast(ix)]) },
                    .@"union" => |@"union"| .{ .only = c.box(@"union".value.*) },
                    .list => |list| .{ .only = c.box(list.elems.items[@intCast(ix)]) },
                    else => unreachable,
                },
                else => unreachable,
            };
            const args = Value{ .@"struct" = .{
                .repr = .{
                    .keys = c.dupe(Value, &.{ .{ .i64 = 0 }, .{ .i64 = 1 }, .{ .i64 = 2 } }),
                    .reprs = c.dupe(Repr, &.{ ix_value.reprOf(), key.reprOf(), val.reprOf() }),
                },
                .values = c.dupe(Value, &.{ ix_value, key, val }),
            } };
            pushFun(c, .{
                .fun = fun.fun.repr.fun,
                .closure = .{ .@"struct" = fun.fun.getClosure() },
                .args = c.dupe(Value, &.{args}),
            });
            return .call;
        },
        .each => {
            const frame = &c.dir_frame_stack.items[c.dir_frame_stack.items.len - 1];
            // Go back to each_body.
            frame.expr.id -= 2;
        },
        .@"return", .stage, .stage_begin, .repr_of, .repr_of_begin, .unstage, .unstage_begin => panic("Can't eval control flow expr: {}", .{expr_data}),
        .f64 => return fail(c, .todo),
    }
    return .next;
}

fn convert(c: *Compiler, from_value: Value, from_repr: Repr, to_repr: Repr) !Value {
    if (from_repr.equal(to_repr)) {
        return from_value;
    } else if (from_repr == .i64 and to_repr == .u32) {
        if (std.math.cast(u32, from_value.i64)) |converted| {
            return .{ .u32 = converted };
        } else {
            return fail(c, .{ .convert_error = .{ .expected = to_repr, .found = from_value } });
        }
    } else if (to_repr == .@"union" and from_repr == .@"struct") {
        if (from_repr.@"struct".keys.len != 1)
            return fail(c, .{ .type_error = .{ .expected = to_repr, .found = from_repr } });
        const key = from_repr.@"struct".keys[0];
        const value = from_value.@"struct".values[0];
        const tag = to_repr.@"union".get(key) orelse
            return fail(c, .{ .type_error = .{ .expected = to_repr, .found = from_repr } });
        if (!value.reprOf().equal(to_repr.@"union".reprs[tag]))
            return fail(c, .{ .type_error = .{ .expected = to_repr, .found = from_repr } });
        return .{ .@"union" = .{ .repr = to_repr.@"union", .tag = tag, .value = c.box(value) } };
    } else if (to_repr == .list and from_repr == .@"struct") {
        const len = from_repr.@"struct".keys.len;
        var elems = ArrayList(Value).initCapacity(c.allocator, len) catch oom();
        _ = elems.addManyAsSliceAssumeCapacity(len);
        for (from_repr.@"struct".keys, from_value.@"struct".values, from_repr.@"struct".reprs) |key, value, repr| {
            // This relies on structs not being allowed to have repeated keys to ensure that all elems are initialized.
            if (key != .i64 or key.i64 < 0 or key.i64 >= len)
                return fail(c, .{ .type_error = .{ .expected = to_repr, .found = from_repr } });
            elems.items[@intCast(key.i64)] = try convert(c, value, repr, to_repr.list.elem.*);
        }
        return .{ .list = .{ .repr = to_repr.list, .elems = elems } };
    } else if (to_repr == .fun and from_repr == .@"struct") {
        if (from_repr.@"struct".keys.len != to_repr.fun.closure.keys.len)
            return fail(c, .{ .type_error = .{ .expected = to_repr, .found = from_repr } });
        for (from_repr.@"struct".keys, to_repr.fun.closure.keys) |from_key, to_key|
            if (!from_key.equal(to_key))
                return fail(c, .{ .type_error = .{ .expected = to_repr, .found = from_repr } });
        const closure = c.allocator.alloc(Value, from_repr.@"struct".keys.len) catch oom();
        for (closure, from_value.@"struct".values, from_repr.@"struct".reprs, to_repr.fun.closure.reprs) |*to_value_inner, from_value_inner, from_repr_inner, to_repr_inner|
            to_value_inner.* = try convert(c, from_value_inner, from_repr_inner, to_repr_inner);
        return .{ .fun = .{
            .repr = to_repr.fun,
            .closure = closure,
        } };
    } else if (to_repr == .any) {
        return .{ .any = c.box(from_value) };
    } else if (from_repr == .only and from_repr.only.reprOf().equal(to_repr)) {
        return from_repr.only.copy(c.allocator);
    } else if (to_repr == .only and from_repr.isEmptyStruct()) {
        return .{ .only = to_repr.only };
    } else if (to_repr == .namespace and from_repr.isEmptyStruct()) {
        return .{ .namespace = .{ .namespace = to_repr.namespace.namespace } };
    } else {
        return fail(c, .{ .type_error = .{ .expected = to_repr, .found = from_repr } });
    }
}

pub fn isValidFun(c: *Compiler, fun: zest.ReprFun) bool {
    _ = c;
    switch (fun.state) {
        .valid => return true,
        // TODO Actually validate.
        .unknown => return false,
    }
}

fn skipTree(c: *Compiler, expect_next: std.meta.Tag(dir.ExprData), ignore_after: std.meta.Tag(dir.ExprData)) void {
    const frame = &c.dir_frame_stack.items[c.dir_frame_stack.items.len - 1];
    const f = c.dir_fun_data.get(frame.fun);
    var ignores: usize = 0;
    while (true) {
        frame.expr.id += 1;
        const expr_data = f.expr_data_post.get(frame.expr);
        if (expr_data == expect_next) {
            if (ignores == 0) break;
            ignores -= 1;
        }
        if (expr_data == ignore_after) {
            ignores += 1;
        }
    }
}

const BoundsCheckOp = enum { load, store };

fn boundsCheck(c: *Compiler, op: BoundsCheckOp, address: i64, repr: Repr) error{EvalError}!usize {
    if (address < 0)
        return fail(c, .{ .out_of_bounds = .{
            .op = op,
            .repr = repr,
            .address = address,
        } });
    const address_usize = @as(usize, @intCast(address));
    if (address_usize < 0 or
        address_usize > c.memory.items.len or
        c.memory.items.len - address_usize < repr.sizeOf())
        return fail(c, .{ .out_of_bounds = .{
            .op = op,
            .repr = repr,
            .address = address,
        } });
    return address_usize;
}

fn fail(c: *Compiler, data: EvalErrorData) error{EvalError} {
    const frame = c.dir_frame_stack.items[c.dir_frame_stack.items.len - 1];
    c.error_data = .{ .eval = .{ .fun = frame.fun, .expr = frame.expr, .data = data } };
    return error.EvalError;
}

pub const EvalErrorData = union(enum) {
    type_error: struct {
        expected: Repr,
        found: Repr,
    },
    convert_error: struct {
        expected: Repr,
        found: Value,
    },
    key_not_found: struct {
        object: Value,
        key: Value,
    },
    wrong_number_of_keys: struct {
        expected: usize,
        actual: usize,
    },
    expected_object: Value,
    expected_is_ref: Value,
    expected_has_no_ref: Value,
    not_a_fun: Value,
    not_a_bool: Value,
    cannot_stage_expr,
    cannot_unstage_value: Repr,
    invalid_call_builtin: struct {
        builtin: Builtin,
        args: []Value,
    },
    cannot_make: struct {
        head: Value,
        args: Value,
    },
    cannot_make_head: struct {
        head: Value,
    },
    cannot_unmake: Value,
    out_of_bounds: struct {
        op: BoundsCheckOp,
        repr: Repr,
        address: i64,
    },
    division_by_zero,
    panic,
    union_never_has_key: struct {
        object: Value,
        key: Value,
    },
    not_a_namespace: Value,
    unknown_namespace: Value,
    definition_not_found: struct {
        namespace: Value,
        key: Value,
    },
    recursive_evaluation: struct {
        namespace: Value,
        key: Value,
    },
    side_effects_in_pure_eval: Builtin,
    todo,
};
