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

fn pushFun(c: *Compiler, frame: dir.Frame) void {
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
        .fun = tir_f.key.fun,
        .expr = f.expr_data_pre.get(tir_f.dir_expr_next).stage.mapping,
        .args = &.{},
        .closure = Value.emptyStruct(),
    });
    var stages_nested: usize = 0;
    while (true) {
        const frame = &c.dir_frame_stack.items[c.dir_frame_stack.items.len - 1];
        const expr_data = f.expr_data_post.get(frame.expr);
        //zest.p(.{ .eval_staged = expr_data });
        switch (expr_data) {
            .call => |call| {
                const args = c.allocator.alloc(Value, call.arg_count) catch oom();
                for (0..call.arg_count) |i| {
                    const ix = call.arg_count - 1 - i;
                    args[ix] = c.value_stack.pop().?;
                }
                const fun = c.value_stack.pop().?;
                if (fun != .fun)
                    return fail(c, .{ .not_a_fun = fun });
                pushFun(c, .{
                    .fun = fun.fun.repr.fun,
                    .closure = .{ .@"struct" = fun.fun.getClosure() },
                    .args = args,
                });
                const return_value = try eval(c);
                c.value_stack.append(return_value) catch oom();
            },
            .stage_begin => {
                stages_nested += 1;
            },
            .stage => |stage| {
                const value = c.value_stack.pop().?;
                stages_nested -= 1;
                if (stages_nested == 0) {
                    tir_f.dir_expr_next = stage.mapping;
                    _ = popFun(c);
                    return value;
                } else {
                    c.value_stack.append(.{ .only = c.box(value) }) catch oom();
                }
            },
            .unstage_begin => |unstage_begin| {
                tir_f.dir_expr_next = unstage_begin.mapping;
                tir_f.dir_expr_next.id += 1;
                const repr = try infer.inferExpr(c, tir_f, f);
                const value = repr.valueOf() orelse return fail(c, .{ .cannot_unstage_value = repr });
                c.value_stack.append(value) catch oom();
                frame.expr = f.expr_data_pre.get(unstage_begin.mapping).unstage_begin.mapping;
            },
            .unstage => {},
            .repr_of_begin => |repr_of_begin| {
                frame.expr.id += 1;
                tir_f.dir_expr_next = repr_of_begin.mapping;
                tir_f.dir_expr_next.id += 1;
                const repr = try infer.inferExpr(c, tir_f, f);
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
        frame.expr.id += 1;
    }
}

pub fn eval(c: *Compiler) error{EvalError}!Value {
    const start_frame_index = c.dir_frame_stack.items.len;
    fun: while (true) {
        const frame = &c.dir_frame_stack.items[c.dir_frame_stack.items.len - 1];
        const f = c.dir_fun_data.get(frame.fun);
        while (true) {
            const expr_data = f.expr_data_post.get(frame.expr);
            //zest.p(.{ .eval = expr_data });
            switch (expr_data) {
                .@"return" => {
                    _ = popFun(c);
                    if (c.dir_frame_stack.items.len < start_frame_index)
                        return c.value_stack.pop().?;
                    const frame_prev = &c.dir_frame_stack.items[c.dir_frame_stack.items.len - 1];
                    const expr_data_prev = c.dir_fun_data.get(frame_prev.fun).expr_data_post.get(frame_prev.expr);
                    if (expr_data_prev == .call_builtin and expr_data_prev.call_builtin == .each) {
                        // Pop call result.
                        _ = c.value_stack.pop().?;
                        // Call each again.
                    } else {
                        frame_prev.expr.id += 1;
                    }
                    continue :fun;
                },
                .stage_begin => {},
                .stage => {
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
            frame.expr.id += 1;
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
        .repr_repr => {
            c.value_stack.append(.{ .repr = .repr }) catch oom();
        },
        .repr_kind_struct => {
            c.value_stack.append(.{ .repr_kind = .@"struct" }) catch oom();
        },
        .repr_kind_union => {
            c.value_stack.append(.{ .repr_kind = .@"union" }) catch oom();
        },
        .repr_kind_only => {
            c.value_stack.append(.{ .repr_kind = .only }) catch oom();
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
                .u32, .i64, .string, .repr, .repr_kind, .fun, .only, .ref => return fail(c, .{ .expected_object = value }),
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
                    if (arg0 == .i64 and arg1 == .i64) {
                        c.value_stack.append(.{ .i64 = if (arg0.i64 == arg1.i64) 1 else 0 }) catch oom();
                    } else if (arg0 == .u32 and arg1 == .u32) {
                        c.value_stack.append(.{ .i64 = if (arg0.u32 == arg1.u32) 1 else 0 }) catch oom();
                    } else {
                        return fail(c, .{ .invalid_call_builtin = .{ .builtin = builtin, .args = c.dupe(Value, &.{ arg0, arg1 }) } });
                    }
                },
                .@"not-equal" => {
                    const arg1 = c.value_stack.pop().?;
                    const arg0 = c.value_stack.pop().?;
                    if (arg0 == .i64 and arg1 == .i64) {
                        c.value_stack.append(.{ .i64 = if (arg0.i64 != arg1.i64) 1 else 0 }) catch oom();
                    } else if (arg0 == .u32 and arg1 == .u32) {
                        c.value_stack.append(.{ .i64 = if (arg0.u32 != arg1.u32) 1 else 0 }) catch oom();
                    } else {
                        return fail(c, .{ .invalid_call_builtin = .{ .builtin = builtin, .args = c.dupe(Value, &.{ arg0, arg1 }) } });
                    }
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
                .reflect => {
                    const value = c.value_stack.pop().?;
                    if (value != .repr)
                        return fail(c, .{ .invalid_call_builtin = .{ .builtin = builtin, .args = c.dupe(Value, &.{value}) } });
                    const tag = @intFromEnum(std.meta.activeTag(value.repr));
                    c.value_stack.append(.{ .@"union" = .{
                        .repr = c.reflection,
                        .tag = tag,
                        .value = c.box(Value.emptyStruct()),
                    } }) catch oom();
                },
                .@"from-only" => {
                    const value = c.value_stack.pop().?;
                    if (value != .only)
                        return fail(c, .{ .invalid_call_builtin = .{ .builtin = builtin, .args = c.dupe(Value, &.{value}) } });
                    c.value_stack.append(value.only.copy(c.allocator)) catch oom();
                },
                .each => {
                    const fun = c.value_stack.pop().?;
                    const value = c.value_stack.pop().?;
                    if (fun != .fun)
                        return fail(c, .{ .invalid_call_builtin = .{ .builtin = builtin, .args = c.dupe(Value, &.{ value, fun }) } });
                    switch (value) {
                        .@"struct" => |@"struct"| {
                            if (@"struct".values.len == 0) {
                                c.value_stack.append(Value.emptyStruct()) catch oom();
                                return .next;
                            }
                            c.value_stack.append(.{ .@"struct" = .{
                                .repr = .{
                                    .keys = @"struct".repr.keys[1..],
                                    .reprs = @"struct".repr.reprs[1..],
                                },
                                .values = @"struct".values[1..],
                            } }) catch oom();
                            c.value_stack.append(fun) catch oom();
                            const key = Value{ .only = c.box(@"struct".repr.keys[0]) };
                            const val = @"struct".values[0];
                            const args = Value{ .@"struct" = .{
                                .repr = .{
                                    .keys = c.dupe(Value, &.{ .{ .i64 = 0 }, .{ .i64 = 1 } }),
                                    .reprs = c.dupe(Repr, &.{ key.reprOf(), val.reprOf() }),
                                },
                                .values = c.dupe(Value, &.{ key, val }),
                            } };
                            pushFun(c, .{
                                .fun = fun.fun.repr.fun,
                                .closure = .{ .@"struct" = fun.fun.getClosure() },
                                .args = c.dupe(Value, &.{args}),
                            });
                            return .call;
                        },
                        .@"union" => |@"union"| {
                            c.value_stack.append(Value.emptyStruct()) catch oom();
                            c.value_stack.append(fun) catch oom();
                            const key = Value{ .only = c.box(@"union".repr.keys[@"union".tag]) };
                            const val = @"union".value.*;
                            const args = Value{ .@"struct" = .{
                                .repr = .{
                                    .keys = c.dupe(Value, &.{ .{ .i64 = 0 }, .{ .i64 = 1 } }),
                                    .reprs = c.dupe(Repr, &.{ key.reprOf(), val.reprOf() }),
                                },
                                .values = c.dupe(Value, &.{ key, val }),
                            } };
                            pushFun(c, .{
                                .fun = fun.fun.repr.fun,
                                .closure = .{ .@"struct" = fun.fun.getClosure() },
                                .args = c.dupe(Value, &.{args}),
                            });
                            return .call;
                        },
                        else => return fail(c, .{ .invalid_call_builtin = .{ .builtin = builtin, .args = c.dupe(Value, &.{ value, fun }) } }),
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
                    if (to_repr == .only) {
                        if (args.@"struct".repr.keys.len != 0)
                            return fail(c, .{ .cannot_make = .{ .head = head, .args = args } });
                        c.value_stack.append(.{ .only = to_repr.only }) catch oom();
                    } else {
                        if (args.@"struct".repr.keys.len != 1 or
                            args.@"struct".repr.keys[0] != .i64 or
                            args.@"struct".repr.keys[0].i64 != 0)
                            return fail(c, .{ .cannot_make = .{ .head = head, .args = args } });
                        const from_repr = args.@"struct".repr.reprs[0];
                        const from_value = args.@"struct".values[0];
                        if (from_repr.equal(to_repr)) {
                            c.value_stack.append(from_value) catch oom();
                        } else if (from_repr == .i64 and to_repr == .u32) {
                            if (std.math.cast(u32, from_value.i64)) |converted| {
                                c.value_stack.append(.{ .u32 = converted }) catch oom();
                            } else {
                                return fail(c, .{ .convert_error = .{ .expected = to_repr, .found = from_value } });
                            }
                        } else if (to_repr == .@"union" and from_repr == .@"struct") {
                            if (from_repr.@"struct".keys.len != 1)
                                return fail(c, .{ .type_error = .{ .expected = to_repr, .found = from_repr } });
                            const key = from_repr.@"struct".keys[0];
                            const value = args.@"struct".values[0].@"struct".values[0];
                            const tag = to_repr.@"union".get(key) orelse
                                return fail(c, .{ .type_error = .{ .expected = to_repr, .found = from_repr } });
                            if (!value.reprOf().equal(to_repr.@"union".reprs[tag]))
                                return fail(c, .{ .type_error = .{ .expected = to_repr, .found = from_repr } });
                            c.value_stack.append(.{ .@"union" = .{ .repr = to_repr.@"union", .tag = tag, .value = c.box(value) } }) catch oom();
                        } else if (from_repr == .only and from_repr.only.reprOf().equal(to_repr)) {
                            c.value_stack.append(from_repr.only.copy(c.allocator)) catch oom();
                        } else {
                            return fail(c, .{ .type_error = .{ .expected = to_repr, .found = from_repr } });
                        }
                    }
                },
                .repr_kind => |repr_kind| switch (repr_kind) {
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
                    .only => {
                        if (args.@"struct".repr.keys.len != 1 or
                            args.@"struct".repr.keys[0] != .i64 or
                            args.@"struct".repr.keys[0].i64 != 0)
                            return fail(c, .{ .cannot_make = .{ .head = head, .args = args } });
                        const value = args.@"struct".values[0];
                        c.value_stack.append(.{ .repr = .{ .only = c.box(value) } }) catch oom();
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
        .while_begin => {
            const frame = c.dir_frame_stack.items[c.dir_frame_stack.items.len - 1];
            c.while_stack.append(frame.expr) catch oom();
        },
        .while_body => {
            const cond_value = c.value_stack.pop().?;
            const cond = cond_value.asBool() orelse
                return fail(c, .{ .not_a_bool = cond_value });
            if (!cond) {
                _ = c.while_stack.pop().?;
                c.value_stack.append(Value.emptyStruct()) catch oom();
                skipTree(c, .@"while", .while_body);
            }
        },
        .@"while" => {
            _ = c.value_stack.pop().?;
            const frame = &c.dir_frame_stack.items[c.dir_frame_stack.items.len - 1];
            frame.expr = c.while_stack.items[c.while_stack.items.len - 1];
        },
        .@"return", .stage, .stage_begin, .repr_of, .repr_of_begin, .unstage, .unstage_begin => panic("Can't eval control flow expr: {}", .{expr_data}),
        .f64 => return fail(c, .todo),
    }
    return .next;
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

fn boundsCheck(c: *Compiler, op: anytype, address: i64, repr: Repr) error{EvalError}!usize {
    if (address < 0)
        return fail(c, .{ .out_of_bounds = .{
            .op = .load,
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
    out_of_bounds: struct {
        op: enum { load, store },
        repr: Repr,
        address: i64,
    },
    division_by_zero,
    panic,
    union_never_has_key: struct {
        object: Value,
        key: Value,
    },
    todo,
};
