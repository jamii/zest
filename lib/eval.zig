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

pub fn pushFun(c: *Compiler, frame: dir.Frame) void {
    c.dir_frame_stack.append(frame) catch oom();
    c.local_stack.appendNTimes(
        Value.emptyStruct(),
        c.dir_fun_data.get(frame.fun).local_data.count(),
    ) catch oom();
}

pub fn popFun(c: *Compiler) dir.Frame {
    const frame = c.dir_frame_stack.pop();
    c.local_stack.shrinkRetainingCapacity(
        c.local_stack.items.len -
            c.dir_fun_data.get(frame.fun).local_data.count(),
    );
    return frame;
}

pub fn evalStaged(c: *Compiler, tir_f: *tir.FunData, arg_reprs: []Repr, closure_repr: Repr) error{EvalError}!Value {
    const frame = &c.dir_frame_stack.items[c.dir_frame_stack.items.len - 1];
    const f = c.dir_fun_data.get(frame.fun);

    var ends_remaining: usize = 0;

    while (true) {
        const expr_data = f.expr_data.get(frame.expr);
        switch (expr_data) {
            .call_end => |call_end| {
                const args = c.allocator.alloc(Value, call_end.arg_count) catch oom();
                for (0..call_end.arg_count) |i| {
                    const ix = call_end.arg_count - 1 - i;
                    args[ix] = c.value_stack.pop();
                }
                const fun = c.value_stack.pop();
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
            // TODO This is a simple version.
            //      To make things like `stage(repr-of(a.b.c))` work we'd need to switch back to infer.
            //      Also have to limit to exprs with no side-effects or panics.
            .unstage_begin => {
                frame.expr.id += 1;
                const value = switch (f.expr_data.get(frame.expr)) {
                    .local_get => |local| value: {
                        const local_repr = tir_f.local_data.get(.{ .id = local.id }).repr.one;
                        break :value local_repr.valueOf() orelse
                            return fail(c, .{ .cannot_unstage_value = local_repr });
                    },
                    .arg => |arg| arg_reprs[arg.id].valueOf() orelse
                        return fail(c, .{ .cannot_unstage_value = arg_reprs[arg.id] }),
                    .closure => closure_repr.valueOf() orelse
                        return fail(c, .{ .cannot_unstage_value = closure_repr }),
                    else => |other| panic("Invalid unstaged expr: {}", .{other}),
                };
                c.value_stack.append(value) catch oom();
            },
            .unstage_end => {},
            .return_end, .arg, .closure => {
                return fail(c, .cannot_stage_expr);
            },
            .stage_end => {},
            else => {
                try evalExpr(c, expr_data);
            },
        }

        switch (treePart(expr_data)) {
            .branch_begin => ends_remaining += 1,
            .branch_end => ends_remaining -= 1,
            .leaf => {},
        }
        if (ends_remaining == 0) {
            assert(c.value_stack.items.len == 1);
            return c.value_stack.pop();
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
            const expr_data = f.expr_data.get(frame.expr);
            switch (expr_data) {
                .call_end => |call_end| {
                    const args = c.allocator.alloc(Value, call_end.arg_count) catch oom();
                    for (0..call_end.arg_count) |i| {
                        const ix = call_end.arg_count - 1 - i;
                        args[ix] = c.value_stack.pop();
                    }
                    const fun = c.value_stack.pop();
                    if (fun != .fun)
                        return fail(c, .{ .not_a_fun = fun });
                    pushFun(c, .{
                        .fun = fun.fun.repr.fun,
                        .closure = .{ .@"struct" = fun.fun.getClosure() },
                        .args = args,
                    });
                    continue :fun;
                },
                .return_end => {
                    _ = popFun(c);
                    if (c.dir_frame_stack.items.len < start_frame_index)
                        return c.value_stack.pop();
                    c.dir_frame_stack.items[c.dir_frame_stack.items.len - 1].expr.id += 1;
                    continue :fun;
                },
                .stage_end, .unstage_end => {},
                else => {
                    try evalExpr(c, expr_data);
                },
            }
            frame.expr.id += 1;
        }
    }
}

pub fn evalExpr(
    c: *Compiler,
    expr_data: dir.ExprData,
) error{EvalError}!void {
    switch (expr_data) {
        .i32 => |i| {
            c.value_stack.append(.{ .i32 = i }) catch oom();
        },
        .string => |string| {
            c.value_stack.append(.{ .string = string }) catch oom();
        },
        .struct_init_end => |count| {
            const keys = c.allocator.alloc(Value, count) catch oom();
            const reprs = c.allocator.alloc(Repr, count) catch oom();
            const values = c.allocator.alloc(Value, count) catch oom();
            for (0..count) |i| {
                const ix = count - 1 - i;
                values[ix] = c.value_stack.pop();
                keys[ix] = c.value_stack.pop();
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
        .fun_init_end => |fun_init| {
            const closure = c.value_stack.pop();
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
        .local_let_end => |local| {
            const value = c.value_stack.pop();
            c.local_stack.items[c.local_stack.items.len - 1 - local.id] = value;
        },
        .assert_object_end => |assert_object| {
            const value = c.value_stack.pop();
            switch (value) {
                .@"struct" => |@"struct"| {
                    if (@"struct".values.len != assert_object.count)
                        return fail(c, .{ .wrong_number_of_keys = .{
                            .expected = assert_object.count,
                            .actual = @"struct".values.len,
                        } });
                },
                .@"union" => return fail(c, .todo),
                .i32, .string, .repr, .fun, .only, .ref => return fail(c, .{ .expected_object = value }),
            }
            c.value_stack.append(value) catch oom();
        },
        .assert_is_ref_end => {
            const value = c.value_stack.pop();
            if (value != .ref)
                return fail(c, .{ .expected_is_ref = value });
            c.value_stack.append(value) catch oom();
        },
        .assert_has_no_ref_end => {
            const value = c.value_stack.pop();
            if (value.reprOf().hasRef(.any))
                return fail(c, .{ .expected_has_no_ref = value });
            c.value_stack.append(value) catch oom();
        },
        .assert_has_no_ref_visible_end => {
            const value = c.value_stack.pop();
            if (value.reprOf().hasRef(.visible))
                return fail(c, .{ .expected_has_no_ref = value });
            c.value_stack.append(value) catch oom();
        },
        .object_get_end => {
            const key = c.value_stack.pop();
            const object = c.value_stack.pop();
            const value = object.get(key) orelse
                return fail(c, .{ .key_not_found = .{ .object = object, .key = key } });
            c.value_stack.append(value) catch oom();
        },
        .ref_init_end => {
            const value = c.value_stack.pop();
            c.value_stack.append(.{ .ref = .{
                .repr = c.box(value.reprOf()),
                .value = c.box(value.copy(c.allocator)),
            } }) catch oom();
        },
        .ref_set_end => {
            const value = c.value_stack.pop();
            const ref = c.value_stack.pop();
            const input_repr = value.reprOf();
            if (!ref.ref.repr.equal(input_repr))
                return fail(c, .{ .type_error = .{
                    .expected = ref.ref.repr.*,
                    .found = input_repr,
                } });
            ref.ref.value.* = value.copy(c.allocator);
        },
        .ref_get_end => {
            const key = c.value_stack.pop();
            const ref = c.value_stack.pop();
            const value_ptr = ref.ref.value.getMut(key) orelse
                return fail(c, .{ .key_not_found = .{ .object = ref.ref.value.*, .key = key } });
            c.value_stack.append(.{ .ref = .{
                .repr = c.box(value_ptr.reprOf()),
                .value = value_ptr,
            } }) catch oom();
        },
        .ref_deref_end => {
            const ref = c.value_stack.pop();
            c.value_stack.append(ref.ref.value.copy(c.allocator)) catch oom();
        },
        .call_builtin_end => |builtin| {
            switch (builtin) {
                .equal => {
                    const arg1 = c.value_stack.pop();
                    const arg0 = c.value_stack.pop();
                    if (arg0 != .i32 or arg1 != .i32)
                        return fail(c, .{ .invalid_call_builtin = .{ .builtin = builtin, .args = c.dupe(Value, &.{ arg0, arg1 }) } });
                    c.value_stack.append(.{ .i32 = if (arg0.i32 == arg1.i32) 1 else 0 }) catch oom();
                },
                .less_than => {
                    const arg1 = c.value_stack.pop();
                    const arg0 = c.value_stack.pop();
                    if (arg0 != .i32 or arg1 != .i32)
                        return fail(c, .{ .invalid_call_builtin = .{ .builtin = builtin, .args = c.dupe(Value, &.{ arg0, arg1 }) } });
                    c.value_stack.append(.{ .i32 = if (arg0.i32 < arg1.i32) 1 else 0 }) catch oom();
                },
                .less_than_or_equal => {
                    const arg1 = c.value_stack.pop();
                    const arg0 = c.value_stack.pop();
                    if (arg0 != .i32 or arg1 != .i32)
                        return fail(c, .{ .invalid_call_builtin = .{ .builtin = builtin, .args = c.dupe(Value, &.{ arg0, arg1 }) } });
                    c.value_stack.append(.{ .i32 = if (arg0.i32 <= arg1.i32) 1 else 0 }) catch oom();
                },
                .more_than => {
                    const arg1 = c.value_stack.pop();
                    const arg0 = c.value_stack.pop();
                    if (arg0 != .i32 or arg1 != .i32)
                        return fail(c, .{ .invalid_call_builtin = .{ .builtin = builtin, .args = c.dupe(Value, &.{ arg0, arg1 }) } });
                    c.value_stack.append(.{ .i32 = if (arg0.i32 > arg1.i32) 1 else 0 }) catch oom();
                },
                .more_than_or_equal => {
                    const arg1 = c.value_stack.pop();
                    const arg0 = c.value_stack.pop();
                    if (arg0 != .i32 or arg1 != .i32)
                        return fail(c, .{ .invalid_call_builtin = .{ .builtin = builtin, .args = c.dupe(Value, &.{ arg0, arg1 }) } });
                    c.value_stack.append(.{ .i32 = if (arg0.i32 >= arg1.i32) 1 else 0 }) catch oom();
                },
                .add => {
                    const arg1 = c.value_stack.pop();
                    const arg0 = c.value_stack.pop();
                    if (arg0 != .i32 or arg1 != .i32)
                        return fail(c, .{ .invalid_call_builtin = .{ .builtin = builtin, .args = c.dupe(Value, &.{ arg0, arg1 }) } });
                    c.value_stack.append(.{ .i32 = arg0.i32 + arg1.i32 }) catch oom();
                },
                .subtract => {
                    const arg1 = c.value_stack.pop();
                    const arg0 = c.value_stack.pop();
                    if (arg0 != .i32 or arg1 != .i32)
                        return fail(c, .{ .invalid_call_builtin = .{ .builtin = builtin, .args = c.dupe(Value, &.{ arg0, arg1 }) } });
                    c.value_stack.append(.{ .i32 = arg0.i32 - arg1.i32 }) catch oom();
                },
                .multiply => {
                    const arg1 = c.value_stack.pop();
                    const arg0 = c.value_stack.pop();
                    if (arg0 != .i32 or arg1 != .i32)
                        return fail(c, .{ .invalid_call_builtin = .{ .builtin = builtin, .args = c.dupe(Value, &.{ arg0, arg1 }) } });
                    c.value_stack.append(.{ .i32 = arg0.i32 * arg1.i32 }) catch oom();
                },
                else => return fail(c, .todo),
            }
        },
        .block_begin => {
            c.block_value_count_stack.append(c.value_stack.items.len) catch oom();
        },
        .block_last => {
            const value_count_at_begin = c.block_value_count_stack.items[c.block_value_count_stack.items.len - 1];
            for (value_count_at_begin..c.value_stack.items.len) |_| {
                _ = c.value_stack.pop();
            }
        },
        .block_end => {
            const value_count_at_begin = c.block_value_count_stack.pop();
            switch (c.value_stack.items.len - value_count_at_begin) {
                0 => c.value_stack.append(Value.emptyStruct()) catch oom(),
                1 => {},
                else => panic("More than one value returned from block", .{}),
            }
        },
        .if_begin => {},
        .if_then => {
            const cond = c.value_stack.pop();
            if (cond != .i32)
                return fail(c, .{ .not_a_bool = cond });
            if (cond.i32 == 0)
                skipExpr(c, .if_else);
        },
        .if_else => {
            skipExpr(c, .if_end);
        },
        .if_end => {},
        .while_begin => {
            const frame = c.dir_frame_stack.items[c.dir_frame_stack.items.len - 1];
            c.while_stack.append(frame.expr) catch oom();
        },
        .while_body => {
            const cond = c.value_stack.pop();
            if (cond != .i32)
                return fail(c, .{ .not_a_bool = cond });
            if (cond.i32 == 0) {
                _ = c.while_stack.pop();
                c.value_stack.append(Value.emptyStruct()) catch oom();
                skipExpr(c, .while_end);
            }
        },
        .while_end => {
            _ = c.value_stack.pop();
            const frame = &c.dir_frame_stack.items[c.dir_frame_stack.items.len - 1];
            frame.expr = c.while_stack.items[c.while_stack.items.len - 1];
        },
        .call_end, .return_end => panic("Can't eval control flow expr: {}", .{expr_data}),
        .nop_end => {},
        else => {
            if (treePart(expr_data) != .branch_begin)
                return fail(c, .todo);
        },
    }
}

fn skipExpr(c: *Compiler, expect_next: std.meta.Tag(dir.ExprData)) void {
    const frame = &c.dir_frame_stack.items[c.dir_frame_stack.items.len - 1];
    const f = c.dir_fun_data.get(frame.fun);
    var ends_remaining: usize = 0;
    while (true) {
        frame.expr.id += 1;
        const expr_data = f.expr_data.get(frame.expr);
        switch (treePart(expr_data)) {
            .branch_begin => ends_remaining += 1,
            .branch_end => ends_remaining -= 1,
            .leaf => {},
        }
        if (ends_remaining == 0) {
            frame.expr.id += 1;
            assert(std.meta.activeTag(f.expr_data.get(frame.expr)) == expect_next);
            break;
        }
    }
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
    todo,
};
