const std = @import("std");
const panic = std.debug.panic;
const assert = std.debug.assert;
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;

const zest = @import("./zest.zig");
const oom = zest.oom;
const deepEqual = zest.deepEqual;
const List = zest.List;
const Compiler = zest.Compiler;
const Value = zest.Value;
const Repr = zest.Repr;
const Builtin = zest.Builtin;
const dir = zest.dir;
const tir = zest.tir;

pub fn evalMain(c: *Compiler) error{EvalError}!Value {
    assert(c.dir_frame_stack.items.len == 0);
    assert(c.value_stack.items.len == 0);
    pushFun(c, .{
        .fun = c.dir_fun_main.?,
        .closure = Value.emptyStruct(),
        .arg = Value.emptyStruct(),
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

pub fn evalStaged(c: *Compiler, tir_f: *tir.FunData, arg_repr: Repr, closure_repr: Repr) error{EvalError}!Value {
    const frame = &c.dir_frame_stack.items[c.dir_frame_stack.items.len - 1];
    const f = c.dir_fun_data.get(frame.fun);

    var ends_remaining: usize = 0;

    while (true) {
        const expr_data = f.expr_data.get(frame.expr);
        switch (expr_data) {
            .call => {
                const args = c.value_stack.pop();
                const fun = c.value_stack.pop();
                if (fun != .fun)
                    return fail(c, .{ .not_a_fun = fun });
                pushFun(c, .{
                    .fun = fun.fun.repr.fun,
                    .closure = .{ .@"struct" = fun.fun.getClosure() },
                    .arg = args,
                });
                const return_value = try eval(c);
                c.value_stack.append(return_value) catch oom();
            },
            // TODO This is a simple version.
            //      To make things like `stage(repr-of(a.b.c))` work we'd need to switch back to infer.
            //      Also have to limit to exprs with no side-effects or panics.
            .unstage => {
                frame.expr.id += 1;
                const value = switch (f.expr_data.get(frame.expr)) {
                    .local_get => |local| value: {
                        const local_repr = tir_f.local_data.get(.{ .id = local.id }).repr.one;
                        break :value local_repr.valueOf() orelse
                            return fail(c, .{ .cannot_unstage_value = local_repr });
                    },
                    .arg => arg_repr.valueOf() orelse
                        return fail(c, .{ .cannot_unstage_value = arg_repr }),
                    .closure => closure_repr.valueOf() orelse
                        return fail(c, .{ .cannot_unstage_value = closure_repr }),
                    else => |other| panic("Invalid unstaged expr: {}", .{other}),
                };
                c.value_stack.append(value) catch oom();
            },
            .@"return", .arg, .closure => {
                return fail(c, .cannot_stage_expr);
            },
            .begin, .stage => {},
            else => {
                try evalExpr(c, expr_data);
            },
        }

        if (expr_data == .begin) ends_remaining += 1;
        if (expr_data.treePart() == .branch_end) ends_remaining -= 1;
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
                .call => {
                    const args = c.value_stack.pop();
                    const fun = c.value_stack.pop();
                    if (fun != .fun)
                        return fail(c, .{ .not_a_fun = fun });
                    pushFun(c, .{
                        .fun = fun.fun.repr.fun,
                        .closure = .{ .@"struct" = fun.fun.getClosure() },
                        .arg = args,
                    });
                    continue :fun;
                },
                .@"return" => {
                    _ = popFun(c);
                    if (c.dir_frame_stack.items.len < start_frame_index)
                        return c.value_stack.pop();
                    c.dir_frame_stack.items[c.dir_frame_stack.items.len - 1].expr.id += 1;
                    continue :fun;
                },
                .begin, .stage, .unstage => {},
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
        .struct_init => |count| {
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
        .fun_init => |fun_init| {
            const closure = c.value_stack.pop();
            c.value_stack.append(.{ .fun = .{
                .repr = .{
                    .fun = fun_init.fun,
                    .closure = closure.@"struct".repr,
                },
                .closure = closure.@"struct".values,
            } }) catch oom();
        },
        .arg => {
            const frame = c.dir_frame_stack.items[c.dir_frame_stack.items.len - 1];
            c.value_stack.append(frame.arg) catch oom();
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
            const value = c.value_stack.pop();
            c.local_stack.items[c.local_stack.items.len - 1 - local.id] = value;
        },
        .assert_object => |assert_object| {
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
        .assert_is_ref => {
            const value = c.value_stack.pop();
            if (value != .ref)
                return fail(c, .{ .expected_is_ref = value });
            c.value_stack.append(value) catch oom();
        },
        .assert_has_no_ref => {
            const value = c.value_stack.pop();
            if (value.reprOf().hasRef())
                return fail(c, .{ .expected_has_no_ref = value });
            c.value_stack.append(value) catch oom();
        },
        .object_get => {
            const key = c.value_stack.pop();
            const object = c.value_stack.pop();
            const value = object.get(key) orelse
                return fail(c, .{ .key_not_found = .{ .object = object, .key = key } });
            c.value_stack.append(value) catch oom();
        },
        .ref_init => {
            const value = c.value_stack.pop();
            c.value_stack.append(.{ .ref = .{
                .repr = c.box(value.reprOf()),
                .value = c.box(value.copy(c.allocator)),
            } }) catch oom();
        },
        .ref_set => {
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
        .ref_get => {
            const key = c.value_stack.pop();
            const ref = c.value_stack.pop();
            const value_ptr = ref.ref.value.getMut(key) orelse
                return fail(c, .{ .key_not_found = .{ .object = ref.ref.value.*, .key = key } });
            c.value_stack.append(.{ .ref = .{
                .repr = c.box(value_ptr.reprOf()),
                .value = value_ptr,
            } }) catch oom();
        },
        .ref_deref => {
            const ref = c.value_stack.pop();
            c.value_stack.append(ref.ref.value.copy(c.allocator)) catch oom();
        },
        .call_builtin => |builtin| {
            const args = c.value_stack.pop();
            switch (builtin) {
                .equal => {
                    const arg0 = args.@"struct".values[0];
                    const arg1 = args.@"struct".values[1];
                    if (arg0 != .i32 or arg1 != .i32)
                        return fail(c, .{ .invalid_call_builtin = .{ .builtin = builtin, .args = args } });
                    c.value_stack.append(.{ .i32 = if (arg0.i32 == arg1.i32) 1 else 0 }) catch oom();
                },
                .less_than => {
                    const arg0 = args.@"struct".values[0];
                    const arg1 = args.@"struct".values[1];
                    if (arg0 != .i32 or arg1 != .i32)
                        return fail(c, .{ .invalid_call_builtin = .{ .builtin = builtin, .args = args } });
                    c.value_stack.append(.{ .i32 = if (arg0.i32 < arg1.i32) 1 else 0 }) catch oom();
                },
                .less_than_or_equal => {
                    const arg0 = args.@"struct".values[0];
                    const arg1 = args.@"struct".values[1];
                    if (arg0 != .i32 or arg1 != .i32)
                        return fail(c, .{ .invalid_call_builtin = .{ .builtin = builtin, .args = args } });
                    c.value_stack.append(.{ .i32 = if (arg0.i32 <= arg1.i32) 1 else 0 }) catch oom();
                },
                .more_than => {
                    const arg0 = args.@"struct".values[0];
                    const arg1 = args.@"struct".values[1];
                    if (arg0 != .i32 or arg1 != .i32)
                        return fail(c, .{ .invalid_call_builtin = .{ .builtin = builtin, .args = args } });
                    c.value_stack.append(.{ .i32 = if (arg0.i32 > arg1.i32) 1 else 0 }) catch oom();
                },
                .more_than_or_equal => {
                    const arg0 = args.@"struct".values[0];
                    const arg1 = args.@"struct".values[1];
                    if (arg0 != .i32 or arg1 != .i32)
                        return fail(c, .{ .invalid_call_builtin = .{ .builtin = builtin, .args = args } });
                    c.value_stack.append(.{ .i32 = if (arg0.i32 >= arg1.i32) 1 else 0 }) catch oom();
                },
                .add => {
                    const arg0 = args.@"struct".values[0];
                    const arg1 = args.@"struct".values[1];
                    if (arg0 != .i32 or arg1 != .i32)
                        return fail(c, .{ .invalid_call_builtin = .{ .builtin = builtin, .args = args } });
                    c.value_stack.append(.{ .i32 = arg0.i32 + arg1.i32 }) catch oom();
                },
                .subtract => {
                    const arg0 = args.@"struct".values[0];
                    const arg1 = args.@"struct".values[1];
                    if (arg0 != .i32 or arg1 != .i32)
                        return fail(c, .{ .invalid_call_builtin = .{ .builtin = builtin, .args = args } });
                    c.value_stack.append(.{ .i32 = arg0.i32 - arg1.i32 }) catch oom();
                },
                .multiply => {
                    const arg0 = args.@"struct".values[0];
                    const arg1 = args.@"struct".values[1];
                    if (arg0 != .i32 or arg1 != .i32)
                        return fail(c, .{ .invalid_call_builtin = .{ .builtin = builtin, .args = args } });
                    c.value_stack.append(.{ .i32 = arg0.i32 * arg1.i32 }) catch oom();
                },
                else => return fail(c, .todo),
            }
        },
        .block => |count| {
            if (count == 0) {
                c.value_stack.append(Value.emptyStruct()) catch oom();
            } else {
                const value = c.value_stack.pop();
                for (0..count - 1) |_|
                    _ = c.value_stack.pop();
                c.value_stack.append(value) catch oom();
            }
        },
        .if_begin => {},
        .if_then => {
            const cond = c.value_stack.pop();
            c.value_stack.append(cond) catch oom();
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
            c.value_stack.append(cond) catch oom();
            if (cond != .i32)
                return fail(c, .{ .not_a_bool = cond });
            if (cond.i32 == 0) {
                _ = c.while_stack.pop();
                skipExpr(c, .while_end);
                const frame = &c.dir_frame_stack.items[c.dir_frame_stack.items.len - 1];
                frame.expr.id += 1;
            }
        },
        .while_end => {
            _ = c.value_stack.pop();
            const frame = &c.dir_frame_stack.items[c.dir_frame_stack.items.len - 1];
            frame.expr = c.while_stack.items[c.while_stack.items.len - 1];
        },
        .call, .@"return" => panic("Can't eval control flow expr: {}", .{expr_data}),
        .nop => {},
        else => return fail(c, .todo),
    }
}

fn skipExpr(c: *Compiler, expect_next: std.meta.Tag(dir.ExprData)) void {
    const frame = &c.dir_frame_stack.items[c.dir_frame_stack.items.len - 1];
    const f = c.dir_fun_data.get(frame.fun);
    var depth: usize = 0;
    while (true) {
        frame.expr.id += 1;
        const expr_data = f.expr_data.get(frame.expr);
        switch (expr_data.treePart()) {
            .branch_begin => depth += 1,
            .branch_end => depth -= 1,
            .leaf, .branch_separator, .branch_begin_without_end => {},
        }
        if (depth == 0) {
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
        args: Value,
    },
    todo,
};
