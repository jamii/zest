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
            .call => |call| {
                const input = popExprInput(c, .call, call);
                if (input.fun != .fun)
                    return fail(c, .{ .not_a_fun = input.fun });
                pushFun(c, .{
                    .fun = input.fun.fun.repr.fun,
                    .closure = .{ .@"struct" = input.fun.fun.getClosure() },
                    .arg = input.args,
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
            inline else => |data, expr_tag| {
                const input = popExprInput(c, expr_tag, data);
                const output = try evalExpr(c, expr_tag, data, input);
                pushExprOutput(c, expr_tag, output);
            },
        }

        if (expr_data == .begin) ends_remaining += 1;
        if (expr_data.isEnd()) ends_remaining -= 1;
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
                .call => |call| {
                    const input = popExprInput(c, .call, call);
                    if (input.fun != .fun)
                        return fail(c, .{ .not_a_fun = input.fun });
                    pushFun(c, .{
                        .fun = input.fun.fun.repr.fun,
                        .closure = .{ .@"struct" = input.fun.fun.getClosure() },
                        .arg = input.args,
                    });
                    continue :fun;
                },
                .@"return" => {
                    _ = popFun(c);
                    if (c.dir_frame_stack.items.len < start_frame_index) {
                        return c.value_stack.pop();
                    }
                    c.dir_frame_stack.items[c.dir_frame_stack.items.len - 1].expr.id += 1;
                    continue :fun;
                },
                .begin, .stage, .unstage => {},
                inline else => |data, expr_tag| {
                    const input = popExprInput(c, expr_tag, data);
                    const output = try evalExpr(c, expr_tag, data, input);
                    pushExprOutput(c, expr_tag, output);
                },
            }
            frame.expr.id += 1;
        }
    }
}

fn popExprInput(
    c: *Compiler,
    comptime expr_tag: std.meta.Tag(dir.ExprData),
    data: std.meta.TagPayload(dir.ExprData, expr_tag),
) std.meta.TagPayload(dir.ExprInput(Value), expr_tag) {
    switch (expr_tag) {
        .i32, .f32, .string, .arg, .closure, .local_get, .ref_set_middle, .stage, .unstage, .begin => return,
        .fun_init, .local_let, .assert_object, .object_get, .ref_init, .ref_get, .ref_set, .ref_deref, .drop, .block, .@"return", .call => {
            const Input = std.meta.TagPayload(dir.ExprInput(Value), expr_tag);
            var input: Input = undefined;
            const fields = @typeInfo(Input).Struct.fields;
            comptime var i: usize = fields.len;
            inline while (i > 0) : (i -= 1) {
                const field = fields[i - 1];
                @field(input, field.name) = c.value_stack.pop();
            }
            return input;
        },
        .struct_init => {
            const keys = c.allocator.alloc(Value, data) catch oom();
            const values = c.allocator.alloc(Value, data) catch oom();
            for (0..data) |i| {
                values[data - 1 - i] = c.value_stack.pop();
                keys[data - 1 - i] = c.value_stack.pop();
            }
            return .{ .keys = keys, .values = values };
        },
    }
}

pub fn evalExpr(
    c: *Compiler,
    comptime expr_tag: std.meta.Tag(dir.ExprData),
    data: std.meta.TagPayload(dir.ExprData, expr_tag),
    input: std.meta.TagPayload(dir.ExprInput(Value), expr_tag),
) error{EvalError}!std.meta.TagPayload(dir.ExprOutput(Value), expr_tag) {
    switch (expr_tag) {
        .i32 => return .{ .i32 = data },
        .string => return .{ .string = data },
        .struct_init => {
            const reprs = c.allocator.alloc(Repr, data) catch oom();
            for (input.values, reprs) |value, *repr| {
                repr.* = value.reprOf();
            }
            // TODO sort
            return .{ .@"struct" = .{
                .repr = .{
                    .keys = input.keys,
                    .reprs = reprs,
                },
                .values = input.values,
            } };
        },
        .fun_init => {
            return .{ .fun = .{
                .repr = .{
                    .fun = data.fun,
                    .closure = input.closure.@"struct".repr,
                },
                .closure = input.closure.@"struct".values,
            } };
        },
        .arg => {
            const frame = c.dir_frame_stack.items[c.dir_frame_stack.items.len - 1];
            return frame.arg;
        },
        .closure => {
            const frame = c.dir_frame_stack.items[c.dir_frame_stack.items.len - 1];
            return frame.closure;
        },
        .local_get => {
            const value = c.local_stack.items[c.local_stack.items.len - 1 - data.id];
            return value;
        },
        .local_let => {
            c.local_stack.items[c.local_stack.items.len - 1 - data.id] = input.value;
            return;
        },
        .assert_object => {
            var value = input.value;
            while (value == .ref) {
                value = value.ref.value.*;
            }
            switch (value) {
                .@"struct" => |@"struct"| {
                    if (@"struct".values.len != data.count)
                        return fail(c, .{ .wrong_number_of_keys = .{
                            .expected = data.count,
                            .actual = @"struct".values.len,
                        } });
                },
                .i32, .string, .repr, .fun, .only => return fail(c, .{ .not_an_object = input.value }),
                .@"union" => return fail(c, .todo),
                .ref => unreachable,
            }
            return;
        },
        .object_get => {
            const value = input.object.get(input.key) orelse
                return fail(c, .{ .key_not_found = .{ .object = input.object, .key = input.key } });
            return value;
        },
        .ref_init => {
            const value = Value{ .ref = .{
                .repr = c.box(input.value.reprOf()),
                .value = c.box(input.value.copy(c.allocator)),
            } };
            return value;
        },
        .ref_set_middle => {},
        .ref_set => {
            const input_repr = input.value.reprOf();
            if (!input.ref.ref.repr.equal(input_repr))
                return fail(c, .{ .type_error = .{
                    .expected = input.ref.ref.repr.*,
                    .found = input_repr,
                } });
            input.ref.ref.value.* = input.value.copy(c.allocator);
            return;
        },
        .ref_get => {
            const value_ptr = input.ref.ref.value.getMut(input.key) orelse
                return fail(c, .{ .key_not_found = .{ .object = input.ref.ref.value.*, .key = input.key } });
            const value = Value{ .ref = .{
                .repr = c.box(value_ptr.reprOf()),
                .value = value_ptr,
            } };
            return value;
        },
        .ref_deref => {
            const value = input.ref.ref.value.copy(c.allocator);
            return value;
        },
        .drop => return,
        .block => return input.value,
        .call, .@"return" => panic("Can't eval control flow expr: {}", .{expr_tag}),
        else => return fail(c, .todo),
    }
}

fn pushExprOutput(
    c: *Compiler,
    comptime expr_tag: std.meta.Tag(dir.ExprData),
    output: std.meta.TagPayload(dir.ExprOutput(Value), expr_tag),
) void {
    switch (@TypeOf(output)) {
        void => return,
        Value => c.value_stack.append(output) catch oom(),
        else => @compileError(@typeName(@TypeOf(output))),
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
    not_an_object: Value,
    not_a_fun: Value,
    cannot_stage_expr,
    cannot_unstage_value: Repr,
    todo,
};
