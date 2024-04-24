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
const DirLocal = zest.DirLocal;
const DirExpr = zest.DirExpr;
const DirExprData = zest.DirExprData;
const DirExprInput = zest.DirExprInput(Value);
const DirExprOutput = zest.DirExprOutput(Value);
const DirFun = zest.DirFun;
const DirFunData = zest.DirFunData;
const Value = zest.Value;
const Repr = zest.Repr;

pub fn evalMain(c: *Compiler) error{EvalError}!Value {
    assert(c.dir_frame_stack.items.len == 0);
    assert(c.value_stack.items.len == 0);
    pushFun(c, c.dir_fun_main.?, Value.emptyStruct(), Value.emptyStruct());
    return eval(c);
}

fn pushFun(c: *Compiler, fun: DirFun, arg: Value, closure: Value) void {
    c.dir_frame_stack.append(.{
        .fun = fun,
        .arg = arg,
        .closure = closure,
        .expr = .{ .id = 0 },
    }) catch oom();
    c.local_stack.appendNTimes(
        Value.emptyStruct(),
        c.dir_fun_data.get(fun).local_data.count(),
    ) catch oom();
}

fn eval(c: *Compiler) error{EvalError}!Value {
    //std.debug.print("---\n\n", .{});
    fun: while (true) {
        const frame = &c.dir_frame_stack.items[c.dir_frame_stack.items.len - 1];
        const f = c.dir_fun_data.get(frame.fun);
        while (true) {
            const expr_data = f.expr_data.get(frame.expr);
            switch (expr_data) {
                .@"return" => {
                    c.local_stack.shrinkRetainingCapacity(
                        c.local_stack.items.len -
                            c.dir_fun_data.get(frame.fun).local_data.count(),
                    );
                    _ = c.dir_frame_stack.pop();
                    if (c.dir_frame_stack.items.len == 0) {
                        assert(c.value_stack.items.len == 1);
                        return c.value_stack.pop();
                    }
                    c.dir_frame_stack.items[c.dir_frame_stack.items.len - 1].expr.id += 1;
                    continue :fun;
                },
                .call => |call| {
                    const input = popExprInput(c, .call, call);
                    if (input.fun != .fun)
                        return fail(c, .{ .not_a_fun = input.fun });
                    const fun = input.fun.fun;
                    pushFun(c, fun.repr.fun, input.args, .{ .@"struct" = fun.getClosure() });
                    continue :fun;
                },
                inline else => |data, expr_tag| {
                    const input = popExprInput(c, expr_tag, data);
                    const output = try evalExpr(c, expr_tag, data, input);
                    pushExprOutput(c, expr_tag, output);
                    //std.debug.print("{}\n{}\n{}\n\n", .{ expr_data, input, output });
                },
            }
            frame.expr.id += 1;
        }
    }
}

fn popExprInput(
    c: *Compiler,
    comptime expr_tag: std.meta.Tag(DirExprData),
    data: std.meta.TagPayload(DirExprData, expr_tag),
) std.meta.TagPayload(DirExprInput, expr_tag) {
    switch (expr_tag) {
        .i32, .f32, .string, .arg, .closure, .local_get => return,
        .fun_init, .local_set, .object_get, .drop, .@"return", .call => {
            const Input = std.meta.TagPayload(DirExprInput, expr_tag);
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

fn evalExpr(
    c: *Compiler,
    comptime expr_tag: std.meta.Tag(DirExprData),
    data: std.meta.TagPayload(DirExprData, expr_tag),
    input: std.meta.TagPayload(DirExprInput, expr_tag),
) error{EvalError}!std.meta.TagPayload(DirExprOutput, expr_tag) {
    switch (expr_tag) {
        .i32 => return .{ .value = .{ .i32 = data } },
        .string => return .{ .value = .{ .string = data } },
        .struct_init => {
            const reprs = c.allocator.alloc(Repr, data) catch oom();
            for (input.values, reprs) |value, *repr| {
                repr.* = value.reprOf();
            }
            // TODO sort
            return .{ .value = .{ .@"struct" = .{
                .repr = .{
                    .keys = input.keys,
                    .reprs = reprs,
                },
                .values = input.values,
            } } };
        },
        .fun_init => {
            return .{ .value = .{ .fun = .{
                .repr = .{
                    .fun = data.fun,
                    .closure = input.closure.@"struct".repr,
                },
                .closure = input.closure.@"struct".values,
            } } };
        },
        .arg => {
            const frame = c.dir_frame_stack.items[c.dir_frame_stack.items.len - 1];
            return .{ .value = frame.arg };
        },
        .closure => {
            const frame = c.dir_frame_stack.items[c.dir_frame_stack.items.len - 1];
            return .{ .value = frame.closure };
        },
        .local_get => {
            const value = c.local_stack.items[c.local_stack.items.len - 1 - data.id];
            return .{ .value = value };
        },
        .local_set => {
            c.local_stack.items[c.local_stack.items.len - 1 - data.id] = input.value;
            return;
        },
        .object_get => {
            const value = input.object.get(input.key) orelse
                return fail(c, .{ .get_missing = .{ .object = input.object, .key = input.key } });
            return .{ .value = value };
        },
        .drop => return,
        .call, .@"return" => panic("Can't eval control flow expr: {}", .{expr_tag}),
        else => return fail(c, .todo),
    }
}

fn pushExprOutput(
    c: *Compiler,
    comptime expr_tag: std.meta.Tag(DirExprData),
    output: std.meta.TagPayload(DirExprOutput, expr_tag),
) void {
    const Output = std.meta.TagPayload(DirExprOutput, expr_tag);
    if (Output == void) return;
    inline for (@typeInfo(Output).Struct.fields) |field| {
        c.value_stack.append(@field(output, field.name)) catch oom();
    }
}

fn fail(c: *Compiler, data: EvalErrorData) error{EvalError} {
    const frame = c.dir_frame_stack.items[c.dir_frame_stack.items.len - 1];
    c.error_data = .{ .eval = .{ .fun = frame.fun, .expr = frame.expr, .data = data } };
    return error.EvalError;
}

pub const EvalErrorData = union(enum) {
    get_missing: struct {
        object: Value,
        key: Value,
    },
    not_a_fun: Value,
    todo,
};
