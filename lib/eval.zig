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
const DirExprInput = zest.DirExprInput;
const DirExprOutput = zest.DirExprOutput;
const DirFun = zest.DirFun;
const DirFunData = zest.DirFunData;
const Value = zest.Value;

pub fn eval(c: *Compiler) error{EvalError}!Value {
    assert(c.frame_stack.items.len == 0);
    assert(c.value_stack.items.len == 0);
    return evalFun(c, c.dir_fun_main.?, Value.emptyStruct());
}

fn evalFun(
    c: *Compiler,
    fun: DirFun,
    arg: Value,
) error{EvalError}!Value {
    assert(c.frame_stack.items.len == 0);
    assert(c.value_stack.items.len == 0);

    var local_value = List(DirLocal, Value).init(c.allocator);
    local_value.appendNTimes(
        Value.emptyStruct(),
        c.dir_fun_data.get(fun).local_data.count(),
    );
    c.frame_stack.append(.{
        .fun = fun,
        .arg = arg,
        .expr = .{ .id = 0 },
        .local_value = local_value,
    }) catch oom();

    fun: while (true) {
        if (c.frame_stack.items.len == 0) {
            assert(c.value_stack.items.len == 1);
            return c.value_stack.pop();
        }
        const frame = &c.frame_stack.items[c.frame_stack.items.len - 1];
        const f = c.dir_fun_data.get(frame.fun);
        while (true) {
            const expr_data = f.expr_data.get(frame.expr);
            switch (expr_data) {
                .@"return" => {
                    _ = c.frame_stack.pop();
                    continue :fun;
                },
                inline else => |data, expr_tag| {
                    const input = popExprInput(c, arg, expr_tag);
                    const output = try evalExprWithInput(c, expr_tag, data, input);
                    pushExprOutput(c, expr_tag, output);
                },
            }
            frame.expr.id += 1;
        }
    }
}

fn popExprInput(
    c: *Compiler,
    arg: Value,
    comptime expr_tag: std.meta.Tag(DirExprData),
) std.meta.TagPayload(DirExprInput, expr_tag) {
    _ = arg;
    const Input = std.meta.TagPayload(DirExprInput, expr_tag);
    if (Input == void) return;
    var input: Input = undefined;
    const fields = @typeInfo(Input).Struct.fields;
    comptime var i: usize = fields.len;
    inline while (i > 0) : (i -= 1) {
        const field = fields[i - 1];
        @field(input, field.name) = c.value_stack.pop();
    }
    return input;
}

fn evalExprWithInput(
    c: *Compiler,
    comptime expr_tag: std.meta.Tag(DirExprData),
    data: std.meta.TagPayload(DirExprData, expr_tag),
    input: std.meta.TagPayload(DirExprInput, expr_tag),
) error{EvalError}!std.meta.TagPayload(DirExprOutput, expr_tag) {
    switch (expr_tag) {
        .i32 => return .{ .value = .{ .i32 = data } },
        .local_get => {
            const frame = c.frame_stack.items[c.frame_stack.items.len - 1];
            return .{ .value = frame.local_value.get(data) };
        },
        .local_set => {
            const frame = c.frame_stack.items[c.frame_stack.items.len - 1];
            frame.local_value.getPtr(data).* = input.value;
            return;
        },
        .object_get => {
            const value = input.object.get(input.key) orelse
                return fail(c, .{ .get_missing = .{ .object = input.object, .key = input.key } });
            return .{ .value = value };
        },
        .drop => return,
        .@"return" => panic("Can't eval control flow expr: {}", .{expr_tag}),
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
    const frame = c.frame_stack.items[c.frame_stack.items.len - 1];
    c.error_data = .{ .eval = .{ .fun = frame.fun, .expr = frame.expr, .data = data } };
    return error.EvalError;
}

pub const EvalErrorData = union(enum) {
    get_missing: struct {
        object: Value,
        key: Value,
    },
    todo,
};
