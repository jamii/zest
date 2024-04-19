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
const DirExpr = zest.DirExpr;
const DirExprData = zest.DirExprData;
const DirExprInput = zest.DirExprInput;
const DirExprOutput = zest.DirExprOutput;
const DirFun = zest.DirFun;
const DirFunData = zest.DirFunData;
const Value = zest.Value;

pub fn eval(c: *Compiler) error{EvalError}!Value {
    var value_stack = ArrayList(Value).init(c.allocator);
    return evalFun(c, c.dir_fun_main.?, &value_stack, Value.emptyStruct());
}

fn evalFun(
    c: *Compiler,
    fun: DirFun,
    value_stack: *ArrayList(Value),
    arg: Value,
) error{EvalError}!Value {
    const f = c.dir_fun_data.get(fun);

    var expr = DirExpr{ .id = 0 };
    while (true) {
        const expr_data = f.expr_data.get(expr);
        switch (expr_data) {
            .@"return" => {
                assert(value_stack.items.len == 1);
                return value_stack.pop();
            },
            else => {},
        }
        try evalExpr(c, f, arg, value_stack, expr);
        expr.id += 1;
    }
}

fn evalExpr(
    c: *Compiler,
    f: DirFunData,
    arg: Value,
    value_stack: *ArrayList(Value),
    expr: DirExpr,
) error{EvalError}!void {
    const expr_data = f.expr_data.get(expr);
    switch (expr_data) {
        inline else => |data, expr_tag| {
            const input = popExprInput(arg, value_stack, expr_tag);
            const output = try evalExprWithInput(c, expr, expr_tag, data, input);
            pushExprOutput(value_stack, expr_tag, output);
        },
    }
}

fn popExprInput(
    arg: Value,
    value_stack: *ArrayList(Value),
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
        @field(input, field.name) = value_stack.pop();
    }
    return input;
}

fn evalExprWithInput(
    c: *Compiler,
    expr: DirExpr,
    comptime expr_tag: std.meta.Tag(DirExprData),
    data: std.meta.TagPayload(DirExprData, expr_tag),
    input: std.meta.TagPayload(DirExprInput, expr_tag),
) error{EvalError}!std.meta.TagPayload(DirExprOutput, expr_tag) {
    switch (expr_tag) {
        .i32 => return .{ .value = .{ .i32 = data } },
        .object_get => {
            const value = input.object.get(input.key) orelse
                return fail(c, expr, .{ .get_missing = .{ .object = input.object, .key = input.key } });
            return .{ .value = value };
        },
        .drop => return,
        .@"return" => panic("Can't eval control flow expr: {}", .{expr_tag}),
        else => return fail(c, expr, .todo),
    }
}

fn pushExprOutput(
    value_stack: *ArrayList(Value),
    comptime expr_tag: std.meta.Tag(DirExprData),
    output: std.meta.TagPayload(DirExprOutput, expr_tag),
) void {
    const Output = std.meta.TagPayload(DirExprOutput, expr_tag);
    if (Output == void) return;
    inline for (@typeInfo(Output).Struct.fields) |field| {
        value_stack.append(@field(output, field.name)) catch oom();
    }
}

fn fail(c: *Compiler, expr: DirExpr, data: EvalErrorData) error{EvalError} {
    c.error_data = .{ .eval = .{ .expr = expr, .data = data } };
    return error.EvalError;
}

pub const EvalErrorData = union(enum) {
    get_missing: struct {
        object: Value,
        key: Value,
    },
    todo,
};
