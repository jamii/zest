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
const TirLocal = zest.TirLocal;
const DirExpr = zest.DirExpr;
const DirExprData = zest.DirExprData;
const DirExprInput = zest.DirExprInput(Repr);
const DirExprOutput = zest.DirExprOutput(Repr);
const TirExprData = zest.TirExprData;
const TirFun = zest.TirFun;
const TirFunData = zest.TirFunData;
const TirFunKey = zest.TirFunKey;
const TirFrame = zest.TirFrame;
const Value = zest.Value;
const Repr = zest.Repr;
const FlatLattice = zest.FlatLattice;

const eval = @import("./eval.zig");

pub fn inferMain(c: *Compiler) error{ EvalError, InferError }!void {
    assert(c.tir_frame_stack.items.len == 0);
    c.tir_fun_main = pushFun(
        c,
        .{
            .fun = c.dir_fun_main.?,
            .closure_repr = Repr.emptyStruct(),
            .arg_repr = Repr.emptyStruct(),
        },
    );
    try infer(c);
}

fn pushFun(c: *Compiler, key: TirFunKey) TirFun {
    const fun = c.tir_fun_data.append(TirFunData.init(c.allocator));
    const f = c.tir_fun_data.getPtr(fun);
    f.local_data.appendNTimes(.{ .repr = .zero }, c.dir_fun_data.get(key.fun).local_data.count());
    c.tir_frame_stack.append(.{ .key = key, .fun = fun, .expr = .{ .id = 0 } }) catch oom();
    return fun;
}

fn infer(c: *Compiler) error{ EvalError, InferError }!void {
    while (true) {
        const direction = try inferFrame(
            c,
            &c.tir_frame_stack.items[c.tir_frame_stack.items.len - 1],
        );
        switch (direction) {
            .call => continue,
            .@"return" => {},
        }
        const child_frame = c.tir_frame_stack.pop();
        const return_repr_lattice = &c.tir_fun_data.getPtr(child_frame.fun).return_repr;
        switch (return_repr_lattice.*) {
            .zero => return_repr_lattice.* = .{ .one = Repr.emptyUnion() },
            .one => {},
            .many => panic("Unreachable - should have errored earlier", .{}),
        }
        c.tir_fun_by_key.put(child_frame.key, child_frame.fun) catch oom();
        if (c.tir_frame_stack.items.len == 0)
            break;
        // Don't need to advance expr.id here - we'll revisit the call and use the cached repr this time.
    }
}

fn inferFrame(c: *Compiler, frame: *TirFrame) error{ EvalError, InferError }!enum { call, @"return" } {
    const dir_f = c.dir_fun_data.get(frame.key.fun);
    const f = c.tir_fun_data.getPtr(frame.fun);
    while (frame.expr.id <= dir_f.expr_data.lastKey().?.id) : (frame.expr.id += 1) {
        const expr_data = dir_f.expr_data.get(frame.expr);
        if (dir_f.expr_is_staged.isSet(frame.expr.id)) {
            switch (expr_data) {
                .call => |call| {
                    const input = try popExprInputValue(c, .call, call);
                    if (input.fun != .fun)
                        return fail(c, .{ .not_a_fun = input.fun.reprOf() });
                    const fun = input.fun.fun;
                    eval.pushFun(c, .{
                        .fun = fun.repr.fun,
                        .arg = input.args,
                        .closure = .{ .@"struct" = fun.getClosure() },
                    });
                    const value = try eval.eval(c);
                    c.repr_stack.append(.{ .only = c.box(value) }) catch oom();
                },
                .@"return" => return fail(c, .staged_return),
                .arg, .closure => return fail(c, .not_compile_time_known),
                .block_begin, .block_end => {},
                inline else => |data, expr_tag| {
                    const input = try popExprInputValue(c, expr_tag, data);
                    // TODO Kinda annoying that we have to push this frame for error handling in evalExpr.
                    c.dir_frame_stack.append(.{
                        .fun = frame.key.fun,
                        .arg = Value.emptyStruct(),
                        .closure = Value.emptyStruct(),
                        .expr = frame.expr,
                    }) catch oom();
                    // TODO this is obviously not correct
                    c.local_stack.appendNTimes(
                        Value.emptyStruct(),
                        dir_f.local_data.count(),
                    ) catch oom();
                    const output = try eval.evalExpr(c, expr_tag, data, input);
                    _ = eval.popFun(c);
                    pushExprOutputValue(c, expr_tag, output);
                },
            }
        } else {
            switch (expr_data) {
                .call => |call| {
                    const input = try popExprInput(c, .call, call);
                    if (input.fun != .fun)
                        return fail(c, .{ .not_a_fun = input.fun });
                    const key = TirFunKey{
                        .fun = input.fun.fun.fun,
                        .closure_repr = .{ .@"struct" = input.fun.fun.closure },
                        .arg_repr = input.args,
                    };
                    if (c.tir_fun_by_key.get(key)) |fun| {
                        // TODO once we have recursive functions, seeing a .zero here indicates that type inference is cyclic
                        const return_repr = c.tir_fun_data.get(fun).return_repr.one;
                        _ = push(c, f, .{ .call = fun }, return_repr);
                        pushExprOutput(c, .call, .{ .value = return_repr });
                    } else {
                        // Put inputs back on stack and switch to the called function.
                        c.repr_stack.append(input.fun) catch oom();
                        c.repr_stack.append(input.args) catch oom();
                        _ = pushFun(c, key);
                        return .call;
                    }
                },
                inline else => |data, expr_tag| {
                    const input = try popExprInput(c, expr_tag, data);
                    const output = try inferExpr(c, f, expr_tag, data, input);
                    pushExprOutput(c, expr_tag, output);
                },
            }
        }
    }
    return .@"return";
}

fn popExprInput(
    c: *Compiler,
    comptime expr_tag: std.meta.Tag(DirExprData),
    data: std.meta.TagPayload(DirExprData, expr_tag),
) error{InferError}!std.meta.TagPayload(DirExprInput, expr_tag) {
    switch (expr_tag) {
        .i32, .f32, .string, .arg, .closure, .local_get, .block_begin, .block_end => return,
        .fun_init, .local_set, .object_get, .drop, .@"return", .call => {
            const Input = std.meta.TagPayload(DirExprInput, expr_tag);
            var input: Input = undefined;
            const fields = @typeInfo(Input).Struct.fields;
            comptime var i: usize = fields.len;
            inline while (i > 0) : (i -= 1) {
                const field = fields[i - 1];
                @field(input, field.name) = switch (field.type) {
                    Repr => c.repr_stack.pop(),
                    Value => try popValue(c),
                    else => @compileError(@typeName(field.type)),
                };
            }
            return input;
        },
        .struct_init => {
            const keys = c.allocator.alloc(Value, data) catch oom();
            const reprs = c.allocator.alloc(Repr, data) catch oom();
            for (0..data) |i| {
                reprs[data - 1 - i] = c.repr_stack.pop();
                keys[data - 1 - i] = try popValue(c);
            }
            return .{ .keys = keys, .values = reprs };
        },
    }
}

fn popExprInputValue(
    c: *Compiler,
    comptime expr_tag: std.meta.Tag(DirExprData),
    data: std.meta.TagPayload(DirExprData, expr_tag),
) error{InferError}!std.meta.TagPayload(zest.DirExprInput(Value), expr_tag) {
    switch (expr_tag) {
        .i32, .f32, .string, .arg, .closure, .local_get, .block_begin, .block_end => return,
        .fun_init, .local_set, .object_get, .drop, .@"return", .call => {
            const Input = std.meta.TagPayload(zest.DirExprInput(Value), expr_tag);
            var input: Input = undefined;
            const fields = @typeInfo(Input).Struct.fields;
            comptime var i: usize = fields.len;
            inline while (i > 0) : (i -= 1) {
                const field = fields[i - 1];
                @field(input, field.name) = try popValue(c);
            }
            return input;
        },
        .struct_init => {
            const keys = c.allocator.alloc(Value, data) catch oom();
            const values = c.allocator.alloc(Value, data) catch oom();
            for (0..data) |i| {
                values[data - 1 - i] = try popValue(c);
                keys[data - 1 - i] = try popValue(c);
            }
            return .{ .keys = keys, .values = values };
        },
    }
}

fn inferExpr(
    c: *Compiler,
    f: *TirFunData,
    comptime expr_tag: std.meta.Tag(DirExprData),
    data: std.meta.TagPayload(DirExprData, expr_tag),
    input: std.meta.TagPayload(DirExprInput, expr_tag),
) error{InferError}!std.meta.TagPayload(DirExprOutput, expr_tag) {
    switch (expr_tag) {
        .i32 => {
            push(c, f, .{ .i32 = data }, .i32);
            return .{ .value = .i32 };
        },
        .string => {
            push(c, f, .{ .string = data }, .string);
            return .{ .value = .string };
        },
        .struct_init => {
            const repr = Repr{ .@"struct" = .{
                .keys = input.keys,
                .reprs = input.values,
            } };
            push(c, f, .struct_init, repr);
            return .{ .value = repr };
        },
        .fun_init => {
            const repr = Repr{ .fun = .{
                .fun = data.fun,
                .closure = input.closure.@"struct",
            } };
            push(c, f, .fun_init, repr);
            return .{ .value = repr };
        },
        .arg => {
            const frame = c.tir_frame_stack.items[c.tir_frame_stack.items.len - 1];
            return .{ .value = frame.key.arg_repr };
        },
        .closure => {
            const frame = c.tir_frame_stack.items[c.tir_frame_stack.items.len - 1];
            return .{ .value = frame.key.closure_repr };
        },
        .local_get => {
            const local = TirLocal{ .id = data.id };
            // Shouldn't be able to reach get before set.
            const repr = f.local_data.get(local).repr.one;
            push(c, f, .{ .local_get = local }, repr);
            return .{ .value = repr };
        },
        .local_set => {
            const local = TirLocal{ .id = data.id };
            push(c, f, .{ .local_set = local }, null);
            _ = try reprUnion(c, &f.local_data.getPtr(local).repr, input.value);
            return;
        },
        .object_get => {
            const repr = switch (input.object) {
                .i32, .string, .repr, .fun, .only => return fail(c, .{ .not_an_object = input.object }),
                .@"struct" => |@"struct"| repr: {
                    const ix = @"struct".get(input.key) orelse
                        return fail(c, .{ .key_not_found = .{ .object = input.object, .key = input.key } });
                    break :repr @"struct".reprs[ix];
                },
                .@"union" => return fail(c, .todo),
            };
            push(c, f, .{ .object_get = .{ .key = input.key } }, repr);
            return .{ .value = repr };
        },
        .drop => {
            push(c, f, .drop, null);
            return;
        },
        .block_begin, .block_end => return,
        .@"return" => {
            push(c, f, .@"return", null);
            _ = try reprUnion(c, &f.return_repr, input.value);
            return;
        },
        .call => panic("Should be handled in infer, not inferExpr", .{}),
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
        c.repr_stack.append(@field(output, field.name)) catch oom();
    }
}

fn pushExprOutputValue(
    c: *Compiler,
    comptime expr_tag: std.meta.Tag(DirExprData),
    output: std.meta.TagPayload(zest.DirExprOutput(Value), expr_tag),
) void {
    const Output = std.meta.TagPayload(zest.DirExprOutput(Value), expr_tag);
    if (Output == void) return;
    inline for (@typeInfo(Output).Struct.fields) |field| {
        c.repr_stack.append(.{ .only = c.box(@field(output, field.name)) }) catch oom();
    }
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

fn popValue(c: *Compiler) error{InferError}!Value {
    return c.repr_stack.pop().valueOf() orelse
        fail(c, .not_compile_time_known);
}

fn push(c: *Compiler, f: *TirFunData, expr: TirExprData, repr: ?Repr) void {
    _ = c;
    _ = f.expr_data.append(expr);
    _ = f.expr_repr.append(repr);
}

fn fail(c: *Compiler, data: InferErrorData) error{InferError} {
    const frame = c.tir_frame_stack.items[c.tir_frame_stack.items.len - 1];
    c.error_data = .{ .infer = .{ .key = frame.key, .fun = frame.fun, .expr = frame.expr, .data = data } };
    return error.InferError;
}

pub const InferErrorData = union(enum) {
    not_compile_time_known,
    type_error: struct {
        expected: Repr,
        found: Repr,
    },
    staged_return,
    not_an_object: Repr,
    key_not_found: struct {
        object: Repr,
        key: Value,
    },
    not_a_fun: Repr,
    todo,
};
