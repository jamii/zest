const std = @import("std");
const panic = std.debug.panic;
const assert = std.debug.assert;
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;

const zest = @import("./zest.zig");
const oom = zest.oom;
const Compiler = zest.Compiler;
const Value = zest.Value;
const Repr = zest.Repr;
const FlatLattice = zest.FlatLattice;
const dir = zest.dir;
const tir = zest.tir;

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

fn pushFun(c: *Compiler, key: tir.FunKey) tir.Fun {
    const fun = c.tir_fun_data.append(tir.FunData.init(c.allocator, key));
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

fn inferFrame(c: *Compiler, frame: *tir.Frame) error{ EvalError, InferError }!enum { call, @"return" } {
    const dir_f = c.dir_fun_data.get(frame.key.fun);
    const f = c.tir_fun_data.getPtr(frame.fun);
    while (frame.expr.id <= dir_f.expr_data.lastKey().?.id) : (frame.expr.id += 1) {
        const expr_data = dir_f.expr_data.get(frame.expr);
        switch (expr_data) {
            .call => |call| {
                const input = try popExprInput(c, .call, call);
                if (input.fun != .fun)
                    return fail(c, .{ .not_a_fun = input.fun });
                const key = tir.FunKey{
                    .fun = input.fun.fun.fun,
                    .closure_repr = .{ .@"struct" = input.fun.fun.closure },
                    .arg_repr = input.args,
                };
                if (c.tir_fun_by_key.get(key)) |fun| {
                    // TODO once we have recursive functions, seeing a .zero here indicates that type inference is cyclic
                    const return_repr = c.tir_fun_data.get(fun).return_repr.one;
                    _ = pushExpr(c, f, .{ .call = fun }, return_repr);
                    pushExprOutput(c, .call, return_repr);
                } else {
                    // Put inputs back on stack and switch to the called function.
                    c.repr_stack.append(input.fun) catch oom();
                    c.repr_stack.append(input.args) catch oom();
                    _ = pushFun(c, key);
                    return .call;
                }
            },
            .stage => {
                eval.pushFun(c, .{
                    .fun = frame.key.fun,
                    .expr = frame.expr,
                    .arg = Value.emptyStruct(),
                    .closure = Value.emptyStruct(),
                });
                const return_value = try eval.evalStaged(c, f, frame.key.arg_repr, frame.key.closure_repr);
                const eval_frame = eval.popFun(c);
                frame.expr = eval_frame.expr;
                // Need to balance the begin.
                _ = pushExpr(c, f, .nop, null);
                c.repr_stack.append(.{ .only = c.box(return_value) }) catch oom();
            },
            inline else => |data, expr_tag| {
                const input = try popExprInput(c, expr_tag, data);
                const output = try inferExpr(c, f, expr_tag, data, input);
                pushExprOutput(c, expr_tag, output);
            },
        }
    }
    return .@"return";
}

fn popExprInput(
    c: *Compiler,
    comptime expr_tag: std.meta.Tag(dir.ExprData),
    data: std.meta.TagPayload(dir.ExprData, expr_tag),
) error{InferError}!std.meta.TagPayload(dir.ExprInput(Repr), expr_tag) {
    switch (expr_tag) {
        .i32, .f32, .string, .arg, .closure, .local_get, .begin, .stage, .unstage => return,
        .fun_init, .local_let, .object_get, .assert_object, .drop, .@"return", .call => {
            const Input = std.meta.TagPayload(dir.ExprInput(Repr), expr_tag);
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

fn inferExpr(
    c: *Compiler,
    f: *tir.FunData,
    comptime expr_tag: std.meta.Tag(dir.ExprData),
    data: std.meta.TagPayload(dir.ExprData, expr_tag),
    input: std.meta.TagPayload(dir.ExprInput(Repr), expr_tag),
) error{InferError}!std.meta.TagPayload(dir.ExprOutput(Repr), expr_tag) {
    switch (expr_tag) {
        .i32 => {
            pushExpr(c, f, .{ .i32 = data }, .i32);
            return .i32;
        },
        .string => {
            pushExpr(c, f, .{ .string = data }, .string);
            return .string;
        },
        .struct_init => {
            const repr = Repr{ .@"struct" = .{
                .keys = input.keys,
                .reprs = input.values,
            } };
            pushExpr(c, f, .struct_init, repr);
            return repr;
        },
        .fun_init => {
            const repr = Repr{ .fun = .{
                .fun = data.fun,
                .closure = input.closure.@"struct",
            } };
            pushExpr(c, f, .fun_init, repr);
            return repr;
        },
        .arg => {
            const frame = c.tir_frame_stack.items[c.tir_frame_stack.items.len - 1];
            const repr = frame.key.arg_repr;
            pushExpr(c, f, .arg, repr);
            return repr;
        },
        .closure => {
            const frame = c.tir_frame_stack.items[c.tir_frame_stack.items.len - 1];
            const repr = frame.key.closure_repr;
            pushExpr(c, f, .closure, repr);
            return repr;
        },
        .local_get => {
            const local = tir.Local{ .id = data.id };
            // Shouldn't be able to reach get before set.
            const repr = f.local_data.get(local).repr.one;
            pushExpr(c, f, .{ .local_get = local }, repr);
            return repr;
        },
        .local_let => {
            const local = tir.Local{ .id = data.id };
            _ = try reprUnion(c, &f.local_data.getPtr(local).repr, input.value);
            pushExpr(c, f, .{ .local_let = local }, null);
            return;
        },
        .assert_object => {
            switch (input.value) {
                .i32, .string, .repr, .fun, .only => return fail(c, .{ .not_an_object = input.value }),
                .@"struct" => {},
                .@"union" => return fail(c, .todo),
            }
            pushExpr(c, f, .drop, null);
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
            pushExpr(c, f, .{ .object_get = .{ .key = input.key } }, repr);
            return repr;
        },
        .drop => {
            pushExpr(c, f, .drop, null);
            return;
        },
        .begin => {
            pushExpr(c, f, .begin, null);
            return;
        },
        .@"return" => {
            _ = try reprUnion(c, &f.return_repr, input.value);
            pushExpr(c, f, .@"return", null);
            return;
        },
        .call, .stage => panic("Should be handled in inferFrame, not inferExpr", .{}),
        else => return fail(c, .todo),
    }
}

fn pushExprOutput(
    c: *Compiler,
    comptime expr_tag: std.meta.Tag(dir.ExprData),
    output: std.meta.TagPayload(dir.ExprOutput(Repr), expr_tag),
) void {
    switch (@TypeOf(output)) {
        void => return,
        Repr => c.repr_stack.append(output) catch oom(),
        else => @compileError(@typeName(@TypeOf(output))),
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
    const repr = c.repr_stack.pop();
    return repr.valueOf() orelse
        fail(c, .{ .value_not_staged = repr });
}

fn pushExpr(c: *Compiler, f: *tir.FunData, expr: tir.ExprData, repr: ?Repr) void {
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
    value_not_staged: Repr,
    type_error: struct {
        expected: Repr,
        found: Repr,
    },
    not_an_object: Repr,
    key_not_found: struct {
        object: Repr,
        key: Value,
    },
    not_a_fun: Repr,
    todo,
};
