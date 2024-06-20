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
const Builtin = zest.Builtin;
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
    for (c.dir_fun_data.get(key.fun).local_data.items()) |dir_local_data| {
        _ = f.local_data.append(.{
            .repr = .zero,
            .is_tmp = dir_local_data.is_tmp,
        });
    }
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
        // Don't need to advance expr.id here - we'll revisit the call and use the cached tir_fun this time.
    }
}

fn inferFrame(c: *Compiler, frame: *tir.Frame) error{ EvalError, InferError }!enum { call, @"return" } {
    const dir_f = c.dir_fun_data.get(frame.key.fun);
    const f = c.tir_fun_data.getPtr(frame.fun);
    while (frame.expr.id <= dir_f.expr_data.lastKey().?.id) : (frame.expr.id += 1) {
        const expr_data = dir_f.expr_data.get(frame.expr);
        switch (expr_data) {
            .call => {
                const args = c.repr_stack.pop();
                const fun = c.repr_stack.pop();
                if (fun != .fun)
                    return fail(c, .{ .not_a_fun = fun });
                const key = tir.FunKey{
                    .fun = fun.fun.fun,
                    .closure_repr = .{ .@"struct" = fun.fun.closure },
                    .arg_repr = args,
                };
                if (c.tir_fun_by_key.get(key)) |tir_fun| {
                    // TODO once we have recursive functions, seeing a .zero here indicates that type inference is cyclic
                    const return_repr = c.tir_fun_data.get(tir_fun).return_repr.one;
                    _ = pushExpr(c, f, .{ .call = tir_fun }, return_repr);
                } else {
                    // Put inputs back on stack and switch to the called function.
                    // When we return to this expr we'll hit the cached tir_fun.
                    c.repr_stack.append(fun) catch oom();
                    c.repr_stack.append(args) catch oom();
                    _ = pushFun(c, key);
                    return .call;
                }
            },
            .stage => {
                frame.expr.id += 1;
                eval.pushFun(c, .{
                    .fun = frame.key.fun,
                    .expr = frame.expr,
                    .arg = Value.emptyStruct(),
                    .closure = Value.emptyStruct(),
                });
                const return_value = try eval.evalStaged(c, f, frame.key.arg_repr, frame.key.closure_repr);
                const eval_frame = eval.popFun(c);
                frame.expr = eval_frame.expr;
                c.repr_stack.append(.{ .only = c.box(return_value) }) catch oom();
            },
            else => {
                try inferExpr(c, f, expr_data);
            },
        }
    }
    return .@"return";
}

fn inferExpr(
    c: *Compiler,
    f: *tir.FunData,
    expr_data: dir.ExprData,
) error{InferError}!void {
    switch (expr_data) {
        .i32 => |i| {
            pushExpr(c, f, .{ .i32 = i }, .i32);
        },
        //.string => {
        //    pushExpr(c, f, .{ .string = data }, .string);
        //    return .string;
        //},
        .struct_init => |count| {
            const keys = c.allocator.alloc(Value, count) catch oom();
            const reprs = c.allocator.alloc(Repr, count) catch oom();
            for (0..count) |i| {
                const ix = count - 1 - i;
                reprs[ix] = c.repr_stack.pop();
                keys[ix] = try popValue(c);
            }
            const repr = Repr{ .@"struct" = .{
                .keys = keys,
                .reprs = reprs,
            } };
            pushExpr(c, f, .struct_init, repr);
        },
        .fun_init => |fun_init| {
            const closure = c.repr_stack.pop();
            const repr = Repr{ .fun = .{
                .fun = fun_init.fun,
                .closure = closure.@"struct",
            } };
            pushExpr(c, f, .fun_init, repr);
        },
        .arg => {
            const frame = c.tir_frame_stack.items[c.tir_frame_stack.items.len - 1];
            const repr = frame.key.arg_repr;
            pushExpr(c, f, .arg, repr);
        },
        .closure => {
            const frame = c.tir_frame_stack.items[c.tir_frame_stack.items.len - 1];
            const repr = frame.key.closure_repr;
            pushExpr(c, f, .closure, repr);
        },
        .local_get => |dir_local| {
            const local = tir.Local{ .id = dir_local.id };
            // Shouldn't be able to reach get before let.
            const repr = f.local_data.get(local).repr.one;
            pushExpr(c, f, .{ .local_get = local }, repr);
        },
        .local_let => |dir_local| {
            const value = c.repr_stack.pop();
            const local = tir.Local{ .id = dir_local.id };
            _ = try reprUnion(c, &f.local_data.getPtr(local).repr, value);
            pushExpr(c, f, .{ .local_let = local }, null);
        },
        .ref_init => {
            const value = c.repr_stack.pop();
            const repr = Repr{ .ref = c.box(value) };
            pushExpr(c, f, .ref_init, repr);
        },
        .assert_object => |assert_object| {
            const value = c.repr_stack.pop();
            switch (value) {
                .@"struct" => |@"struct"| {
                    if (@"struct".keys.len != assert_object.count)
                        return fail(c, .{ .wrong_number_of_keys = .{
                            .expected = assert_object.count,
                            .actual = @"struct".keys.len,
                        } });
                },
                .@"union" => return fail(c, .todo),
                .i32, .string, .repr, .fun, .only, .ref => return fail(c, .{ .expected_object = value }),
            }
            pushExpr(c, f, .nop, value);
        },
        .assert_is_ref => {
            const value = c.repr_stack.pop();
            if (value != .ref)
                return fail(c, .{ .expected_is_ref = value });
            pushExpr(c, f, .nop, value);
        },
        .assert_has_no_ref => {
            const value = c.repr_stack.pop();
            if (value.hasRef())
                return fail(c, .{ .expected_has_no_ref = value });
            pushExpr(c, f, .nop, value);
        },
        .object_get => {
            const key = try popValue(c);
            const object = c.repr_stack.pop();
            const get = try objectGet(c, object, key);
            pushExpr(c, f, .{ .object_get = .{ .index = get.index, .offset = get.offset } }, get.repr);
        },
        .ref_get => {
            const key = try popValue(c);
            const ref = c.repr_stack.pop();
            const get = try objectGet(c, ref.ref.*, key);
            const repr = Repr{ .ref = c.box(get.repr) };
            pushExpr(c, f, .{ .ref_get = .{ .index = get.index, .offset = get.offset } }, repr);
        },
        .ref_set => {
            const value = c.repr_stack.pop();
            const ref = c.repr_stack.pop();
            if (!ref.ref.equal(value))
                return fail(c, .{ .type_error = .{
                    .expected = ref.ref.*,
                    .found = value,
                } });
            pushExpr(c, f, .ref_set, null);
        },
        .ref_deref => {
            const ref = c.repr_stack.pop();
            const repr = ref.ref.*;
            pushExpr(c, f, .ref_deref, repr);
        },
        .call_builtin => |builtin| {
            const args = c.repr_stack.pop();
            switch (builtin) {
                .add => {
                    const arg0 = args.@"struct".reprs[0];
                    const arg1 = args.@"struct".reprs[1];
                    if (arg0 != .i32 or arg1 != .i32)
                        return fail(c, .{ .invalid_call_builtin = .{ .builtin = builtin, .args = args } });
                    pushExpr(c, f, .{ .call_builtin = .add_i32 }, .i32);
                },
                .subtract => {
                    const arg0 = args.@"struct".reprs[0];
                    const arg1 = args.@"struct".reprs[1];
                    if (arg0 != .i32 or arg1 != .i32)
                        return fail(c, .{ .invalid_call_builtin = .{ .builtin = builtin, .args = args } });
                    pushExpr(c, f, .{ .call_builtin = .subtract_i32 }, .i32);
                },
                .multiply => {
                    const arg0 = args.@"struct".reprs[0];
                    const arg1 = args.@"struct".reprs[1];
                    if (arg0 != .i32 or arg1 != .i32)
                        return fail(c, .{ .invalid_call_builtin = .{ .builtin = builtin, .args = args } });
                    pushExpr(c, f, .{ .call_builtin = .multiply_i32 }, .i32);
                },
                else => return fail(c, .todo),
            }
        },
        .block => |count| {
            if (count == 0) {
                pushExpr(c, f, .{ .block = count }, Repr.emptyStruct());
            } else {
                const repr = c.repr_stack.pop();
                for (0..count - 1) |_|
                    _ = c.repr_stack.pop();
                pushExpr(c, f, .{ .block = count }, repr);
            }
        },
        .@"if" => {
            const @"else" = c.repr_stack.pop();
            const then = c.repr_stack.pop();
            const cond = c.repr_stack.pop();
            if (cond != .i32)
                return fail(c, .{ .not_a_bool = cond });
            if (!then.equal(@"else"))
                return fail(c, .{ .type_error = .{ .expected = then, .found = @"else" } });
            pushExpr(c, f, .@"if", then);
        },
        .then => {
            pushExpr(c, f, .then, null);
        },
        .@"else" => {
            pushExpr(c, f, .@"else", null);
        },
        .@"return" => {
            const value = c.repr_stack.pop();
            _ = try reprUnion(c, &f.return_repr, value);
            pushExpr(c, f, .@"return", null);
        },
        .begin => {
            pushExpr(c, f, .begin, null);
        },
        .nop => {
            pushExpr(c, f, .nop, null);
        },
        .call, .stage => panic("Should be handled in inferFrame, not inferExpr", .{}),
        else => return fail(c, .todo),
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

fn objectGet(c: *Compiler, object: Repr, key: Value) error{InferError}!struct { index: usize, repr: Repr, offset: u32 } {
    switch (object) {
        .i32, .string, .repr, .fun, .only => return fail(c, .{ .expected_object = object }),
        .@"struct" => |@"struct"| {
            const ix = @"struct".get(key) orelse
                return fail(c, .{ .key_not_found = .{ .object = object, .key = key } });
            return .{
                .index = ix,
                .repr = @"struct".reprs[ix],
                .offset = @intCast(@"struct".offsetOf(ix)),
            };
        },
        .@"union" => return fail(c, .todo),
        .ref => unreachable, // always passes through ref_deref first
    }
}

fn popValue(c: *Compiler) error{InferError}!Value {
    const repr = c.repr_stack.pop();
    return repr.valueOf() orelse
        fail(c, .{ .value_not_staged = repr });
}

fn pushExpr(c: *Compiler, f: *tir.FunData, expr: tir.ExprData, repr: ?Repr) void {
    _ = f.expr_data.append(expr);
    _ = f.expr_repr.append(repr);
    if (repr != null) c.repr_stack.append(repr.?) catch oom();
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
    wrong_number_of_keys: struct {
        expected: usize,
        actual: usize,
    },
    expected_object: Repr,
    expected_is_ref: Repr,
    expected_has_no_ref: Repr,
    key_not_found: struct {
        object: Repr,
        key: Value,
    },
    not_a_fun: Repr,
    not_a_bool: Repr,
    invalid_call_builtin: struct {
        builtin: Builtin,
        args: Repr,
    },
    todo,
};
