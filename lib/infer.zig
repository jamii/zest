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
const Value = zest.Value;
const Repr = zest.Repr;
const FlatLattice = zest.FlatLattice;

pub fn inferMain(c: *Compiler) error{InferError}!void {
    assert(c.tir_frame_stack.items.len == 0);
    c.tir_fun_main = try pushFun(
        c,
        .{
            .fun = c.dir_fun_main.?,
            .closure_repr = Repr.emptyStruct(),
            .arg_repr = Repr.emptyStruct(),
        },
    );
    try infer(c);
}

fn pushFun(c: *Compiler, key: TirFunKey) error{InferError}!TirFun {
    const fun = c.tir_fun_data.append(TirFunData.init(c.allocator));
    const f = c.tir_fun_data.getPtr(fun);
    f.local_data.appendNTimes(.{ .repr = .zero }, c.dir_fun_data.get(key.fun).local_data.count());
    c.tir_frame_stack.append(.{ .key = key, .fun = fun, .expr = .{ .id = 0 } }) catch oom();
    return fun;
}

fn infer(c: *Compiler) error{InferError}!void {
    while (true) {
        const frame = &c.tir_frame_stack.items[c.tir_frame_stack.items.len - 1];
        const dir_f = c.dir_fun_data.get(frame.key.fun);
        const f = c.tir_fun_data.getPtr(frame.fun);
        while (frame.expr.id < dir_f.expr_data.lastKey().?.id) : (frame.expr.id += 1) {
            const expr_data = dir_f.expr_data.get(frame.expr);
            switch (expr_data) {
                inline else => |data, expr_tag| {
                    const input = try popExprInput(c, expr_tag, data);
                    const output = try inferExpr(c, f, expr_tag, data, input);
                    pushExprOutput(c, expr_tag, output);
                },
            }
        }
        _ = c.tir_frame_stack.pop();
        if (c.tir_frame_stack.items.len == 0)
            break;
        c.tir_frame_stack.items[c.tir_frame_stack.items.len - 1].expr.id += 1;
    }
}

fn popExprInput(
    c: *Compiler,
    comptime expr_tag: std.meta.Tag(DirExprData),
    data: std.meta.TagPayload(DirExprData, expr_tag),
) error{InferError}!std.meta.TagPayload(DirExprInput, expr_tag) {
    switch (expr_tag) {
        .i32, .f32, .string, .arg, .closure, .local_get => return,
        .fun_init, .local_set, .object_get, .drop, .@"return", .call => {
            const Input = std.meta.TagPayload(DirExprInput, expr_tag);
            var input: Input = undefined;
            const fields = @typeInfo(Input).Struct.fields;
            comptime var i: usize = fields.len;
            inline while (i > 0) : (i -= 1) {
                const field = fields[i - 1];
                @field(input, field.name) = switch (field.type) {
                    Repr => popRepr(c),
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
                reprs[data - 1 - i] = popRepr(c);
                keys[data - 1 - i] = try popValue(c);
            }
            return .{ .keys = keys, .values = reprs };
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
            // TODO sort?
            const repr = Repr{ .@"struct" = .{
                .keys = input.keys,
                .reprs = input.values,
            } };
            push(c, f, .{ .struct_init = data }, repr);
            return .{ .value = repr };
        },
        //.fun_init => {
        //    return .{ .value = .{ .fun = .{
        //        .repr = .{
        //            .fun = data.fun,
        //            .closure = input.closure.@"struct".repr,
        //        },
        //        .closure = input.closure.@"struct".values,
        //    } } };
        //},
        //.arg => {
        //    const frame = c.tir_frame_stack.items[c.tir_frame_stack.items.len - 1];
        //    return .{ .value = frame.arg };
        //},
        //.closure => {
        //    const frame = c.tir_frame_stack.items[c.tir_frame_stack.items.len - 1];
        //    return .{ .value = frame.closure };
        //},
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
        //.object_get => {
        //    const value = input.object.get(input.key) orelse
        //        return fail(c, .{ .get_missing = .{ .object = input.object, .key = input.key } });
        //    return .{ .value = value };
        //},
        .drop => return,
        .@"return" => {
            push(c, f, .@"return", null);
            _ = try reprUnion(c, &f.return_repr, input.value);
            return;
        },
        //.call => panic("Can't eval control flow expr: {}", .{expr_tag}),
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
        c.repr_or_value_stack.append(.{ .repr = @field(output, field.name) }) catch oom();
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

fn popRepr(c: *Compiler) Repr {
    return c.repr_or_value_stack.pop().reprOf();
}

fn popValue(c: *Compiler) error{InferError}!Value {
    switch (c.repr_or_value_stack.pop()) {
        .repr => return fail(c, .not_compile_time_known),
        .value => |value| return value,
    }
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
    todo,
};
