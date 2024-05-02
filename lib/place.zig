const std = @import("std");
const panic = std.debug.panic;
const assert = std.debug.assert;
const Allocator = std.mem.Allocator;

const zest = @import("./zest.zig");
const oom = zest.oom;
const Compiler = zest.Compiler;
const Repr = zest.Repr;
const tir = zest.tir;

pub fn place(c: *Compiler) void {
    for (c.tir_fun_data.items()) |*f| {
        placeFun(c, f);
    }
}

fn placeFun(c: *Compiler, f: *tir.FunData) void {
    f.expr_address.appendNTimes(null, f.expr_data.count());

    assert(c.tir_address_stack.items.len == 0);

    var expr = f.expr_data.lastKey().?;
    while (true) {
        const expr_data = f.expr_data.get(expr);
        switch (expr_data) {
            inline else => |data, expr_tag| {
                const repr = f.expr_repr.get(expr);
                const output = popExprOutput(c, expr_tag);
                if (@TypeOf(output) != void) {
                    f.expr_address.getPtr(expr).* = output;
                }
                const input = placeExpr(c, f, expr_tag, data, repr, output);
                pushExprInput(c, expr_tag, input);
            },
        }
        if (expr.id == 0) break;
        expr.id -= 1;
    }

    assert(c.tir_address_stack.items.len == 0);
}

fn popExprOutput(
    c: *Compiler,
    comptime expr_tag: std.meta.Tag(tir.ExprData),
) std.meta.TagPayload(tir.ExprOutput(tir.Address), expr_tag) {
    const Output = std.meta.TagPayload(tir.ExprOutput(tir.Address), expr_tag);
    switch (Output) {
        void => return,
        tir.Address => return c.tir_address_stack.pop(),
        else => @compileError(@typeName(Output)),
    }
}

fn placeExpr(
    c: *Compiler,
    f: *tir.FunData,
    comptime expr_tag: std.meta.Tag(tir.ExprData),
    data: std.meta.TagPayload(tir.ExprData, expr_tag),
    repr: ?Repr,
    output: std.meta.TagPayload(tir.ExprOutput(tir.Address), expr_tag),
) std.meta.TagPayload(tir.ExprInput(tir.Address), expr_tag) {
    switch (expr_tag) {
        .i32, .f32, .string, .local_get, .arg, .closure, .block_begin, .block_end => return,
        .drop, .object_get => return .{ .base = .{ .shadow = f.shadow_data.append({}) } },
        .struct_init => {
            const repr_struct = repr.?.@"struct";
            const addresses = c.allocator.alloc(
                tir.Address,
                repr_struct.keys.len,
            ) catch oom();
            var input = output;
            for (addresses, 0..) |*address, i| {
                address.* = input;
                input.offset += repr_struct.reprs[i].sizeOf();
            }
            return addresses;
        },
        .fun_init => return output,
        .call => return .{
            .{ .base = .{ .shadow = f.shadow_data.append({}) } },
            .{ .base = .{ .shadow = f.shadow_data.append({}) } },
        },
        .local_let => return .{ .base = .{ .local = data } },
        .@"return" => return .{ .base = .@"return" },
    }
}

fn pushExprInput(
    c: *Compiler,
    comptime expr_tag: std.meta.Tag(tir.ExprData),
    input: std.meta.TagPayload(tir.ExprInput(tir.Address), expr_tag),
) void {
    switch (@TypeOf(input)) {
        void => {},
        tir.Address => c.tir_address_stack.append(input) catch oom(),
        [2]tir.Address => c.tir_address_stack.appendSlice(&input) catch oom(),
        []tir.Address => c.tir_address_stack.appendSlice(input) catch oom(),
        else => @compileError(@typeName(@TypeOf(input))),
    }
}
