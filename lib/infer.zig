const std = @import("std");
const panic = std.debug.panic;
const assert = std.debug.assert;
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const wasm = std.wasm;

const zest = @import("./zest.zig");
const oom = zest.oom;
const Compiler = zest.Compiler;
const Function = zest.Function;
const FunctionData = zest.FunctionData;
const Specialization = zest.Specialization;
const SpecializationData = zest.SpecializationData;
const Node = zest.Node;
const NodeData = zest.NodeData;
const Repr = zest.Repr;

pub fn infer(c: *Compiler) error{InferError}!void {
    c.specialization_main = try inferFunction(c, c.function_main.?, &.{});
}

fn inferFunction(c: *Compiler, function: Function, in_reprs: []Repr) error{InferError}!Specialization {
    const function_data = c.function_data.get(function);

    var specialization_data = SpecializationData.init(c.allocator, function);
    specialization_data.in_reprs.appendSlice(in_reprs);
    specialization_data.node_data.appendSlice(function_data.node_data.items());
    for (0..specialization_data.node_data.count()) |node_id| {
        const repr = try inferExpr(c, &specialization_data, .{ .id = node_id });
        _ = specialization_data.node_reprs.append(repr);
    }
    return c.specialization_data.append(specialization_data);
}

fn inferExpr(c: *Compiler, s: *SpecializationData, node: Node) !Repr {
    const node_data = s.node_data.get(node);
    switch (node_data) {
        .value => |value| {
            return value.reprOf(c.allocator);
        },
        .@"return" => |returned_node| {
            const returned_repr = s.node_reprs.get(returned_node);
            if (s.out_repr) |out_repr| {
                if (!out_repr.equal(returned_repr)) {
                    return fail(c, s.function, node, "Expected {}, found {}", .{ out_repr, returned_repr });
                }
            } else {
                s.out_repr = returned_repr;
            }
            return Repr.emptyStruct();
        },
    }
}

fn fail(c: *Compiler, function: Function, node: Node, comptime message: []const u8, args: anytype) error{InferError} {
    const function_data = c.function_data.get(function);
    const node_data = function_data.node_data.get(node);
    c.error_message = std.fmt.allocPrint(
        c.allocator,
        "At {}={}, {}={}. " ++
            message,
        .{ function, function_data, node, node_data } ++
            args,
    ) catch oom();
    return error.InferError;
}
