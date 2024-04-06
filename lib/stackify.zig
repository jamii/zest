const std = @import("std");
const panic = std.debug.panic;
const assert = std.debug.assert;
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;

const zest = @import("./zest.zig");
const oom = zest.oom;
const List = zest.List;
const Compiler = zest.Compiler;
const Specialization = zest.Specialization;
const SpecializationData = zest.SpecializationData;
const Local = zest.Local;
const Node = zest.Node;
const NodeData = zest.NodeData;
const Repr = zest.Repr;

pub fn stackify(c: *Compiler) void {
    for (c.specialization_data.items()) |*s| {
        stackifySpecialization(c, s);
    }
}

pub fn stackifySpecialization(c: *Compiler, s: *SpecializationData) void {
    var node_to_local = List(Node, ?Local).init(c.allocator);
    var node_next = s.node_first;
    while (node_next) |node| {
        // Get next node before inserting anything after this node.
        node_next = s.node_next.get(node);
        stackifyNode(c, s, &node_to_local, node);
    }
}

pub fn stackifyNode(c: *Compiler, s: *SpecializationData, node_to_local: *List(Node, ?Local), node: Node) void {
    _ = c;

    const node_data = s.node_data.get(node);

    // Load inputs
    switch (node_data) {
        .value, .local_get => {},
        .local_set => |local_set| {
            _ = s.insertBefore(node, .{
                .local_get = node_to_local.get(local_set.node).?,
            });
        },
        .@"return" => |returned_node| {
            _ = s.insertBefore(node, .{
                .local_get = node_to_local.get(returned_node).?,
            });
        },
        .call => |call| {
            for (call.args) |arg_node| {
                _ = s.insertBefore(node, .{
                    .local_get = node_to_local.get(arg_node).?,
                });
            }
        },
    }

    // Store ouputs
    switch (node_data) {
        .value, .local_get, .call => {
            const local = s.local_repr.append(s.node_repr.get(node));
            _ = node_to_local.append(local);
            _ = s.insertAfter(node, .{
                .local_set = .{
                    .local = local,
                    .node = node,
                },
            });
        },
        .local_set, .@"return" => {},
    }
}
