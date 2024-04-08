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

fn stackifySpecialization(c: *Compiler, s: *SpecializationData) void {
    var node_to_local = List(Node, ?Local).init(c.allocator);
    var node_next = s.node_first;
    while (node_next) |node| {
        // Get next node before inserting anything after this node.
        node_next = s.node_next.get(node);
        stackifyNode(c, s, &node_to_local, node);
    }
}

fn stackifyNode(c: *Compiler, s: *SpecializationData, node_to_local: *List(Node, ?Local), node: Node) void {
    _ = c;

    const node_data = s.node_data.get(node);

    // Load inputs
    switch (node_data) {
        .value, .local_get, .shadow_ptr => {},
        .local_set => |local_set| {
            _ = s.insertBefore(node, .{ .local_get = .{ .local = node_to_local.get(local_set.value).? } });
        },
        .@"return" => |returned_node| {
            _ = s.insertBefore(node, .{ .local_get = .{ .local = node_to_local.get(returned_node).? } });
        },
        .call => |call| {
            for (call.args) |arg| {
                _ = s.insertBefore(node, .{ .local_get = .{ .local = node_to_local.get(arg).? } });
            }
        },
        .intrinsic => |intrinsic| {
            switch (intrinsic) {
                .i32_add => |args| {
                    for (args) |arg| {
                        _ = s.insertBefore(node, .{ .local_get = .{ .local = node_to_local.get(arg).? } });
                    }
                },
            }
        },
        .load => |load| {
            _ = s.insertBefore(node, .{ .local_get = .{ .local = node_to_local.get(load.address).? } });
        },
        .store => |store| {
            _ = s.insertBefore(node, .{ .local_get = .{ .local = node_to_local.get(store.address).? } });
            _ = s.insertBefore(node, .{ .local_get = .{ .local = node_to_local.get(store.value).? } });
        },
        .copy => |copy| {
            _ = s.insertBefore(node, .{ .local_get = .{ .local = node_to_local.get(copy.to).? } });
            _ = s.insertBefore(node, .{ .local_get = .{ .local = node_to_local.get(copy.from).? } });
        },
        .struct_init => panic("Unexpected {}", .{node_data}),
    }

    // Store outputs
    const has_output = switch (node_data) {
        .value, .local_get, .call, .shadow_ptr, .load => true,
        .local_set, .@"return", .store, .copy => false,
        .intrinsic => |intrinsic| switch (intrinsic) {
            .i32_add => true,
        },
        .struct_init => panic("Unexpected {}", .{node_data}),
    };
    if (has_output) {
        const local = s.local_repr.append(s.node_repr.get(node));
        _ = node_to_local.append(local);
        _ = s.insertAfter(node, .{
            .local_set = .{
                .local = local,
                .value = node,
            },
        });
    }
}
