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

pub fn shadowify(c: *Compiler) void {
    for (c.specialization_data.items()) |*s| {
        shadowifySpecialization(c, s);
    }
}

pub fn shadowifySpecialization(c: *Compiler, s: *SpecializationData) void {
    var node_next = s.node_first;
    while (node_next) |node| {
        // Get next node before inserting anything after this node.
        node_next = s.node_next.get(node);
        shadowifyNode(c, s, node);
    }
}

pub fn shadowifyNode(c: *Compiler, s: *SpecializationData, node: Node) void {
    _ = c;
    _ = s;
    _ = node;
    // TODO have previous passes kept node_repr up to date?
    //switch (s.node_repr.get(node)) {
    //    .i32, .string => return,
    //    .@"struct" => {},
    //    .@"union" => panic("TODO {}", .{node}),
    //}

    //const node_data = s.node_data.get(node);
    //switch (node_data) {
    //    .value => |value| panic("TODO {}", .{node_data}),
    //    .struct_init => |struct_init| {
    //        s.insertBefore(node, )
    //    },
    //    else => panic("TODO {}", .{node_data}),
    //}
    // TODO
}
