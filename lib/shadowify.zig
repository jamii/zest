const std = @import("std");
const panic = std.debug.panic;
const assert = std.debug.assert;
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;

const zest = @import("./zest.zig");
const oom = zest.oom;
const List = zest.List;
const Map = zest.Map;
const Compiler = zest.Compiler;
const Specialization = zest.Specialization;
const SpecializationData = zest.SpecializationData;
const Arg = zest.Arg;
const Local = zest.Local;
const Shadow = zest.Shadow;
const Node = zest.Node;
const NodeData = zest.NodeData;
const Repr = zest.Repr;

/// NOTE node_repr and out_repr is invalid after this pass
pub fn shadowify(c: *Compiler) void {
    for (c.specialization_data.items()) |*s| {
        shadowifySpecialization(c, s);
    }
}

fn shadowifySpecialization(c: *Compiler, s: *SpecializationData) void {
    var local_to_shadow = Map(Local, Shadow).init(c.allocator);

    const return_arg = switch (s.out_repr) {
        .i32 => null,
        .@"struct" => s.in_repr.append(.i32),
        .string, .@"union" => panic("TODO {}", .{s.out_repr}),
    };
    var node_next = s.node_first;
    while (node_next) |node| {
        // Get next node before inserting anything around this node.
        node_next = s.node_next.get(node);
        shadowifyNode(c, s, &local_to_shadow, return_arg, node);
    }

    for (s.in_repr.items()) |*in_repr| {
        switch (in_repr.*) {
            .i32 => {},
            .@"struct" => in_repr.* = .i32,
            .string, .@"union" => panic("TODO {}", .{in_repr}),
        }
    }
}

fn shadowifyNode(c: *Compiler, s: *SpecializationData, local_to_shadow: *Map(Local, Shadow), return_arg: ?Arg, node: Node) void {
    const node_data = s.node_data.get(node);
    const repr = s.node_repr.get(node);
    switch (node_data) {
        .value => |value| {
            switch (value) {
                .i32 => {},
                .@"struct" => |@"struct"| {
                    if (@"struct".values.len == 0) {
                        // Replace with null ptr
                        s.node_data.getPtr(node).* = .{ .value = .{ .i32 = 0 } };
                    } else {
                        panic("TODO {}", .{value});
                    }
                },
                .string, .@"union" => panic("TODO {}", .{value}),
            }
        },
        .struct_init => |struct_init| {
            const shadow = s.shadow_repr.append(repr);
            s.node_data.getPtr(node).* = .{ .shadow_ptr = shadow };
            var offset: usize = 0;
            for (struct_init.values, repr.@"struct".reprs) |value, value_repr| {
                const offset_node = s.insertAfter(node, .{ .value = .{ .i32 = @intCast(offset) } });
                const address_node = s.insertAfter(offset_node, .{ .add = .{ node, offset_node } });
                switch (value_repr) {
                    .i32 => _ = s.insertAfter(address_node, .{ .store = .{
                        .address = address_node,
                        .value = value,
                    } }),
                    .@"struct" => _ = s.insertAfter(address_node, .{ .copy = .{
                        .to = address_node,
                        .from = value,
                        .byte_count = value_repr.sizeOf(),
                    } }),
                    .string, .@"union" => panic("TODO {}", .{node_data}),
                }
                offset += value_repr.sizeOf();
            }
        },
        .@"return" => |value| {
            if (return_arg) |arg| {
                const arg_node = s.insertBefore(node, .{ .arg_get = .{ .arg = arg } });
                _ = s.insertBefore(node, .{ .copy = .{
                    .to = arg_node,
                    .from = value,
                    .byte_count = repr.sizeOf(),
                } });
            } else {
                // Otherwise can just return
            }
        },
        .call => |call| {
            const return_repr = c.specialization_data.get(call.specialization.?).out_repr;
            switch (return_repr) {
                .i32 => {},
                .@"struct" => {
                    const shadow = s.shadow_repr.append(return_repr);
                    const return_node = s.insertBefore(node, .{ .shadow_ptr = shadow });
                    s.node_data.getPtr(node).call.args = std.mem.concat(
                        c.allocator,
                        Node,
                        &.{
                            call.args,
                            &.{return_node},
                        },
                    ) catch oom();
                },
                .string, .@"union" => panic("TODO {}", .{node_data}),
            }
        },
        .get => |get| {
            const object_repr = s.node_repr.get(get.object).@"struct";
            const index = object_repr.get(get.key).?;
            var offset: usize = 0;
            for (object_repr.reprs[0..index]) |repr_before| {
                offset += repr_before.sizeOf();
            }
            const offset_node = s.insertBefore(node, .{ .value = .{ .i32 = @intCast(offset) } });
            switch (repr) {
                .i32 => {
                    const address_node = s.insertBefore(node, .{ .add = .{ get.object, offset_node } });
                    s.node_data.getPtr(node).* = .{ .load = .{ .address = address_node, .repr = repr } };
                },
                .@"struct" => {
                    s.node_data.getPtr(node).* = .{ .add = .{ get.object, offset_node } };
                },
                .string, .@"union" => panic("TODO {}", .{node_data}),
            }
        },
        .arg_get => {
            // arg_get just retrieves struct pointers
        },
        .local_get => |local_get| {
            switch (repr) {
                .i32 => {},
                .@"struct" => {
                    const shadow = local_to_shadow.get(local_get.local).?;
                    s.node_data.getPtr(node).* = .{ .shadow_ptr = shadow };
                },
                .string, .@"union" => panic("TODO {}", .{node_data}),
            }
        },
        .local_set => |local_set| {
            const local_repr = s.local_repr.get(local_set.local);
            switch (local_repr) {
                .i32 => {},
                .@"struct" => {
                    s.local_repr.getPtr(local_set.local).* = .i32; // TODO delete local
                    const shadow = s.shadow_repr.append(local_repr);
                    local_to_shadow.put(local_set.local, shadow) catch oom();
                    const address_node = s.insertBefore(node, .{ .shadow_ptr = shadow });
                    s.node_data.getPtr(node).* = .{ .copy = .{
                        .to = address_node,
                        .from = local_set.value,
                        .byte_count = local_repr.sizeOf(),
                    } };
                    s.node_repr.getPtr(node).* = .i32;
                },
                .string, .@"union" => panic("TODO {}", .{node_data}),
            }
        },
        .add => {
            // No structs here.
        },
        .shadow_ptr, .load, .store, .copy => panic("Unexpected {}", .{node_data}),
    }
}
