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

    const return_arg = if (isPrimitive(s.out_repr))
        null
    else
        s.in_repr.append(.i32);

    var node_next = s.node_first;
    while (node_next) |node| {
        // Get next node before inserting anything around this node.
        node_next = s.node_next.get(node);
        shadowifyNode(c, s, &local_to_shadow, return_arg, node);
    }

    for (s.in_repr.items()) |*in_repr| {
        if (!isPrimitive(in_repr.*)) in_repr.* = .i32;
    }
}

fn shadowifyNode(c: *Compiler, s: *SpecializationData, local_to_shadow: *Map(Local, Shadow), return_arg: ?Arg, node: Node) void {
    const node_data = s.node_data.get(node);
    const repr = s.node_repr.get(node);
    switch (node_data) {
        .value => |value| {
            if (isPrimitive(repr)) return;

            switch (value) {
                .@"struct" => |@"struct"| {
                    if (@"struct".values.len == 0) {
                        // Replace with null ptr
                        s.node_data.getPtr(node).* = .{ .value = .{ .i32 = 0 } };
                    } else {
                        panic("TODO {}", .{value});
                    }
                },
                .i32, .string, .@"union", .repr => unreachable,
            }
        },
        .struct_init => |struct_init| {
            const shadow = s.shadow_repr.append(repr);
            s.node_data.getPtr(node).* = .{ .shadow_ptr = shadow };
            var offset: usize = 0;
            for (struct_init.values, repr.@"struct".reprs) |value, value_repr| {
                const offset_node = s.insertAfter(node, .{ .value = .{ .i32 = @intCast(offset) } });
                const address_node = s.insertAfter(offset_node, .{ .add = .{ node, offset_node } });
                if (isPrimitive(value_repr)) {
                    _ = s.insertAfter(address_node, .{ .store = .{
                        .to = address_node,
                        .value = value,
                    } });
                } else {
                    const byte_count_node = s.insertAfter(address_node, .{ .value = .{
                        .i32 = @intCast(value_repr.sizeOf()),
                    } });
                    _ = s.insertAfter(byte_count_node, .{ .copy = .{
                        .to = address_node,
                        .from = value,
                        .byte_count = byte_count_node,
                    } });
                }
                offset += value_repr.sizeOf();
            }
        },
        .@"return" => |value| {
            if (isPrimitive(s.node_repr.get(value))) return;

            const arg_node = s.insertBefore(node, .{ .arg_get = .{ .arg = return_arg.? } });
            const byte_count_node = s.insertBefore(node, .{ .value = .{
                .i32 = @intCast(repr.sizeOf()),
            } });
            _ = s.insertBefore(node, .{ .copy = .{
                .to = arg_node,
                .from = value,
                .byte_count = byte_count_node,
            } });
        },
        .call => |call| {
            const return_repr = c.specialization_data.get(call.specialization.?).out_repr;
            if (isPrimitive(return_repr)) return;

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
        .get => |get| {
            const object_repr = s.node_repr.get(get.object).@"struct";
            const index = object_repr.get(get.key).?;
            var offset: usize = 0;
            for (object_repr.reprs[0..index]) |repr_before| {
                offset += repr_before.sizeOf();
            }
            const offset_node = s.insertBefore(node, .{ .value = .{ .i32 = @intCast(offset) } });
            if (isPrimitive(repr)) {
                const address_node = s.insertBefore(node, .{ .add = .{ get.object, offset_node } });
                s.node_data.getPtr(node).* = .{ .load = .{ .from = address_node, .repr = repr } };
            } else {
                s.node_data.getPtr(node).* = .{ .add = .{ get.object, offset_node } };
            }
        },
        .arg_get => {
            // arg_get just retrieves struct pointers
        },
        .local_get => |local_get| {
            if (isPrimitive(repr)) return;

            const shadow = local_to_shadow.get(local_get.local).?;
            s.node_data.getPtr(node).* = .{ .shadow_ptr = shadow };
        },
        .local_set => |local_set| {
            const local_repr = s.local_repr.get(local_set.local);
            if (isPrimitive(local_repr)) return;

            s.local_repr.getPtr(local_set.local).* = .i32; // TODO delete local
            const shadow = s.shadow_repr.append(local_repr);
            local_to_shadow.put(local_set.local, shadow) catch oom();
            const address_node = s.insertBefore(node, .{ .shadow_ptr = shadow });
            const byte_count_node = s.insertBefore(node, .{ .value = .{
                .i32 = @intCast(local_repr.sizeOf()),
            } });
            s.node_data.getPtr(node).* = .{ .copy = .{
                .to = address_node,
                .from = local_set.value,
                .byte_count = byte_count_node,
            } };
            s.node_repr.getPtr(node).* = .i32;
        },
        .get_repr_data => {
            panic("TODO unions {}", .{node_data});
            //return Repr.reprData();
        },
        .add, .load, .store, .copy, .stack_top => {
            // No structs here.
        },
        .shadow_ptr => panic("Unexpected {}", .{node_data}),
    }
}

fn isPrimitive(repr: Repr) bool {
    return switch (repr) {
        .i32 => true,
        .@"struct" => false,
        .string, .@"union", .repr => panic("TODO {}", .{repr}),
    };
}
