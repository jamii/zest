const std = @import("std");
const panic = std.debug.panic;
const assert = std.debug.assert;
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;

const zest = @import("./zest.zig");
const oom = zest.oom;
const Compiler = zest.Compiler;
const Expr = zest.Expr;
const ExprData = zest.ExprData;
const Function = zest.Function;
const FunctionData = zest.FunctionData;
const Node = zest.Node;
const NodeData = zest.NodeData;
const Value = zest.Value;
const AbstractValue = zest.AbstractValue;

pub fn lower(c: *Compiler) error{LowerError}!void {
    c.function_main = try lowerFunction(c, &.{}, c.expr_data.lastKey().?);
}

fn lowerFunction(c: *Compiler, args: []const []const u8, body: Expr) error{LowerError}!Function {
    _ = args;

    var function_data = FunctionData.init(c.allocator);
    const result_node = try lowerExpr(c, &function_data, body);
    _ = function_data.node_data.append(.{ .@"return" = result_node });
    return c.function_data.append(function_data);
}

fn lowerExpr(c: *Compiler, f: *FunctionData, expr: Expr) error{LowerError}!Node {
    const expr_data = c.expr_data.get(expr);
    switch (expr_data) {
        .i32 => |i| {
            return f.node_data.append(.{ .value = .{ .i32 = i } });
        },
        .object => |object| {
            const keys = c.allocator.alloc(Value, object.keys.len) catch oom();
            for (keys, object.keys) |*key_node, key| key_node.* = try evalKey(c, f, key);

            const values = c.allocator.alloc(Node, object.values.len) catch oom();
            for (values, object.values) |*value_node, value| value_node.* = try lowerExpr(c, f, value);

            // TODO sort keys and values
            return f.node_data.append(.{ .struct_init = .{ .keys = keys, .values = values } });
        },
        .name => |name| {
            const binding = c.scope.lookup(name) orelse
                return fail(c, expr, "Not defined: {s}", .{name});
            switch (binding.value) {
                .node => |node| return node,
                .function => return fail(c, expr, "You may not use a function here", .{}),
            }
        },
        .let => |let| {
            const value = try lowerExprOrFn(c, f, let.value);
            if (let.mut) return fail(c, expr, "TODO", .{});
            c.scope.push(.{
                .name = let.name,
                .value = value,
            });
            return f.node_data.append(.{ .value = Value.emptyStruct() });
        },
        .@"fn" => return fail(c, expr, "You may not create a function here", .{}),
        .call => |call| {
            if (call.args.keys.len > 0) return fail(c, expr, "TODO params", .{});
            const head = try lowerExprOrFn(c, f, call.head);
            if (head != .function) return fail(c, expr, "Cannot call {}", .{head});
            return f.node_data.append(.{ .call = .{
                .function = head.function,
                .specialization = null,
                .args = &.{},
            } });
        },
        .get => |get| {
            const object = try lowerExpr(c, f, get.object);
            const key = try evalKey(c, f, get.key);
            return f.node_data.append(.{ .get = .{ .object = object, .key = key } });
        },
        .statements => |statements| {
            const scope_saved = c.scope.save();
            defer c.scope.restore(scope_saved);

            if (statements.len == 0) {
                return f.node_data.append(.{ .value = Value.emptyStruct() });
            } else {
                var node: ?Node = null;
                for (statements) |statement| {
                    node = try lowerExpr(c, f, statement);
                }
                return node.?;
            }
        },
        else => return fail(c, expr, "TODO", .{}),
    }
}

fn lowerExprOrFn(c: *Compiler, f: *FunctionData, expr: Expr) error{LowerError}!AbstractValue {
    const expr_data = c.expr_data.get(expr);
    switch (expr_data) {
        .name => |name| {
            const binding = c.scope.lookup(name) orelse
                return fail(c, expr, "Not defined: {s}", .{name});
            return binding.value;
        },
        .@"fn" => |@"fn"| {
            if (@"fn".params.keys.len > 0) return fail(c, expr, "TODO params", .{});
            return .{ .function = try lowerFunction(c, &.{}, @"fn".body) };
        },
        else => {
            return .{ .node = try lowerExpr(c, f, expr) };
        },
    }
}

fn evalKey(c: *Compiler, f: *FunctionData, expr: Expr) error{LowerError}!Value {
    const expr_data = c.expr_data.get(expr);
    switch (expr_data) {
        .name => |name| return .{ .string = name },
        else => return evalExpr(c, f, expr),
    }
}

fn evalExpr(c: *Compiler, f: *FunctionData, expr: Expr) error{LowerError}!Value {
    _ = f;
    const expr_data = c.expr_data.get(expr);
    switch (expr_data) {
        .i32 => |i| return .{ .i32 = i },
        .string => |string| return .{ .string = string },
        else => return fail(c, expr, "Can't const-eval", .{}),
    }
}

fn fail(c: *Compiler, expr: Expr, comptime message: []const u8, args: anytype) error{LowerError} {
    const expr_data = c.expr_data.get(expr);
    c.error_message = std.fmt.allocPrint(
        c.allocator,
        "At {}={}. " ++
            message,
        .{ expr, expr_data } ++
            args,
    ) catch oom();
    return error.LowerError;
}
