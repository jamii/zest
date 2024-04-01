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

pub fn lower(c: *Compiler) error{LowerError}!void {
    c.function_main = try lowerFunction(c, &.{}, c.expr_data.lastKey());
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
        .statements => |statements| {
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
