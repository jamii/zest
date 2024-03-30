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

pub fn lower(c: *Compiler) error{LowerError}!void {
    const main = c.function_data.append(FunctionData.init(c.allocator));
    const result = try lowerExpr(c, main, c.expr_data.lastKey());
    _ = c.function_data.getPtr(main).node_data.append(.{ .@"return" = result });
}

pub fn lowerExpr(c: *Compiler, f: Function, expr: Expr) error{LowerError}!Node {
    const nodes = &c.function_data.getPtr(f).node_data;
    const expr_data = c.expr_data.get(expr);
    switch (expr_data) {
        .i32 => |i| {
            return nodes.append(.{ .i32 = i });
        },
        .statements => |statements| {
            if (statements.len == 0) {
                // TODO empty struct
                return nodes.append(.{ .i32 = 0 });
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
