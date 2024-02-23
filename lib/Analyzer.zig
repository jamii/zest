const std = @import("std");
const panic = std.debug.panic;
const assert = std.debug.assert;
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;

const util = @import("./util.zig");
const oom = util.oom;
const Parser = @import("./Parser.zig");
const ExprId = Parser.ExprId;
const Expr = Parser.Expr;
const Repr = @import("./Semantalyzer.zig").Repr;

const Self = @This();
allocator: Allocator,
parser: Parser,
reprs: []?Repr,
error_message: ?[]const u8,

pub fn init(allocator: Allocator, parser: Parser) Self {
    const reprs = allocator.alloc(?Repr, parser.exprs.items.len) catch oom();
    for (reprs) |*repr| repr.* = null;
    return .{
        .allocator = allocator,
        .parser = parser,
        .reprs = reprs,
        .error_message = null,
    };
}

pub fn analyze(self: *Self) error{AnalyzeError}!void {
    _ = try self.analyzeExpr(self.parser.exprs.items.len - 1, null);
}

pub fn analyzeExpr(self: *Self, expr_id: ExprId, repr_in: ?Repr) error{AnalyzeError}!Repr {
    const repr = try self.analyzeExprInner(expr_id, repr_in);
    // TODO check repr is compatible with repr_in
    self.reprs[expr_id] = repr;
    return repr;
}

pub fn analyzeExprInner(self: *Self, expr_id: ExprId, repr_in: ?Repr) error{AnalyzeError}!Repr {
    const expr = self.parser.exprs.items[expr_id];
    switch (expr) {
        .i64 => return .i64,
        .statements => |statements| {
            if (statements.len == 0) {
                return Repr.emptyStruct();
            } else {
                for (statements[0 .. statements.len - 1]) |statement| {
                    _ = try self.analyzeExpr(statement, null);
                }
                return self.analyzeExpr(statements[statements.len - 1], repr_in);
            }
        },
        else => return self.fail("TODO Can't analyze {}", .{expr}),
    }
}

fn fail(self: *Self, comptime message: []const u8, args: anytype) error{AnalyzeError} {
    self.error_message = std.fmt.allocPrint(self.allocator, message, args) catch oom();
    return error.AnalyzeError;
}
