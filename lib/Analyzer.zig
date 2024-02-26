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
const Value = @import("./Semantalyzer.zig").Value;

const Self = @This();
allocator: Allocator,
parser: Parser,
reprs: []?Repr,
places: []?Place,
functions: ArrayList(Function),
frame: ArrayList(usize),
frame_offset_max: usize,
stack_offset_max: usize,
error_message: ?[]const u8,

pub const Place = union(enum) {
    result,
    shadow: u32,
};

pub const Function = struct {
    name: []const u8,
    params: []const Repr,
    result: Repr,
    locals_count: u32,
    frame_size: usize,
    body: ExprId,
};

pub fn init(allocator: Allocator, parser: Parser) Self {
    const reprs = allocator.alloc(?Repr, parser.exprs.items.len) catch oom();
    for (reprs) |*repr| repr.* = null;

    const places = allocator.alloc(?Place, parser.exprs.items.len) catch oom();
    for (places) |*place| place.* = null;

    const functions = ArrayList(Function).init(allocator);

    var frame = ArrayList(usize).init(allocator);
    frame.append(0) catch oom();

    return .{
        .allocator = allocator,
        .parser = parser,
        .reprs = reprs,
        .places = places,
        .functions = functions,
        .frame = frame,
        .frame_offset_max = 0,
        .stack_offset_max = 8 << 20, // 8mb
        .error_message = null,
    };
}

pub fn analyze(self: *Self) error{AnalyzeError}!void {
    const main_id = self.parser.exprs.items.len - 1;
    _ = try self.reprOfExpr(main_id, null);
    _ = try self.placeExpr(main_id, .result);
    self.functions.append(.{
        .name = "main",
        .params = &.{},
        .result = self.reprs[main_id].?,
        .locals_count = 0,
        .frame_size = self.frame_offset_max,
        .body = main_id,
    }) catch oom();
}

fn reprOfExpr(self: *Self, expr_id: ExprId, repr_in: ?Repr) error{AnalyzeError}!Repr {
    const repr = try self.reprOfExprInner(expr_id, repr_in);
    // TODO check repr is compatible with repr_in
    self.reprs[expr_id] = repr;
    return repr;
}

fn reprOfExprInner(self: *Self, expr_id: ExprId, repr_in: ?Repr) error{AnalyzeError}!Repr {
    const expr = self.parser.exprs.items[expr_id];
    switch (expr) {
        .i64 => return .i64,
        .call => |call| {
            const head = self.parser.exprs.items[call.head];
            if (head != .builtin) return self.fail("TODO Can't analyze {}", .{expr});
            switch (head.builtin) {
                .add, .subtract => {
                    if (call.args.keys.len != 2) return self.fail("Wrong number of args to {}", .{expr});
                    const key0 = try self.evalConstant(call.args.keys[0]);
                    const key1 = try self.evalConstant(call.args.keys[1]);
                    if (key0 != .i64 and key0.i64 != 0) return self.fail("Wrong arg names to {}", .{expr});
                    if (key1 != .i64 and key1.i64 != 0) return self.fail("Wrong arg names to {}", .{expr});
                    const repr0 = try self.reprOfExpr(call.args.values[0], null);
                    const repr1 = try self.reprOfExpr(call.args.values[1], null);
                    if (repr0 != .i64) return self.fail("Expected i64, found {}", .{repr0});
                    if (repr1 != .i64) return self.fail("Expected i64, found {}", .{repr1});
                    return .i64;
                },
                else => return self.fail("TODO Can't analyze {}", .{head.builtin}),
            }
        },
        .statements => |statements| {
            if (statements.len == 0) {
                return Repr.emptyStruct();
            } else {
                for (statements[0 .. statements.len - 1]) |statement| {
                    _ = try self.reprOfExpr(statement, null);
                }
                return self.reprOfExpr(statements[statements.len - 1], repr_in);
            }
        },
        else => return self.fail("TODO Can't analyze {}", .{expr}),
    }
}

fn placeExpr(self: *Self, expr_id: ExprId, dest: Place) error{AnalyzeError}!void {
    self.places[expr_id] = dest;
    const expr = self.parser.exprs.items[expr_id];
    switch (expr) {
        .i64 => {},
        .call => |call| {
            const head = self.parser.exprs.items[call.head];
            if (head != .builtin) return self.fail("TODO Can't analyze {}", .{expr});
            switch (head.builtin) {
                .add, .subtract => {
                    try self.placeExpr(call.args.values[0], self.framePush(self.reprs[call.args.values[0]].?));
                    try self.placeExpr(call.args.values[1], self.framePush(self.reprs[call.args.values[1]].?));
                },
                else => return self.fail("TODO Can't analyze {}", .{head.builtin}),
            }
        },
        .statements => |statements| {
            for (statements, 0..) |statement, ix| {
                if (ix == statements.len - 1) {
                    try self.placeExpr(statement, dest);
                } else {
                    const statement_dest = self.framePush(self.reprs[expr_id].?);
                    defer self.framePop();

                    try self.placeExpr(statement, statement_dest);
                }
            }
        },
        else => return self.fail("TODO Can't analyze {}", .{expr}),
    }
}

fn framePush(self: *Self, repr: Repr) Place {
    const offset_prev = self.frame.items[self.frame.items.len - 1];
    const offset = offset_prev + repr.sizeOf(); // TODO alignment
    self.frame.append(offset) catch oom();
    self.frame_offset_max = @max(self.frame_offset_max, offset);
    return .{ .shadow = @intCast(offset) };
}

fn framePop(self: *Self) void {
    _ = self.frame.pop();
}

fn evalConstant(self: *Self, expr_id: ExprId) error{AnalyzeError}!Value {
    const expr = self.parser.exprs.items[expr_id];
    switch (expr) {
        .i64 => |num| return .{ .i64 = num },
        else => return self.fail("Cannot const eval {}", .{expr}),
    }
}

fn fail(self: *Self, comptime message: []const u8, args: anytype) error{AnalyzeError} {
    self.error_message = std.fmt.allocPrint(self.allocator, message, args) catch oom();
    return error.AnalyzeError;
}
