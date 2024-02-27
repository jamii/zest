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
const StructRepr = @import("./Semantalyzer.zig").StructRepr;
const Value = @import("./Semantalyzer.zig").Value;

const Self = @This();
allocator: Allocator,
parser: Parser,
reprs: []?Repr,
places: []?Place,
constants: []?Value,
functions: ArrayList(Function),
frame_offset: usize,
frame_offset_max: usize,
stack_offset_max: usize,
error_message: ?[]const u8,

pub const Place = struct {
    base: enum { result, shadow },
    offset: u32,

    pub fn equal(self: Place, other: Place) bool {
        return self.base == other.base and self.offset == other.offset;
    }

    pub fn offsetBy(self: Place, offset: u32) Place {
        return .{ .base = self.base, .offset = self.offset + offset };
    }
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

    const constants = allocator.alloc(?Value, parser.exprs.items.len) catch oom();
    for (constants) |*constant| constant.* = null;

    const functions = ArrayList(Function).init(allocator);

    var frame = ArrayList(usize).init(allocator);
    frame.append(0) catch oom();

    return .{
        .allocator = allocator,
        .parser = parser,
        .reprs = reprs,
        .places = places,
        .constants = constants,
        .functions = functions,
        .frame_offset = 0,
        .frame_offset_max = 0,
        .stack_offset_max = 8 << 20, // 8mb
        .error_message = null,
    };
}

pub fn analyze(self: *Self) error{AnalyzeError}!void {
    const main_id = self.parser.exprs.items.len - 1;
    _ = try self.reprOfExpr(main_id, null);
    _ = try self.placeExpr(main_id, .{ .base = .result, .offset = 0 });
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
        .object => |object| {
            const keys = self.allocator.alloc(Value, object.keys.len) catch oom();
            for (keys, object.keys) |*key, key_expr| key.* = try self.evalConstantKey(key_expr);
            const reprs = self.allocator.alloc(Repr, object.values.len) catch oom();
            for (reprs, object.values, 0..) |*repr, value_expr, ix| {
                const value_repr_in = if (repr_in != null and
                    repr_in.? == .@"struct" and
                    repr_in.?.@"struct".reprs.len == reprs.len)
                    repr_in.?.@"struct".reprs[ix]
                else
                    null;
                repr.* = try self.reprOfExpr(value_expr, value_repr_in);
            }
            return .{ .@"struct" = StructRepr.sorted(self.allocator, keys, reprs) };
        },
        .call => |call| {
            const head = self.parser.exprs.items[call.head];
            if (head != .builtin) return self.fail("TODO Can't analyze {}", .{expr});
            switch (head.builtin) {
                .add, .subtract => {
                    if (call.args.keys.len != 2) return self.fail("Wrong number of args to {}", .{expr});
                    const key0 = try self.evalConstantKey(call.args.keys[0]);
                    const key1 = try self.evalConstantKey(call.args.keys[1]);
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
        .get => |get| {
            const object_repr = try self.reprOfExpr(get.object, null);
            const key = try self.evalConstantKey(get.key);
            if (object_repr != .@"struct") return self.fail("Expected struct, found {}", .{object_repr});
            const ix = object_repr.@"struct".ixOf(key) orelse
                return self.fail("Key {} does not exist in {}", .{ key, object_repr });
            return object_repr.@"struct".reprs[ix];
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

fn placeExpr(self: *Self, expr_id: ExprId, maybe_dest: ?Place) error{AnalyzeError}!void {
    // Allocate stack for result if needed.
    const repr = self.reprs[expr_id].?;
    const dest = maybe_dest orelse self.framePush(repr);
    self.places[expr_id] = dest;

    // After subexprs, restore stack to current position.
    const frame_offset_now = self.frame_offset;
    defer self.frame_offset = frame_offset_now;

    const expr = self.parser.exprs.items[expr_id];
    switch (expr) {
        .i64 => {},
        .object => |object| {
            if (repr != .@"struct") return self.fail("TODO Can't analyze {}", .{expr});
            // objects.keys are constant
            for (repr.@"struct".keys, object.values) |key, value_expr| {
                try self.placeExpr(value_expr, dest.offsetBy(@intCast(repr.@"struct".offsetOf(key).?)));
            }
        },
        .call => |call| {
            const head = self.parser.exprs.items[call.head];
            if (head != .builtin) return self.fail("TODO Can't analyze {}", .{expr});
            switch (head.builtin) {
                .add, .subtract => {
                    try self.placeExpr(call.args.values[0], null);
                    try self.placeExpr(call.args.values[1], null);
                },
                else => return self.fail("TODO Can't analyze {}", .{head.builtin}),
            }
        },
        .get => |get| {
            try self.placeExpr(get.object, null);
            // get.key is constant
        },
        .statements => |statements| {
            for (statements, 0..) |statement, ix| {
                try self.placeExpr(statement, if (ix == statements.len - 1) dest else null);
            }
        },
        else => return self.fail("TODO Can't analyze {}", .{expr}),
    }
}

fn framePush(self: *Self, repr: Repr) Place {
    self.frame_offset += repr.sizeOf();
    self.frame_offset_max = @max(self.frame_offset_max, self.frame_offset);
    return .{ .base = .shadow, .offset = @intCast(self.frame_offset) };
}

fn framePop(self: *Self) void {
    _ = self.frame.pop();
}

fn evalConstantKey(self: *Self, expr_id: ExprId) error{AnalyzeError}!Value {
    const expr = self.parser.exprs.items[expr_id];
    const value: Value = switch (expr) {
        .name => |name| .{ .string = self.allocator.dupe(u8, name) catch oom() },
        else => return self.evalConstant(expr_id),
    };
    self.constants[expr_id] = value;
    return value;
}

fn evalConstant(self: *Self, expr_id: ExprId) error{AnalyzeError}!Value {
    const expr = self.parser.exprs.items[expr_id];
    const value: Value = switch (expr) {
        .i64 => |num| .{ .i64 = num },
        else => return self.fail("Cannot const eval {}", .{expr}),
    };
    self.constants[expr_id] = value;
    return value;
}

fn fail(self: *Self, comptime message: []const u8, args: anytype) error{AnalyzeError} {
    self.error_message = std.fmt.allocPrint(self.allocator, message, args) catch oom();
    return error.AnalyzeError;
}
