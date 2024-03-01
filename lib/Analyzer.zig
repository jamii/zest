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
place_hints: []?Place,
places: []?Place,
constants: []?Value,
name_lets: []?ExprId,
functions: ArrayList(Function),
frame_offset: usize,
frame_offset_max: usize,
stack_offset_max: usize,
scope: ArrayList(Binding),
error_message: ?[]const u8,

pub const Place = struct {
    base: enum { result, shadow },
    offset: u32,
    length: u32,

    pub fn empty() Place {
        return .{
            .base = .shadow,
            .offset = 0,
            .length = 0,
        };
    }

    pub fn equal(self: Place, other: Place) bool {
        return self.base == other.base and self.offset == other.offset and self.length == other.length;
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

pub const Binding = struct {
    mut: bool,
    name: []const u8,
    value_id: ExprId,
};

pub fn init(allocator: Allocator, parser: Parser) Self {
    const reprs = allocator.alloc(?Repr, parser.exprs.items.len) catch oom();
    for (reprs) |*repr| repr.* = null;

    const place_hints = allocator.alloc(?Place, parser.exprs.items.len) catch oom();
    for (place_hints) |*place_hint| place_hint.* = null;

    const places = allocator.alloc(?Place, parser.exprs.items.len) catch oom();
    for (places) |*place| place.* = null;

    const constants = allocator.alloc(?Value, parser.exprs.items.len) catch oom();
    for (constants) |*constant| constant.* = null;

    const name_lets = allocator.alloc(?ExprId, parser.exprs.items.len) catch oom();
    for (name_lets) |*name_let| name_let.* = null;

    const functions = ArrayList(Function).init(allocator);

    var frame = ArrayList(usize).init(allocator);
    frame.append(0) catch oom();

    return .{
        .allocator = allocator,
        .parser = parser,
        .reprs = reprs,
        .place_hints = place_hints,
        .places = places,
        .constants = constants,
        .name_lets = name_lets,
        .functions = functions,
        .frame_offset = 0,
        .frame_offset_max = 0,
        .stack_offset_max = 8 << 20, // 8mb
        .scope = ArrayList(Binding).init(allocator),
        .error_message = null,
    };
}

pub fn analyze(self: *Self) error{AnalyzeError}!void {
    const main_id = self.parser.exprs.items.len - 1;
    _ = try self.reprOfExpr(main_id, null);
    _ = try self.placeOfExpr(main_id, null);
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
    if (repr_in != null and repr_in.?.order(repr) != .eq)
        return self.fail("Expected {}, found {}", .{ repr_in.?, repr });
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
        .name => |name| {
            const binding = try self.lookup(name);
            self.name_lets[expr_id] = binding.value_id;
            return self.reprs[binding.value_id].?;
        },
        .let => |let| {
            _ = try self.reprOfExpr(let.value, null);
            self.scope.append(.{ .mut = let.mut, .name = let.name, .value_id = let.value }) catch oom();
            return Repr.emptyStruct();
        },
        .set => |set| {
            try self.assertIsPath(set.path);
            const path_repr = try self.reprOfExpr(set.path, null);
            _ = try self.reprOfExpr(set.value, path_repr);
            return Repr.emptyStruct();
        },
        .call => |call| {
            const head = self.parser.exprs.items[call.head];
            if (head != .builtin) return self.fail("TODO Can't analyze {}", .{expr});
            switch (head.builtin) {
                .equal,
                .less_than,
                .less_than_or_equal,
                .more_than,
                .more_than_or_equal,
                .add,
                .subtract,
                => {
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
            if (object_repr != .@"struct")
                return self.fail("Expected struct, found {}", .{object_repr});
            const ix = object_repr.@"struct".ixOf(key) orelse
                return self.fail("Key {} does not exist in {}", .{ key, object_repr });
            return object_repr.@"struct".reprs[ix];
        },
        .statements => |statements| {
            if (statements.len == 0) {
                return Repr.emptyStruct();
            } else {
                const scope_len = self.scope.items.len;
                for (statements[0 .. statements.len - 1]) |statement| {
                    _ = try self.reprOfExpr(statement, null);
                }
                const repr = self.reprOfExpr(statements[statements.len - 1], repr_in);
                self.scope.shrinkRetainingCapacity(scope_len);
                return repr;
            }
        },
        else => return self.fail("TODO Can't analyze {}", .{expr}),
    }
}

fn assertIsPath(self: *Self, expr_id: ExprId) error{AnalyzeError}!void {
    const expr = self.parser.exprs.items[expr_id];
    switch (expr) {
        .name => |name| {
            const binding = try self.lookup(name);
            if (!binding.mut) return self.fail("{s} is not a mutable variable", .{name});
        },
        .get => |get| {
            try self.assertIsPath(get.object);
        },
        else => return self.fail("{} is not valid in a path expression", .{expr}),
    }
}

fn placeOfExpr(self: *Self, expr_id: ExprId, hint: ?Place) error{AnalyzeError}!Place {
    const place = try self.placeOfExprInner(expr_id, hint);
    self.place_hints[expr_id] = hint;
    self.places[expr_id] = place;
    return place;
}

fn placeOfExprInner(self: *Self, expr_id: ExprId, hint: ?Place) error{AnalyzeError}!Place {
    const expr = self.parser.exprs.items[expr_id];
    const repr = self.reprs[expr_id].?;
    switch (expr) {
        .i64 => {
            return hint orelse self.framePush(repr);
        },
        .object => |object| {
            const place = hint orelse self.framePush(repr);
            const frame_offset_now = self.frame_offset;
            defer self.frame_offset = frame_offset_now;

            if (repr != .@"struct") return self.fail("TODO Can't analyze {}", .{expr});
            for (repr.@"struct".keys, object.values) |key, value_expr| {
                _ = try self.placeOfExpr(value_expr, repr.@"struct".placeOf(place, key).?);
            }

            return place;
        },
        .name => {
            const value_id = self.name_lets[expr_id].?;
            return self.places[value_id].?;
        },
        .let => |let| {
            _ = try self.placeOfExpr(let.value, null);
            return Place.empty();
        },
        .set => |set| {
            _ = try self.placeOfExpr(set.path, null);
            // TODO Would be nice to use path as hint here, but need to check for aliasing eg `x = [/a x/b, /b x/a]`
            _ = try self.placeOfExpr(set.value, null);
            return Place.empty();
        },
        .call => |call| {
            const place = hint orelse self.framePush(repr);
            const frame_offset_now = self.frame_offset;
            defer self.frame_offset = frame_offset_now;

            for (call.args.values) |value| {
                _ = try self.placeOfExpr(value, null);
            }

            return place;
        },
        .get => |get| {
            const object_place = try self.placeOfExpr(get.object, null);
            const object_repr = self.reprs[get.object].?;
            const key = self.constants[get.key].?;
            return object_repr.@"struct".placeOf(object_place, key).?;
        },
        .statements => |statements| {
            const place = hint orelse self.framePush(repr);
            const frame_offset_now = self.frame_offset;
            defer self.frame_offset = frame_offset_now;

            for (statements, 0..) |statement, ix| {
                _ = try self.placeOfExpr(statement, if (ix == statements.len - 1) place else null);
            }

            return place;
        },
        else => return self.fail("TODO Can't analyze {}", .{expr}),
    }
}

fn framePush(self: *Self, repr: Repr) Place {
    const offset = self.frame_offset;
    self.frame_offset += repr.sizeOf();
    self.frame_offset_max = @max(self.frame_offset_max, self.frame_offset);
    return .{
        .base = .shadow,
        .offset = @intCast(offset),
        .length = @intCast(repr.sizeOf()),
    };
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

fn lookup(self: *Self, name: []const u8) error{AnalyzeError}!Binding {
    var i = self.scope.items.len;
    while (i > 0) : (i -= 1) {
        const binding = self.scope.items[i - 1];
        if (std.mem.eql(u8, binding.name, name)) {
            return binding;
        }
    }
    return self.fail("Name {s} not in scope", .{name});
}

fn fail(self: *Self, comptime message: []const u8, args: anytype) error{AnalyzeError} {
    self.error_message = std.fmt.allocPrint(self.allocator, message, args) catch oom();
    return error.AnalyzeError;
}
