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
    name: []const u8,
    mutable: bool,
    value_id: ExprId,
};

pub fn init(allocator: Allocator, parser: Parser) Self {
    const reprs = allocator.alloc(?Repr, parser.exprs.items.len) catch oom();
    for (reprs) |*repr| repr.* = null;

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
    _ = try self.placeExpr(main_id, null);
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
            const path = self.parser.exprs.items[let.path];
            switch (path) {
                .name => {
                    const value = self.parser.exprs.items[let.value];
                    const mutable = value == .mut;
                    const value_id = if (mutable) value.mut else let.value;
                    _ = try self.reprOfExpr(value_id, null);
                    self.scope.append(.{ .name = path.name, .mutable = mutable, .value_id = value_id }) catch oom();
                },
                .mut => |mut| {
                    const path_repr = try self.reprOfPath(mut);
                    _ = try self.reprOfExpr(let.value, path_repr);
                },
                else => return self.fail("{} cannot appear on the left-hand side of an assignment (=).", .{path}),
            }
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
            return self.reprOfGet(object_repr, get.key);
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

fn reprOfPath(self: *Self, expr_id: ExprId) error{AnalyzeError}!Repr {
    const expr = self.parser.exprs.items[expr_id];
    switch (expr) {
        .name => |name| {
            const binding = try self.lookup(name);
            self.name_lets[expr_id] = binding.value_id;
            if (!binding.mutable) return self.fail("{s} is not a mutable variable", .{name});
            return self.reprs[binding.value_id].?;
        },
        .get => |get| {
            const object_repr = try self.reprOfPath(get.object);
            return self.reprOfGet(object_repr, get.key);
        },
        else => return self.fail("{} is not valid in a path expression", .{expr}),
    }
}

fn reprOfGet(self: *Self, object_repr: Repr, key_id: ExprId) error{AnalyzeError}!Repr {
    const key = try self.evalConstantKey(key_id);
    if (object_repr != .@"struct") return self.fail("Expected struct, found {}", .{object_repr});
    const ix = object_repr.@"struct".ixOf(key) orelse
        return self.fail("Key {} does not exist in {}", .{ key, object_repr });
    return object_repr.@"struct".reprs[ix];
}

fn placeExpr(self: *Self, expr_id: ExprId, maybe_dest: ?Place) error{AnalyzeError}!void {
    // Allocate stack for result if needed.
    const repr = self.reprs[expr_id].?;
    const dest = maybe_dest orelse self.framePush(repr);
    self.places[expr_id] = dest;

    const frame_offset_now = self.frame_offset;

    const expr = self.parser.exprs.items[expr_id];
    switch (expr) {
        .i64 => {},
        .object => |object| {
            if (repr != .@"struct") return self.fail("TODO Can't analyze {}", .{expr});
            // objects.keys are constant
            for (repr.@"struct".keys, object.values) |key, value_expr| {
                try self.placeExpr(value_expr, repr.@"struct".placeOf(dest, key).?);
            }
            self.frame_offset = frame_offset_now;
        },
        .name => {},
        .let => |let| {
            const path = self.parser.exprs.items[let.path];
            switch (path) {
                .name => {
                    const value = self.parser.exprs.items[let.value];
                    const mutable = value == .mut;
                    const value_id = if (mutable) value.mut else let.value;
                    try self.placeExpr(value_id, null);
                    // Don't reset frame - we need to keep this variable!
                },
                .mut => |mut| {
                    const path_dest = try self.placeOfPath(mut);
                    // TODO Can avoid this copy in most cases, but need to be careful about aliasing.
                    //try self.placeExpr(let.value, path_dest);
                    self.places[mut] = path_dest;
                    try self.placeExpr(let.value, null);
                },
                else => return self.fail("{} cannot appear on the left-hand side of an assignment (=).", .{path}),
            }
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
                    try self.placeExpr(call.args.values[0], null);
                    try self.placeExpr(call.args.values[1], null);
                },
                else => return self.fail("TODO Can't analyze {}", .{head.builtin}),
            }
            self.frame_offset = frame_offset_now;
        },
        .get => |get| {
            try self.placeExpr(get.object, null);
            // get.key is constant
            self.frame_offset = frame_offset_now;
        },
        .statements => |statements| {
            for (statements, 0..) |statement, ix| {
                try self.placeExpr(statement, if (ix == statements.len - 1) dest else null);
            }
            self.frame_offset = frame_offset_now;
        },
        else => return self.fail("TODO Can't analyze {}", .{expr}),
    }
}

fn placeOfPath(self: *Self, expr_id: ExprId) error{AnalyzeError}!Place {
    const expr = self.parser.exprs.items[expr_id];
    switch (expr) {
        .name => {
            const value_id = self.name_lets[expr_id].?;
            return self.places[value_id].?;
        },
        .get => |get| {
            const object_repr = self.reprs[get.object].?;
            const object_place = try self.placeOfPath(get.object);
            const key = self.constants[get.key].?;
            return object_repr.@"struct".placeOf(object_place, key).?;
        },
        else => return self.fail("{} is not valid in a path expression", .{expr}),
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
