const std = @import("std");
const panic = std.debug.panic;
const assert = std.debug.assert;
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;

const util = @import("./util.zig");
const oom = util.oom;
const HashMap = util.HashMap;
const Parser = @import("./Parser.zig");
const ExprId = Parser.ExprId;
const Expr = Parser.Expr;
const Repr = @import("./Semantalyzer.zig").Repr;
const StructRepr = @import("./Semantalyzer.zig").StructRepr;
const Value = @import("./Semantalyzer.zig").Value;

const Self = @This();
allocator: Allocator,
parser: Parser,

// Results
functions: ArrayList(Function),
functions_by_key: HashMap(FunctionKey, FunctionId),
stack_offset_max: usize,
error_message: ?[]const u8,

// Temporary state
function_id: FunctionId,
scope: ArrayList(Binding),

pub const FunctionId = usize;

pub const Place = struct {
    base: union(enum) {
        result,
        param: u32,
        shadow,
    },
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
        return switch (self.base) {
            .result => other.base == .result,
            .param => other.base == .param and self.base.param == other.base.param,
            .shadow => other.base == .shadow,
        } and
            self.offset == other.offset and self.length == other.length;
    }
};

pub const FunctionKey = struct {
    params: StructRepr,
    body: ExprId,

    pub fn update(self: FunctionKey, hasher: anytype) void {
        self.params.update(hasher);
        hasher.update(std.mem.asBytes(&self.body));
    }

    pub fn equal(self: FunctionKey, other: FunctionKey) bool {
        return self.body == other.body and self.params.order(other.params) == .eq;
    }
};

pub const Function = struct {
    // Meta
    name: []const u8,
    params: StructRepr,
    result: ?Repr, // =null when we haven't finished inference yet.
    locals_count: u32,
    frame_offset_max: usize,
    body: ExprId,

    // Body
    reprs: HashMap(ExprId, Repr),
    place_hints: HashMap(ExprId, Place),
    places: HashMap(ExprId, Place),
    constants: HashMap(ExprId, Value),
    bindings: HashMap(ExprId, Binding),
    calls: HashMap(ExprId, FunctionId),

    // Temporary state
    frame_offset: usize,

    fn init(allocator: Allocator, name: []const u8, params: StructRepr, body: ExprId) Function {
        return .{
            .name = name,
            .params = params,
            .result = null,
            .locals_count = 0,
            .frame_offset_max = 0,
            .body = body,

            .reprs = HashMap(ExprId, Repr).init(allocator),
            .place_hints = HashMap(ExprId, Place).init(allocator),
            .places = HashMap(ExprId, Place).init(allocator),
            .constants = HashMap(ExprId, Value).init(allocator),
            .bindings = HashMap(ExprId, Binding).init(allocator),
            .calls = HashMap(ExprId, FunctionId).init(allocator),

            .frame_offset = @sizeOf(u32), // Enough room for static link.
        };
    }
};

pub const Binding = struct {
    mut: bool,
    name: []const u8,
    source: union(enum) {
        let: struct {
            fn_id: FunctionId,
            value_id: ExprId,
        },
        param: usize,
    },
    repr: Repr,
};

pub fn init(allocator: Allocator, parser: Parser) Self {
    return .{
        .allocator = allocator,
        .parser = parser,
        .functions = ArrayList(Function).init(allocator),
        .functions_by_key = HashMap(FunctionKey, FunctionId).init(allocator),
        .stack_offset_max = 8 << 20, // 8mb
        .error_message = null,
        .function_id = 0,
        .scope = ArrayList(Binding).init(allocator),
    };
}

pub fn analyze(self: *Self) error{AnalyzeError}!void {
    const main_id = self.parser.exprs.items.len - 1;
    self.function_id = self.appendFunction(Function.init(
        self.allocator,
        "main",
        Repr.emptyStruct().@"struct",
        main_id,
    ));
    self.getFunction().result = try self.reprOfExpr(main_id, null);
    for (self.functions.items, 0..) |function, function_id| {
        self.function_id = function_id;
        _ = try self.placeOfExpr(function.body, null);
    }
}

fn appendFunction(self: *Self, function: Function) FunctionId {
    const id = self.functions.items.len;
    self.functions_by_key.put(
        .{ .params = function.params, .body = function.body },
        id,
    ) catch oom();
    self.functions.append(function) catch oom();
    return id;
}

fn reprOfExpr(self: *Self, expr_id: ExprId, repr_in: ?Repr) error{AnalyzeError}!Repr {
    const repr = try self.reprOfExprInner(expr_id, repr_in);
    if (repr_in != null and repr_in.?.order(repr) != .eq)
        return self.fail("Expected {}, found {}", .{ repr_in.?, repr });
    self.getFunction().reprs.put(expr_id, repr) catch oom();
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
            self.getFunction().bindings.put(expr_id, binding) catch oom();
            return binding.repr;
        },
        .let => |let| {
            const value_repr = try self.reprOfExpr(let.value, null);
            self.scope.append(.{
                .mut = let.mut,
                .name = let.name,
                .source = .{ .let = .{ .fn_id = self.function_id, .value_id = let.value } },
                .repr = value_repr,
            }) catch oom();
            return Repr.emptyStruct();
        },
        .set => |set| {
            try self.assertIsPath(set.path);
            const path_repr = try self.reprOfExpr(set.path, null);
            _ = try self.reprOfExpr(set.value, path_repr);
            return Repr.emptyStruct();
        },
        .@"fn" => {
            const name = switch (self.parser.exprs.items[self.parser.parents[expr_id].?]) {
                .let => |let| let.name,
                else => std.fmt.allocPrint(self.allocator, "anon#{}", .{self.functions.items.len}) catch oom(),
            };
            return .{ .@"fn" = .{
                .name = name,
                .expr_id = expr_id,
                .scope = self.allocator.dupe(Binding, self.scope.items) catch oom(),
            } };
        },
        .call => |call| {
            const head = self.parser.exprs.items[call.head];
            switch (head) {
                .builtin => |builtin| {
                    switch (builtin) {
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
                .name => |name| {
                    const binding = try self.lookup(name);
                    if (binding.repr != .@"fn") return self.fail("Can't call {}", .{binding.repr});
                    self.getFunction().bindings.put(expr_id, binding) catch oom();

                    const params = StructRepr{
                        .keys = self.allocator.alloc(Value, call.args.keys.len) catch oom(),
                        .reprs = self.allocator.alloc(Repr, call.args.values.len) catch oom(),
                    };
                    for (call.args.keys, params.keys) |key_id, *key|
                        key.* = try self.evalConstantKey(key_id);
                    for (call.args.values, params.reprs) |value_id, *repr|
                        repr.* = try self.reprOfExpr(value_id, null);

                    const fn_expr = self.parser.exprs.items[binding.repr.@"fn".expr_id].@"fn";
                    if (fn_expr.params.keys.len != params.keys.len)
                        return self.fail("Expected {} args, found {}", .{ fn_expr.params.keys.len, params.keys.len });
                    for (fn_expr.params.keys, params.keys) |fn_param_id, call_param| {
                        const fn_param = try self.evalConstantKey(fn_param_id); // TODO redundant evaluation - store in FnRepr instead?
                        if (!fn_param.equal(call_param))
                            return self.fail("Expected key {}, found key {}", .{ fn_param, call_param });
                    }

                    if (self.functions_by_key.get(.{ .params = params, .body = fn_expr.body })) |function_id| {
                        self.getFunction().calls.put(expr_id, function_id) catch oom();
                        return self.functions.items[function_id].result.?;
                    }

                    const new_function_id = self.appendFunction(Function.init(
                        self.allocator,
                        binding.repr.@"fn".name,
                        params,
                        fn_expr.body,
                    ));
                    self.getFunction().calls.put(expr_id, new_function_id) catch oom();

                    const old_function_id = self.function_id;
                    self.function_id = new_function_id;
                    defer self.function_id = old_function_id;

                    const old_scope = self.scope;
                    self.scope = ArrayList(Binding).init(self.allocator);
                    self.scope.appendSlice(binding.repr.@"fn".scope) catch oom();
                    defer self.scope = old_scope;

                    for (0.., fn_expr.params.values, params.reprs) |param_ix, param_id, param_repr| {
                        // TODO lift mut handling into parser?
                        const param_expr = self.parser.exprs.items[param_id];
                        const mut = param_expr == .mut;
                        const name_expr = if (mut) self.parser.exprs.items[param_expr.mut] else param_expr;
                        if (name_expr != .name) return self.fail("Can't use {} as pattern", .{name_expr});
                        self.scope.append(.{
                            .name = name_expr.name,
                            .mut = mut,
                            .source = .{ .param = param_ix },
                            .repr = param_repr,
                        }) catch oom();
                    }
                    const result = try self.reprOfExpr(fn_expr.body, null);
                    self.getFunction().result = result;
                    return result;
                },
                else => return self.fail("TODO Can't analyze {}", .{expr}),
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
    if (hint) |place_hint| self.getFunction().place_hints.put(expr_id, place_hint) catch oom();
    self.getFunction().places.put(expr_id, place) catch oom();
    return place;
}

fn placeOfExprInner(self: *Self, expr_id: ExprId, hint: ?Place) error{AnalyzeError}!Place {
    const expr = self.parser.exprs.items[expr_id];
    const repr = self.getFunction().reprs.get(expr_id).?;
    switch (expr) {
        .i64 => {
            return hint orelse self.framePush(repr);
        },
        .object => |object| {
            const place = hint orelse self.framePush(repr);
            const frame_offset_now = self.getFunction().frame_offset;
            defer self.getFunction().frame_offset = frame_offset_now;

            if (repr != .@"struct") return self.fail("TODO Can't analyze {}", .{expr});
            for (repr.@"struct".keys, object.values) |key, value_expr| {
                _ = try self.placeOfExpr(value_expr, repr.@"struct".placeOf(place, key).?);
            }

            return place;
        },
        .name => {
            const binding = self.getFunction().bindings.get(expr_id).?;
            return switch (binding.source) {
                // TODO for closed-over variables it matters which frame we're looking at!
                .let => |let| self.functions.items[let.fn_id].places.get(let.value_id).?,
                .param => |param| Place{
                    .base = .{ .param = @intCast(param) },
                    .offset = 0,
                    .length = @intCast(repr.sizeOf()),
                },
            };
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
        .@"fn" => {
            return Place.empty();
        },
        .call => |call| {
            const place = hint orelse self.framePush(repr);
            const frame_offset_now = self.getFunction().frame_offset;
            defer self.getFunction().frame_offset = frame_offset_now;

            for (call.args.values) |value| {
                _ = try self.placeOfExpr(value, null);
            }

            return place;
        },
        .get => |get| {
            const object_place = try self.placeOfExpr(get.object, null);
            const object_repr = self.getFunction().reprs.get(get.object).?;
            const key = self.getFunction().constants.get(get.key).?;
            return object_repr.@"struct".placeOf(object_place, key).?;
        },
        .statements => |statements| {
            const place = hint orelse self.framePush(repr);
            const frame_offset_now = self.getFunction().frame_offset;
            defer self.getFunction().frame_offset = frame_offset_now;

            for (statements, 0..) |statement, ix| {
                _ = try self.placeOfExpr(statement, if (ix == statements.len - 1) place else null);
            }

            return place;
        },
        else => return self.fail("TODO Can't analyze {}", .{expr}),
    }
}

fn framePush(self: *Self, repr: Repr) Place {
    const offset = self.getFunction().frame_offset;
    self.getFunction().frame_offset += repr.sizeOf();
    self.getFunction().frame_offset_max = @max(self.getFunction().frame_offset_max, self.getFunction().frame_offset);
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
    self.getFunction().constants.put(expr_id, value) catch oom();
    return value;
}

fn evalConstant(self: *Self, expr_id: ExprId) error{AnalyzeError}!Value {
    const expr = self.parser.exprs.items[expr_id];
    const value: Value = switch (expr) {
        .i64 => |num| .{ .i64 = num },
        else => return self.fail("Cannot const eval {}", .{expr}),
    };
    self.getFunction().constants.put(expr_id, value) catch oom();
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

fn getFunction(self: *Self) *Function {
    return &self.functions.items[self.function_id];
}

fn fail(self: *Self, comptime message: []const u8, args: anytype) error{AnalyzeError} {
    self.error_message = std.fmt.allocPrint(self.allocator, message, args) catch oom();
    return error.AnalyzeError;
}
