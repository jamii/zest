const std = @import("std");
const panic = std.debug.panic;
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;

const Parser = @import("./Parser.zig");
const ExprId = Parser.ExprId;
const Expr = Parser.Expr;

const Self = @This();
allocator: Allocator,
parser: Parser,
scope: Scope,
error_message: ?[]const u8,

pub const Scope = ArrayList(Binding);
pub const Binding = struct {
    mut: bool,
    name: []const u8,
    value: Value,
};

pub const Kind = enum {
    int64,
    float64,
    string,
    map,
    @"fn",
};

pub const Value = union(Kind) {
    int64: i64,
    float64: f64,
    string: []u8,
    map: Map,
    @"fn": Fn,

    pub fn kind(self: Value) Kind {
        return std.meta.activeTag(self);
    }

    pub fn hash(self: Value) u64 {
        var hasher = std.hash.Wyhash.init(42);
        self.update(&hasher);
        return hasher.final();
    }

    pub fn update(self: Value, hasher: anytype) void {
        switch (self) {
            .int64 => |int64| hasher.update(std.mem.asBytes(&int64)),
            // TODO NaN
            .float64 => |float64| hasher.update(std.mem.asBytes(&float64)),
            .string => |string| hasher.update(string),
            .map => |map| {
                // TODO Does seed/ordering matter?
                var iter = map.iterator();
                while (iter.next()) |entry| {
                    entry.key_ptr.update(hasher);
                    entry.value_ptr.update(hasher);
                }
            },
            .@"fn" => |@"fn"| {
                for (@"fn".captures.items) |binding| {
                    // For a given `@"fn".body`, the names in the bindings should always be the same.
                    binding.value.update(hasher);
                }
                hasher.update(std.mem.asBytes(&@"fn".body));
            },
        }
    }

    pub fn order(self: Value, other: Value) std.math.Order {
        switch (std.math.order(@intFromEnum(self.kind()), @intFromEnum(other.kind()))) {
            .lt => return .lt,
            .gt => return .gt,
            .eq => {},
        }
        switch (self) {
            .int64 => return std.math.order(self.int64, other.int64),
            // TODO NaN
            .float64 => return std.math.order(self.float64, other.float64),
            .string => return std.mem.order(u8, self.string, other.string),
            .map => {
                var self_entries = mapSortedEntries(self.map);
                defer self_entries.deinit();

                var other_entries = mapSortedEntries(other.map);
                defer other_entries.deinit();

                for (0..@min(self_entries.items.len, other_entries.items.len)) |i| {
                    const self_entry = self_entries.items[i];
                    const other_entry = other_entries.items[i];
                    switch (self_entry.key_ptr.order(other_entry.key_ptr.*)) {
                        .lt => return .lt,
                        .gt => return .gt,
                        .eq => {
                            switch (self_entry.value_ptr.order(other_entry.value_ptr.*)) {
                                .lt => return .lt,
                                .gt => return .gt,
                                .eq => {},
                            }
                        },
                    }
                }
                return std.math.order(self_entries.items.len, other_entries.items.len);
            },
            .@"fn" => {
                panic("TODO", .{});
            },
        }
    }

    pub fn equal(self: Value, other: Value) bool {
        if (self.kind() != other.kind()) return false;
        switch (self) {
            .int64 => return self.int64 == other.int64,
            // TODO NaN
            .float64 => return self.float64 == other.float64,
            .string => return std.mem.eql(u8, self.string, other.string),
            .map => {
                const self_map = self.map;
                const other_map = other.map;
                if (self_map.count() != other_map.count()) return false;
                var iter = self_map.iterator();
                while (iter.next()) |entry| {
                    const self_key = entry.key_ptr.*;
                    const other_key = other_map.get(self_key) orelse return false;
                    if (!self_key.equal(other_key)) return false;
                }
                return true;
            },
            .@"fn" => {
                const self_fn = self.@"fn";
                const other_fn = other.@"fn";
                if (self_fn.body != other_fn.body) return false;
                if (self_fn.captures.items.len != other_fn.captures.items.len) return false;
                for (self_fn.captures.items, other_fn.captures.items) |self_binding, other_binding| {
                    if (!self_binding.value.equal(other_binding.value)) return false;
                }
                return true;
            },
        }
    }

    pub fn format(self: Value, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = fmt;
        _ = options;
        switch (self) {
            .int64 => |int64| try writer.print("{}", .{int64}),
            .float64 => |float64| try writer.print("{d}", .{float64}),
            .string => |string| {
                try writer.writeByte('\'');
                for (string) |char| {
                    switch (char) {
                        '\n' => try writer.writeAll("\\n"),
                        '\'' => try writer.writeAll("\\'"),
                        '\\' => try writer.writeAll("\\\\"),
                        else => try writer.writeByte(char),
                    }
                }
                try writer.writeByte('\'');
            },
            .map => |map| {
                try writer.writeAll("[");

                var first = true;

                var ix: f64 = 0;
                while (true) : (ix += 1) {
                    if (map.get(.{ .float64 = ix })) |value| {
                        if (!first) try writer.writeAll(", ");
                        try writer.print("{}", .{value});
                        first = false;
                    } else {
                        break;
                    }
                }

                var entries = mapSortedEntries(map);
                defer entries.deinit();

                for (entries.items) |entry| {
                    const key = entry.key_ptr.*;
                    if (key == .float64 and
                        key.float64 == @trunc(key.float64) and
                        @trunc(key.float64) >= 0 and
                        @trunc(key.float64) < ix)
                        // Already printed this one.
                        continue;
                    if (!first) try writer.writeAll(", ");
                    try writer.print("{} = {}", .{ entry.key_ptr.*, entry.value_ptr.* });
                    first = false;
                }

                try writer.writeAll("]");
            },
            .@"fn" => |@"fn"| {
                // TODO print something parseable
                try writer.print("fn[{}", .{@"fn".body});
                for (@"fn".captures.items) |binding| {
                    try writer.print(", {s}={}", .{ binding.name, binding.value });
                }
                try writer.writeAll("]");
            },
        }
    }

    pub fn copy(self: Value, allocator: Allocator) Value {
        switch (self) {
            .int64 => |int64| return .{ .int64 = int64 },
            .float64 => |float64| return .{ .float64 = float64 },
            .string => |string| return .{ .string = allocator.dupe(u8, string) catch panic("OOM", .{}) },
            .map => |map| {
                var map_copy = map.cloneWithAllocator(allocator) catch panic("OOM", .{});
                var iter = map_copy.iterator();
                while (iter.next()) |entry| {
                    entry.key_ptr.copyInPlace(allocator);
                    entry.value_ptr.copyInPlace(allocator);
                }
                return .{ .map = map_copy };
            },
            .@"fn" => |@"fn"| {
                // TODO Wait for std.ArrayList.cloneWithAllocator to exist.
                var captures_copy = Scope.initCapacity(allocator, @"fn".captures.items.len) catch panic("OOM", .{});
                captures_copy.appendSliceAssumeCapacity(@"fn".captures.items);
                for (captures_copy.items) |*binding| {
                    binding.value.copyInPlace(allocator);
                }
                return .{ .@"fn" = .{
                    .captures = captures_copy,
                    .muts = @"fn".muts,
                    .params = @"fn".params,
                    .body = @"fn".body,
                } };
            },
        }
    }

    pub fn copyInPlace(self: *Value, allocator: Allocator) void {
        const value = self.copy(allocator);
        self.* = value;
    }
};

pub const Map = std.HashMap(
    Value,
    Value,
    struct {
        pub fn hash(_: @This(), key: Value) u64 {
            return key.hash();
        }
        pub fn eql(_: @This(), key1: Value, key2: Value) bool {
            return key1.equal(key2);
        }
    },
    std.hash_map.default_max_load_percentage,
);

// TODO This is stupid expensive. Cache it somewhere.
fn mapSortedEntries(map: Map) ArrayList(Map.Entry) {
    var entries = ArrayList(Map.Entry).initCapacity(map.allocator, map.count()) catch panic("OOM", .{});
    errdefer entries.deinit();

    var iter = map.iterator();
    while (iter.next()) |entry| {
        entries.appendAssumeCapacity(entry);
    }

    std.sort.heap(Map.Entry, entries.items, {}, (struct {
        fn lessThan(_: void, a: Map.Entry, b: Map.Entry) bool {
            return a.key_ptr.order(b.key_ptr.*) == .lt;
        }
    }).lessThan);

    return entries;
}

pub const Fn = struct {
    captures: Scope,
    muts: []bool,
    params: [][]const u8,
    body: ExprId,
};

pub fn init(allocator: Allocator, parser: Parser) Self {
    return .{
        .allocator = allocator,
        .parser = parser,
        .scope = Scope.init(allocator),
        .error_message = null,
    };
}

pub fn semantalyze(self: *Self) error{SemantalyzeError}!Value {
    return self.eval(self.parser.exprs.items.len - 1);
}

fn eval(self: *Self, expr_id: ExprId) error{SemantalyzeError}!Value {
    const expr = self.parser.exprs.items[expr_id];
    switch (expr) {
        .int64 => |int64| return .{ .int64 = int64 },
        .float64 => |float64| return .{ .float64 = float64 },
        .string => |string| return .{ .string = self.allocator.dupe(u8, string) catch panic("OOM", .{}) },
        .map => |map_expr| {
            var map = Map.init(self.allocator);
            for (map_expr.keys, map_expr.values) |key_id, value_id| {
                const key = try self.eval(key_id);
                const value = try self.eval(value_id);
                const prev = map.fetchPut(key, value) catch panic("OOM", .{});
                if (prev) |_| return self.fail("Duplicate key in map literal: {}", .{key});
            }
            return .{ .map = map };
        },
        .builtin => {
            panic("Direct eval of builtin should be unreachable", .{});
        },
        .name => |name| {
            const binding = try self.lookup(name);
            return binding.value;
        },
        .let => |let| {
            const value = try self.eval(let.value);
            self.scope.append(.{
                .mut = let.mut,
                .name = let.name,
                .value = value.copy(self.allocator),
            }) catch panic("OOM", .{});
            return .{ .float64 = 0 }; // TODO void/null or similar
        },
        .set => |set| {
            const value = try self.eval(set.value);
            try self.pathSet(set.path, value.copy(self.allocator));
            return .{ .float64 = 0 }; // TODO void/null or similar
        },
        .@"if" => |@"if"| {
            const cond = try self.boolish(try self.eval(@"if".cond));
            return self.eval(if (cond) @"if".if_true else @"if".if_false);
        },
        .@"while" => |@"while"| {
            while (true) {
                const cond = try self.boolish(try self.eval(@"while".cond));
                if (!cond) return .{ .float64 = 0 }; // TODO void/null or similar
                _ = try self.eval(@"while".body);
            }
        },
        .@"fn" => |@"fn"| {
            var captures = Scope.init(self.allocator);
            var locals = ArrayList([]const u8).initCapacity(self.allocator, @"fn".params.len) catch panic("OOM", .{});
            locals.appendSliceAssumeCapacity(@"fn".params);
            try self.capture(@"fn".body, &captures, &locals);
            return .{ .@"fn" = .{
                .captures = captures,
                .muts = @"fn".muts,
                .params = @"fn".params,
                .body = @"fn".body,
            } };
        },
        .call => |call| {
            const head_expr = self.parser.exprs.items[call.head];
            if (head_expr == .builtin) {
                if (call.args.len != 2)
                    return self.fail("Wrong float64 of arguments ({}) to {}", .{ call.args.len, head_expr });
                const value_a = try self.eval(call.args[0]);
                const value_b = try self.eval(call.args[1]);
                switch (head_expr.builtin) {
                    .equal => return if (value_a.equal(value_b)) .{ .float64 = 1 } else .{ .float64 = 0 },
                    .less_than, .less_than_or_equal, .more_than, .more_than_or_equal => panic("TODO", .{}),
                    .add, .subtract, .multiply, .divide => {
                        if (value_a != .float64) return self.fail("Cannot call {} on {}", .{ head_expr, value_a });
                        if (value_b != .float64) return self.fail("Cannot call {} on {}", .{ head_expr, value_b });
                        const result = switch (head_expr.builtin) {
                            .add => value_a.float64 + value_b.float64,
                            .subtract => value_a.float64 - value_b.float64,
                            .multiply => value_a.float64 * value_b.float64,
                            .divide => value_a.float64 / value_b.float64,
                            else => unreachable,
                        };
                        return .{ .float64 = result };
                    },
                }
            } else {
                const head = try self.eval(call.head);
                switch (head) {
                    .map => |map| {
                        if (call.args.len != 1)
                            return self.fail("Wrong float64 of arguments ({}) to {}", .{ call.args.len, head });
                        if (call.muts[0] == true)
                            return self.fail("Can't pass mut arg to map", .{});
                        const key = try self.eval(call.args[0]);
                        return (try self.mapGet(map, key)).*;
                    },
                    .@"fn" => |@"fn"| {
                        if (call.args.len != @"fn".params.len)
                            return self.fail("Wrong float64 of arguments ({}) to {}", .{ call.args.len, head });

                        for (call.muts, @"fn".muts) |arg_mut, param_mut| {
                            if (arg_mut != param_mut)
                                return self.fail("Expected {s} arg, found {s} arg", .{
                                    if (param_mut) "mut" else "const",
                                    if (arg_mut) "mut" else "const",
                                });
                        }

                        // Set up fn scope.
                        // TODO Wait for std.ArrayList.cloneWithAllocator to exist.
                        var fn_scope = Scope.initCapacity(self.allocator, @"fn".captures.items.len) catch panic("OOM", .{});
                        fn_scope.appendSliceAssumeCapacity(@"fn".captures.items);
                        for (fn_scope.items) |binding| {
                            // (Don't have to copy scope items because they can't be mutated.)
                            if (binding.mut) {
                                panic("Mut capture", .{});
                            }
                        }

                        // Add args to scope.
                        // TODO check all args are disjoint
                        for (@"fn".params, call.muts, call.args) |param, arg_mut, arg| {
                            const value = try self.eval(arg);
                            fn_scope.append(.{
                                .mut = arg_mut,
                                .name = param,
                                .value = value.copy(self.allocator),
                            }) catch panic("OOM", .{});
                        }

                        // Call fn.
                        std.mem.swap(Scope, &fn_scope, &self.scope);
                        const return_value = try self.eval(@"fn".body);

                        // Copy mut args back to original locations.
                        for (@"fn".params, call.muts, call.args) |param, arg_mut, arg| {
                            if (arg_mut) {
                                // Lookup param in fn_scope.
                                const binding = try self.lookup(param);
                                // Set in current scope.
                                std.mem.swap(Scope, &fn_scope, &self.scope);
                                try self.pathSet(arg, binding.value);
                                std.mem.swap(Scope, &fn_scope, &self.scope);
                            }
                        }

                        // Restore current scope.
                        std.mem.swap(Scope, &fn_scope, &self.scope);

                        return return_value;
                    },
                    else => return self.fail("Cannot call {}", .{head}),
                }
            }
        },
        .get_static => |get_static| {
            const value = try self.eval(get_static.map);
            if (value != .map) return self.fail("Cannot get key from non-map {}", .{value});
            const key = switch (get_static.key) {
                .int64 => |int64| Value{ .int64 = int64 },
                .string => |string| Value{ .string = self.allocator.dupe(u8, string) catch panic("OOM", .{}) },
            };
            return (try self.mapGet(value.map, key)).*;
        },
        .exprs => |exprs| {
            const scope_len = self.scope.items.len;
            var value = Value{ .float64 = 0 }; // TODO void/null or similar
            for (exprs) |subexpr| {
                value = try self.eval(subexpr);
            }
            self.scope.shrinkRetainingCapacity(scope_len);
            return value;
        },
    }
}

fn capture(self: *Self, expr_id: ExprId, captures: *Scope, locals: *ArrayList([]const u8)) error{SemantalyzeError}!void {
    const expr = self.parser.exprs.items[expr_id];
    switch (expr) {
        .int64, .float64, .string, .builtin => {},
        .map => |map| {
            for (map.keys) |key| try self.capture(key, captures, locals);
            for (map.values) |value| try self.capture(value, captures, locals);
        },
        .name => |name| {
            for (locals.items) |local| {
                if (std.mem.eql(u8, name, local)) {
                    return; // Not a capture - bound locally.
                }
            }
            for (captures.items) |binding| {
                if (std.mem.eql(u8, name, binding.name)) {
                    return; // Already captured.
                }
            }
            const binding = try self.lookup(name);
            captures.append(.{
                .mut = false, // TODO decide whether to allow mutable capture
                .name = name,
                .value = binding.value.copy(self.allocator),
            }) catch panic("OOM", .{});
        },
        .let => |let| {
            try self.capture(let.value, captures, locals);
            locals.append(let.name) catch panic("OOM", .{});
        },
        .set => |set| {
            try self.capture(set.path, captures, locals);
            try self.capture(set.value, captures, locals);
        },
        .@"if" => |@"if"| {
            try self.capture(@"if".cond, captures, locals);
            try self.capture(@"if".if_true, captures, locals);
            try self.capture(@"if".if_false, captures, locals);
        },
        .@"while" => |@"while"| {
            try self.capture(@"while".cond, captures, locals);
            try self.capture(@"while".body, captures, locals);
        },
        .@"fn" => |@"fn"| {
            const locals_len = locals.items.len;
            for (@"fn".params) |param| {
                locals.append(param) catch panic("OOM", .{});
            }
            try self.capture(@"fn".body, captures, locals);
            locals.shrinkRetainingCapacity(locals_len);
        },
        .call => |call| {
            try self.capture(call.head, captures, locals);
            for (call.args) |arg| {
                try self.capture(arg, captures, locals);
            }
        },
        .get_static => |get_static| {
            try self.capture(get_static.map, captures, locals);
        },
        .exprs => |exprs| {
            const locals_len = locals.items.len;
            for (exprs) |subexpr| {
                try self.capture(subexpr, captures, locals);
            }
            locals.shrinkRetainingCapacity(locals_len);
        },
    }
}

fn mapGet(self: *Self, map: Map, key: Value) error{SemantalyzeError}!*Value {
    if (map.getPtr(key)) |value| {
        return value;
    } else {
        return self.fail("Key {} not found in {}", .{ key, Value{ .map = map } });
    }
}

fn pathSet(self: *Self, expr_id: ExprId, value: Value) !void {
    const path = try self.evalPath(expr_id);
    path.* = value;
}

// This should only be called from `set` - too footgunny otherwise.
fn evalPath(self: *Self, expr_id: ExprId) error{SemantalyzeError}!*Value {
    const expr = self.parser.exprs.items[expr_id];
    switch (expr) {
        .name => |name| {
            const binding = try self.lookup(name);
            if (!binding.mut) return self.fail("Cannot set a non-mut variable: {}", .{std.zig.fmtEscapes(name)});
            return &binding.value;
        },
        .call => |call| {
            const head = try self.evalPath(call.head);
            switch (head.*) {
                .map => |*map| {
                    if (call.args.len != 1)
                        return self.fail("Wrong float64 of arguments ({}) to {}", .{ call.args.len, head });
                    if (call.muts[0] == true)
                        return self.fail("Can't pass mut arg to map", .{});
                    const key = try self.eval(call.args[0]);
                    return self.mapGet(map.*, key);
                },
                else => return self.fail("Cannot call {}", .{head}),
            }
        },
        .get_static => |get_static| {
            const value = try self.evalPath(get_static.map);
            if (value.* != .map) return self.fail("Cannot get key from non-map {}", .{value});
            const key = switch (get_static.key) {
                .int64 => |int64| Value{ .int64 = int64 },
                .string => |string| Value{ .string = self.allocator.dupe(u8, string) catch panic("OOM", .{}) },
            };
            return self.mapGet(value.map, key);
        },
        else => return self.fail("Not allowed in set/mut expr {}", .{expr}),
    }
}

fn boolish(self: *Self, value: Value) error{SemantalyzeError}!bool {
    if (value == .float64) {
        if (value.float64 == 1) return true;
        if (value.float64 == 0) return false;
    }
    return self.fail("Expected boolean (0 or 1). Found {}", .{value});
}

fn lookup(self: *Self, name: []const u8) error{SemantalyzeError}!*Binding {
    const bindings = self.scope.items;
    var i = bindings.len;
    while (i > 0) : (i -= 1) {
        const binding = &bindings[i - 1];
        if (std.mem.eql(u8, binding.name, name)) {
            return binding;
        }
    }
    return self.fail("Undefined variable: {s}", .{name});
}

fn fail(self: *Self, comptime message: []const u8, args: anytype) error{SemantalyzeError} {
    self.error_message = std.fmt.allocPrint(self.allocator, message, args) catch panic("OOM", .{});
    return error.SemantalyzeError;
}
