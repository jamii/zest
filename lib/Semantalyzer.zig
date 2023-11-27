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
    i64,
    f64,
    string,
    list,
    map,
    @"fn",
    repr,
    repr_kind,
};

pub const Value = union(Kind) {
    i64: i64,
    f64: f64,
    string: []u8,
    list: ArrayList(Value),
    map: Map,
    @"fn": Fn,
    repr: Repr,
    repr_kind: ReprKind,

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
            .i64 => |int| hasher.update(std.mem.asBytes(&int)),
            // TODO NaN
            .f64 => |float| hasher.update(std.mem.asBytes(&float)),
            .string => |string| hasher.update(string),
            .list => |list| {
                for (0.., list.items) |key, value| {
                    (Value{ .i64 = @intCast(key) }).update(hasher);
                    value.update(hasher);
                }
            },
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
            .repr => |repr| {
                repr.update(hasher);
            },
            .repr_kind => |repr_kind| {
                repr_kind.update(hasher);
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
            .i64 => return std.math.order(self.i64, other.i64),
            // TODO NaN
            .f64 => return std.math.order(self.f64, other.f64),
            .string => return std.mem.order(u8, self.string, other.string),
            .list => {
                const self_items = self.list.items;
                const other_items = other.list.items;
                for (0..@min(self_items.len, other_items.len)) |i| {
                    const self_elem = self_items[i];
                    const other_elem = other_items[i];
                    switch (self_elem.order(other_elem)) {
                        .lt => return .lt,
                        .gt => return .gt,
                        .eq => {},
                    }
                }
                return std.math.order(self_items.len, other_items.len);
            },
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
                        .eq => {},
                    }
                    switch (self_entry.value_ptr.order(other_entry.value_ptr.*)) {
                        .lt => return .lt,
                        .gt => return .gt,
                        .eq => {},
                    }
                }
                return std.math.order(self_entries.items.len, other_entries.items.len);
            },
            .@"fn" => {
                panic("TODO", .{});
            },
            .repr => {
                return self.repr.order(other.repr);
            },
            .repr_kind => {
                return self.repr_kind.order(other.repr_kind);
            },
        }
    }

    pub fn equal(self: Value, other: Value) bool {
        if (self.kind() != other.kind()) return false;
        switch (self) {
            .i64 => return self.i64 == other.i64,
            // TODO NaN
            .f64 => return self.f64 == other.f64,
            .string => return std.mem.eql(u8, self.string, other.string),
            .list => {
                const self_list = self.list;
                const other_list = other.list;
                if (self_list.items.len != other_list.items.len) return false;
                for (self_list.items, other_list.items) |self_elem, other_elem| {
                    if (!self_elem.equal(other_elem)) return false;
                }
                return true;
            },
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
            .repr => return self.repr.order(other.repr) == .eq,
            .repr_kind => return self.repr_kind.order(other.repr_kind) == .eq,
        }
    }

    pub fn format(self: Value, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        switch (self) {
            .i64 => |int| try writer.print("{}", .{int}),
            .f64 => |float| {
                if (float == @trunc(float)) {
                    // Always print .0 so we know it's a float.
                    try writer.print("{d:.1}", .{float});
                } else {
                    try writer.print("{d}", .{float});
                }
            },
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
            .list => |list| {
                try writer.writeAll("[");

                var first = true;

                for (list.items) |elem| {
                    if (!first) try writer.writeAll(", ");
                    try writer.print("{}", .{elem});
                    first = false;
                }

                try writer.writeAll("]");
            },
            .map => |map| {
                try writer.writeAll("[");

                var first = true;

                var ix: i64 = 0;
                while (true) : (ix += 1) {
                    if (map.get(.{ .i64 = ix })) |value| {
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
                    if (key == .i64 and
                        key.i64 >= 0 and
                        key.i64 < ix)
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
            .repr => |repr| {
                try repr.format(fmt, options, writer);
            },
            .repr_kind => |repr_kind| {
                try repr_kind.format(fmt, options, writer);
            },
        }
    }

    pub fn copy(self: Value, allocator: Allocator) Value {
        switch (self) {
            .i64 => |int| return .{ .i64 = int },
            .f64 => |float| return .{ .f64 = float },
            .string => |string| return .{ .string = allocator.dupe(u8, string) catch panic("OOM", .{}) },
            .list => |list| {
                var list_copy = list.clone() catch panic("OOM", .{});
                for (list_copy.items) |*elem| {
                    elem.copyInPlace(allocator);
                }
                return .{ .list = list_copy };
            },
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
            // repr are always immutable
            .repr, .repr_kind => return self,
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

pub const Repr = union(enum) {
    i64,
    f64,
    string,
    list: *Repr,
    map: [2]*Repr,

    pub fn update(self: Repr, hasher: anytype) void {
        hasher.update(&[1]u8{@intFromEnum(std.meta.activeTag(self))});
        switch (self) {
            .i64, .f64, .string => {},
            .list => |elem| {
                elem.update(hasher);
            },
            .map => |key_value| {
                key_value[0].update(hasher);
                key_value[1].update(hasher);
            },
        }
    }

    pub fn order(self: Repr, other: Repr) std.math.Order {
        switch (std.math.order(@intFromEnum(std.meta.activeTag(self)), @intFromEnum(std.meta.activeTag(other)))) {
            .lt => return .lt,
            .gt => return .gt,
            .eq => {},
        }
        switch (self) {
            .i64, .f64, .string => return .eq,
            .list => return self.list.order(other.list.*),
            .map => {
                switch (self.map[0].order(other.map[0].*)) {
                    .lt => return .lt,
                    .gt => return .gt,
                    .eq => {},
                }
                switch (self.map[1].order(other.map[1].*)) {
                    .lt => return .lt,
                    .gt => return .gt,
                    .eq => {},
                }
                return .eq;
            },
        }
    }

    pub fn format(self: Repr, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = fmt;
        _ = options;
        try writer.writeAll(@tagName(self));
        switch (self) {
            .i64, .f64, .string => {},
            .list => |elem| try writer.print("[{}]", .{elem.*}),
            .map => |key_value| try writer.print("[{}, {}]", .{ key_value[0].*, key_value[1].* }),
        }
    }
};

pub const ReprKind = enum {
    list,
    map,

    pub fn update(self: ReprKind, hasher: anytype) void {
        hasher.update(&[1]u8{@intFromEnum(self)});
    }

    pub fn order(self: ReprKind, other: ReprKind) std.math.Order {
        return std.math.order(@intFromEnum(self), @intFromEnum(other));
    }

    pub fn format(self: ReprKind, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = fmt;
        _ = options;
        try writer.writeAll(@tagName(self));
    }
};

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
        .i64 => |int| return .{ .i64 = int },
        .f64 => |float| return .{ .f64 = float },
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
            if (self.lookup(name)) |binding| {
                return binding.value;
            } else |err| {
                if (std.mem.eql(u8, name, "i64")) {
                    return .{ .repr = .i64 };
                }
                if (std.mem.eql(u8, name, "f64")) {
                    return .{ .repr = .f64 };
                }
                if (std.mem.eql(u8, name, "string")) {
                    return .{ .repr = .string };
                }
                if (std.mem.eql(u8, name, "list")) {
                    return .{ .repr_kind = .list };
                }
                return err;
            }
        },
        .let => |let| {
            const value = try self.eval(let.value);
            self.scope.append(.{
                .mut = let.mut,
                .name = let.name,
                .value = value.copy(self.allocator),
            }) catch panic("OOM", .{});
            return .{ .f64 = 0 }; // TODO void/null or similar
        },
        .set => |set| {
            const value = try self.eval(set.value);
            try self.pathSet(set.path, value.copy(self.allocator));
            return .{ .f64 = 0 }; // TODO void/null or similar
        },
        .@"if" => |@"if"| {
            const cond = try self.boolish(try self.eval(@"if".cond));
            return self.eval(if (cond) @"if".if_true else @"if".if_false);
        },
        .@"while" => |@"while"| {
            while (true) {
                const cond = try self.boolish(try self.eval(@"while".cond));
                if (!cond) return .{ .f64 = 0 }; // TODO void/null or similar
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
                    return self.fail("Wrong number of arguments ({}) to {}", .{ call.args.len, head_expr });
                const value_a = try self.eval(call.args[0]);
                const value_b = try self.eval(call.args[1]);
                switch (head_expr.builtin) {
                    .equal => return if (value_a.equal(value_b)) .{ .f64 = 1 } else .{ .f64 = 0 },
                    .less_than, .less_than_or_equal, .more_than, .more_than_or_equal => panic("TODO", .{}),
                    .add, .subtract, .multiply, .divide => {
                        if (value_a != .f64) return self.fail("Cannot call {} on {}", .{ head_expr, value_a });
                        if (value_b != .f64) return self.fail("Cannot call {} on {}", .{ head_expr, value_b });
                        const result = switch (head_expr.builtin) {
                            .add => value_a.f64 + value_b.f64,
                            .subtract => value_a.f64 - value_b.f64,
                            .multiply => value_a.f64 * value_b.f64,
                            .divide => value_a.f64 / value_b.f64,
                            else => unreachable,
                        };
                        return .{ .f64 = result };
                    },
                }
            } else {
                const head = try self.eval(call.head);
                switch (head) {
                    .map => |map| {
                        if (call.args.len != 1)
                            return self.fail("Wrong number of arguments ({}) to {}", .{ call.args.len, head });
                        if (call.muts[0] == true)
                            return self.fail("Can't pass mut arg to map", .{});
                        const key = try self.eval(call.args[0]);
                        return (try self.mapGet(map, key)).*;
                    },
                    .@"fn" => |@"fn"| {
                        if (call.args.len != @"fn".params.len)
                            return self.fail("Wrong number of arguments ({}) to {}", .{ call.args.len, head });

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
                    .repr => |repr| {
                        if (call.args.len != 1)
                            return self.fail("Wrong number of arguments ({}) to {}", .{ call.args.len, head });
                        if (call.muts[0] == true)
                            return self.fail("Can't pass mut arg to repr", .{});
                        const value = try self.eval(call.args[0]);
                        return self.convert(repr, value);
                    },
                    .repr_kind => |repr_kind| {
                        switch (repr_kind) {
                            .list => {
                                if (call.args.len != 1)
                                    return self.fail("Wrong number of arguments ({}) to {}", .{ call.args.len, head });
                                if (call.muts[0] == true)
                                    return self.fail("Can't pass mut arg to repr", .{});
                                const elem = try self.eval(call.args[0]);
                                if (elem != .repr)
                                    return self.fail("Can't pass {} to {}", .{ elem, head });
                                return .{ .repr = .{ .list = box(self.allocator, elem.repr) } };
                            },
                            else => return self.fail("TODO", .{}),
                        }
                    },
                    else => return self.fail("Cannot call {}", .{head}),
                }
            }
        },
        .get_static => |get_static| {
            const value = try self.eval(get_static.map);
            if (value != .map) return self.fail("Cannot get key from non-map {}", .{value});
            const key = switch (get_static.key) {
                .i64 => |int| Value{ .i64 = int },
                .string => |string| Value{ .string = self.allocator.dupe(u8, string) catch panic("OOM", .{}) },
            };
            return (try self.mapGet(value.map, key)).*;
        },
        .exprs => |exprs| {
            const scope_len = self.scope.items.len;
            var value = Value{ .f64 = 0 }; // TODO void/null or similar
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
        .i64, .f64, .string, .builtin => {},
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
                        return self.fail("Wrong number of arguments ({}) to {}", .{ call.args.len, head });
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
                .i64 => |int| Value{ .i64 = int },
                .string => |string| Value{ .string = self.allocator.dupe(u8, string) catch panic("OOM", .{}) },
            };
            return self.mapGet(value.map, key);
        },
        else => return self.fail("Not allowed in set/mut expr {}", .{expr}),
    }
}

fn boolish(self: *Self, value: Value) error{SemantalyzeError}!bool {
    if (value == .f64) {
        if (value.f64 == 1) return true;
        if (value.f64 == 0) return false;
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

fn convert(self: *Self, repr: Repr, value: Value) error{SemantalyzeError}!Value {
    switch (repr) {
        .i64 => {
            switch (value) {
                .i64 => return value.copy(self.allocator),
                .f64 => |float| {
                    if (float == @trunc(float)) {
                        return .{ .i64 = @intFromFloat(float) };
                    } else {
                        return self.fail("Cannot convert {} to {}", .{ value, repr });
                    }
                },
                else => return self.fail("Cannot convert {} to {}", .{ value, repr }),
            }
        },
        .f64 => {
            switch (value) {
                .f64 => return value.copy(self.allocator),
                .i64 => |int| {
                    const float = std.math.lossyCast(f64, int);
                    if (int == @as(i64, @intFromFloat(float))) {
                        return .{ .f64 = float };
                    } else {
                        return self.fail("Cannot convert {} to {}", .{ value, repr });
                    }
                },
                else => return self.fail("Cannot convert {} to {}", .{ value, repr }),
            }
        },
        .string => {
            switch (value) {
                .string => return value.copy(self.allocator),
                else => return self.fail("Cannot convert {} to {}", .{ value, repr }),
            }
        },
        .list => |elem_repr| {
            switch (value) {
                .list => |list| {
                    var ix: i64 = 0;
                    var elems = ArrayList(Value).init(self.allocator);
                    for (list.items) |elem| {
                        elems.append(try self.convert(elem_repr.*, elem)) catch panic("OOM", .{});
                        ix += 1;
                    }
                    return .{ .list = elems };
                },
                .map => |map| {
                    var ix: i64 = 0;
                    var elems = ArrayList(Value).init(self.allocator);
                    while (map.get(.{ .i64 = ix })) |elem| {
                        elems.append(try self.convert(elem_repr.*, elem)) catch panic("OOM", .{});
                        ix += 1;
                    }
                    if (map.count() != ix)
                        return self.fail("Cannot convert {} to {}", .{ value, repr });
                    return .{ .list = elems };
                },
                else => return self.fail("Cannot convert {} to {}", .{ value, repr }),
            }
        },
        else => return self.fail("TODO", .{}),
    }
}

fn fail(self: *Self, comptime message: []const u8, args: anytype) error{SemantalyzeError} {
    self.error_message = std.fmt.allocPrint(self.allocator, message, args) catch panic("OOM", .{});
    return error.SemantalyzeError;
}

fn box(allocator: Allocator, value: anytype) *@TypeOf(value) {
    const value_ptr = allocator.create(@TypeOf(value)) catch panic("OOM", .{});
    value_ptr.* = value;
    return value_ptr;
}
