const std = @import("std");
const panic = std.debug.panic;
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;

const Parser = @import("./Parser.zig");
const ExprId = Parser.ExprId;
const Expr = Parser.Expr;
const ObjectExpr = Parser.ObjectExpr;

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

pub const Value = union(enum) {
    i64: i64,
    f64: f64,
    string: []u8,
    @"struct": Struct,
    list: List,
    map: Map,
    @"union": Union,
    any: *Value,
    only: Only,
    @"fn": Fn,
    repr: Repr,
    repr_kind: ReprKind,

    pub fn hash(self: Value) u64 {
        var hasher = std.hash.Wyhash.init(42);
        self.update(&hasher);
        return hasher.final();
    }

    pub fn update(self: Value, hasher: anytype) void {
        // TODO Should we hash reprs?
        hasher.update(&[1]u8{@intFromEnum(std.meta.activeTag(self))});
        switch (self) {
            .i64 => |int| hasher.update(std.mem.asBytes(&int)),
            // TODO NaN
            .f64 => |float| hasher.update(std.mem.asBytes(&float)),
            .string => |string| hasher.update(string),
            .@"struct" => |@"struct"| {
                for (@"struct".repr.keys, @"struct".values) |key, value| {
                    key.update(hasher);
                    value.update(hasher);
                }
            },
            .list => |list| {
                list.repr.update(hasher);
                for (0.., list.elems.items) |key, value| {
                    (Value{ .i64 = @intCast(key) }).update(hasher);
                    value.update(hasher);
                }
            },
            .map => |map| {
                const entries = mapSortedEntries(map);
                defer entries.deinit();

                for (entries.items) |entry| {
                    entry.key_ptr.update(hasher);
                    entry.value_ptr.update(hasher);
                }
            },
            .@"union" => |@"union"| {
                @"union".value.update(hasher);
            },
            .any => |value| {
                value.update(hasher);
            },
            .only => |only| {
                only.repr.update(hasher);
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
        switch (self.reprOf().order(other.reprOf())) {
            .lt => return .lt,
            .gt => return .gt,
            .eq => {},
        }
        switch (self) {
            .i64 => return std.math.order(self.i64, other.i64),
            // TODO NaN
            .f64 => return std.math.order(self.f64, other.f64),
            .string => return std.mem.order(u8, self.string, other.string),
            .@"struct" => {
                const self_struct = self.@"struct";
                const other_struct = other.@"struct";
                for (0..@min(self_struct.repr.keys.len, other_struct.repr.keys.len)) |i| {
                    switch (self_struct.repr.keys[i].order(other_struct.repr.keys[i])) {
                        .lt => return .lt,
                        .gt => return .gt,
                        .eq => {},
                    }
                    switch (self_struct.values[i].order(other_struct.values[i])) {
                        .lt => return .lt,
                        .gt => return .gt,
                        .eq => {},
                    }
                }
                return .eq;
            },
            .list => {
                const self_items = self.list.elems.items;
                const other_items = other.list.elems.items;
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
                const self_entries = mapSortedEntries(self.map);
                defer self_entries.deinit();

                const other_entries = mapSortedEntries(other.map);
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
            .@"union" => {
                const self_member = self.@"union".repr[self.@"union".tag];
                const other_member = other.@"union".repr[other.@"union".tag];
                switch (self_member.order(other_member)) {
                    .lt => return .lt,
                    .gt => return .gt,
                    .eq => {},
                }
                return self.@"union".value.order(other.@"union".value.*);
            },
            .any => {
                return self.any.order(other.any.*);
            },
            .only => {
                return .eq;
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
        if (self.reprOf().order(other.reprOf()) != .eq) return false;
        switch (self) {
            .i64 => return self.i64 == other.i64,
            // TODO NaN
            .f64 => return self.f64 == other.f64,
            .string => return std.mem.eql(u8, self.string, other.string),
            .@"struct" => {
                const self_struct = self.@"struct";
                const other_struct = other.@"struct";
                if (self_struct.repr.keys.len != other_struct.repr.keys.len) return false;
                for (self_struct.repr.keys, other_struct.repr.keys) |self_key, other_key| {
                    if (!self_key.equal(other_key)) return false;
                }
                for (self_struct.values, other_struct.values) |self_value, other_value| {
                    if (!self_value.equal(other_value)) return false;
                }
                return true;
            },
            .list => {
                const self_list = self.list;
                const other_list = other.list;
                if (self_list.elems.items.len != other_list.elems.items.len) return false;
                for (self_list.elems.items, other_list.elems.items) |self_elem, other_elem| {
                    if (!self_elem.equal(other_elem)) return false;
                }
                return true;
            },
            .map => {
                const self_map = self.map;
                const other_map = other.map;
                if (self_map.entries.count() != other_map.entries.count()) return false;
                var iter = self_map.entries.iterator();
                while (iter.next()) |entry| {
                    const self_key = entry.key_ptr.*;
                    const other_key = other_map.entries.get(self_key) orelse return false;
                    if (!self_key.equal(other_key)) return false;
                }
                return true;
            },
            .@"union" => {
                return self.@"union".value.equal(other.@"union".value.*);
            },
            .any => {
                return self.any.equal(other.any.*);
            },
            .only => {
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

    const FormatKey = struct {
        key: Value,

        pub fn format(self: FormatKey, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
            _ = fmt;
            _ = options;
            if (self.key == .string and isName(self.key.string)) {
                try writer.print("{s}", .{self.key.string});
            } else {
                try writer.print("{}", .{self.key});
            }
        }
    };

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
            .@"struct" => |@"struct"| {
                try writer.writeAll("[");
                var first = true;
                var positional = true;
                for (0.., @"struct".repr.keys, @"struct".values) |i, key, value| {
                    if (!first) try writer.writeAll(", ");
                    if (!positional or key != .i64 or key.i64 != i) {
                        try writer.print("{}: ", .{FormatKey{ .key = key }});
                        positional = false;
                    }
                    try writer.print("{}", .{value});
                    first = false;
                }
                try writer.writeAll("]");
            },
            .list => |list| {
                try writer.print("{}[", .{Repr{ .list = list.repr }});

                try writer.writeAll("[");
                var first = true;
                for (list.elems.items) |elem| {
                    if (!first) try writer.writeAll(", ");
                    try writer.print("{}", .{elem});
                    first = false;
                }
                try writer.writeAll("]");

                try writer.writeAll("]");
            },
            .map => |map| {
                try writer.print("{}[", .{Repr{ .map = map.repr }});

                try writer.writeAll("[");

                const entries = mapSortedEntries(map);
                defer entries.deinit();

                var first = true;
                for (entries.items) |entry| {
                    if (!first) try writer.writeAll(", ");
                    try writer.print("{}: {}", .{ FormatKey{ .key = entry.key_ptr.* }, entry.value_ptr.* });
                    first = false;
                }

                try writer.writeAll("]");

                try writer.writeAll("]");
            },
            .@"union" => |@"union"| {
                try writer.print("{}[{}]", .{ Repr{ .@"union" = @"union".repr }, @"union".value.* });
            },
            .any => |value| {
                try writer.print("{}[{}]", .{ Repr{ .any = {} }, value.* });
            },
            .only => |only| {
                try writer.print("{}[]", .{Repr{ .only = only.repr }});
            },
            .@"fn" => |@"fn"| {
                // TODO print something parseable
                try writer.print("fn[{}", .{@"fn".body});
                for (@"fn".captures.items) |binding| {
                    try writer.print(", {s}: {}", .{ binding.name, binding.value });
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
            .@"struct" => |@"struct"| {
                var values_copy = allocator.dupe(Value, @"struct".values) catch panic("OOM", .{});
                for (values_copy) |*value| {
                    value.copyInPlace(allocator);
                }
                return .{ .@"struct" = .{
                    .repr = @"struct".repr,
                    .values = values_copy,
                } };
            },
            .list => |list| {
                var elems_copy = list.elems.clone() catch panic("OOM", .{});
                for (elems_copy.items) |*elem| {
                    elem.copyInPlace(allocator);
                }
                return .{ .list = .{
                    .repr = list.repr,
                    .elems = elems_copy,
                } };
            },
            .map => |map| {
                var entries_copy = map.entries.cloneWithAllocator(allocator) catch panic("OOM", .{});
                var iter = entries_copy.iterator();
                while (iter.next()) |entry| {
                    entry.key_ptr.copyInPlace(allocator);
                    entry.value_ptr.copyInPlace(allocator);
                }
                return .{ .map = .{ .repr = map.repr, .entries = entries_copy } };
            },
            .any => |value| {
                return .{ .any = box(allocator, value.copy(allocator)) };
            },
            .only => |only| return .{ .only = only },
            .@"union" => |@"union"| {
                const value_copy = @"union".value.copy(allocator);
                return .{ .@"union" = .{ .repr = @"union".repr, .tag = @"union".tag, .value = box(allocator, value_copy) } };
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

    pub fn reprOf(self: Value) Repr {
        switch (self) {
            .i64 => return .i64,
            .f64 => return .f64,
            .string => return .string,
            .@"struct" => |@"struct"| return .{ .@"struct" = @"struct".repr },
            .list => |list| return .{ .list = list.repr },
            .map => |map| return .{ .map = map.repr },
            .@"union" => |@"union"| return .{ .@"union" = @"union".repr },
            .any => return .any,
            .only => |only| return .{ .only = only.repr },
            .@"fn" => return .i64, // TODO,
            .repr => return .repr,
            .repr_kind => return .i64, // TODO,
        }
    }
};

pub const Struct = struct {
    repr: StructRepr,
    values: []Value,
};

pub const List = struct {
    repr: ListRepr,
    elems: ArrayList(Value),
};

pub const Map = struct {
    repr: MapRepr,
    entries: ValueHashMap,
};

pub const ValueHashMap = std.HashMap(
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

pub const Union = struct {
    repr: UnionRepr,
    tag: usize,
    value: *Value,
};

pub const Only = struct {
    repr: OnlyRepr,
};

pub const Repr = union(enum) {
    i64,
    f64,
    string,
    @"struct": StructRepr,
    list: ListRepr,
    map: MapRepr,
    any,
    only: OnlyRepr,
    @"union": UnionRepr,
    repr,

    pub fn update(self: Repr, hasher: anytype) void {
        hasher.update(&[1]u8{@intFromEnum(std.meta.activeTag(self))});
        switch (self) {
            .i64, .f64, .string, .any, .repr => {},
            .@"struct" => |@"struct"| {
                for (@"struct".keys, @"struct".reprs) |key, repr| {
                    key.update(hasher);
                    repr.update(hasher);
                }
            },
            .list => |elem| {
                elem.update(hasher);
            },
            .map => |key_value| {
                key_value[0].update(hasher);
                key_value[1].update(hasher);
            },
            .@"union" => |members| {
                for (members) |member| {
                    member.update(hasher);
                }
            },
            .only => |value| {
                value.update(hasher);
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
            .i64, .f64, .string, .any, .repr => return .eq,
            .@"struct" => {
                const self_struct = self.@"struct";
                const other_struct = other.@"struct";
                for (0..@min(self_struct.keys.len, other_struct.keys.len)) |i| {
                    switch (self_struct.keys[i].order(other_struct.keys[i])) {
                        .lt => return .lt,
                        .gt => return .gt,
                        .eq => {},
                    }
                    switch (self_struct.reprs[i].order(other_struct.reprs[i])) {
                        .lt => return .lt,
                        .gt => return .gt,
                        .eq => {},
                    }
                }
                return .eq;
            },
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
            .@"union" => {
                const self_members = self.@"union";
                const other_members = other.@"union";
                for (0..@min(self_members.len, other_members.len)) |i| {
                    switch (self_members[i].order(other_members[i])) {
                        .lt => return .lt,
                        .gt => return .gt,
                        .eq => {},
                    }
                }
                return .eq;
            },
            .only => {
                return self.only.order(other.only.*);
            },
        }
    }

    pub fn format(self: Repr, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = fmt;
        _ = options;
        try writer.writeAll(@tagName(self));
        switch (self) {
            .i64, .f64, .string, .any, .repr => {},
            .@"struct" => |@"struct"| {
                try writer.writeAll("[");
                var first = true;
                for (0.., @"struct".keys, @"struct".reprs) |i, key, repr| {
                    if (!first) try writer.writeAll(", ");
                    if (key != .i64 or key.i64 != i)
                        try writer.print("{}: ", .{key});
                    try writer.print("{}", .{repr});
                    first = false;
                }
                try writer.writeAll("]");
            },
            .list => |elem| try writer.print("[{}]", .{elem.*}),
            .map => |key_value| try writer.print("[{}, {}]", .{ key_value[0].*, key_value[1].* }),
            .@"union" => |members| {
                try writer.writeAll("[");
                var first = true;
                for (members) |member| {
                    if (!first) try writer.writeAll(", ");
                    try writer.print("{}", .{member});
                    first = false;
                }
                try writer.writeAll("]");
            },
            .only => |value| {
                try writer.print("[{}]", .{value});
            },
        }
    }
};

pub const StructRepr = struct {
    keys: []Value,
    reprs: []Repr,
};

pub const ListRepr = *Repr;
pub const MapRepr = [2]*Repr;
pub const UnionRepr = []Repr;
pub const OnlyRepr = *Value;

pub const ReprKind = enum {
    @"struct",
    list,
    map,
    @"union",
    only,

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
fn mapSortedEntries(map: Map) ArrayList(ValueHashMap.Entry) {
    var entries = ArrayList(ValueHashMap.Entry).initCapacity(map.entries.allocator, map.entries.count()) catch panic("OOM", .{});
    errdefer entries.deinit();

    var iter = map.entries.iterator();
    while (iter.next()) |entry| {
        entries.appendAssumeCapacity(entry);
    }

    std.sort.heap(ValueHashMap.Entry, entries.items, {}, (struct {
        fn lessThan(_: void, a: ValueHashMap.Entry, b: ValueHashMap.Entry) bool {
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
        .object => |object_expr| {
            return .{ .@"struct" = try self.evalObject(object_expr) };
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
                if (std.mem.eql(u8, name, "struct")) {
                    return .{ .repr_kind = .@"struct" };
                }
                if (std.mem.eql(u8, name, "list")) {
                    return .{ .repr_kind = .list };
                }
                if (std.mem.eql(u8, name, "map")) {
                    return .{ .repr_kind = .map };
                }
                if (std.mem.eql(u8, name, "union")) {
                    return .{ .repr_kind = .@"union" };
                }
                if (std.mem.eql(u8, name, "any")) {
                    return .{ .repr = .any };
                }
                if (std.mem.eql(u8, name, "only")) {
                    return .{ .repr_kind = .only };
                }
                return err;
            }
        },
        .let => |let| {
            const value = try self.eval(let.value);
            if (self.lookup(let.name)) |_| {
                return self.fail("Name {s} shadows earlier definition", .{let.name});
            } else |_| {
                self.scope.append(.{
                    .mut = let.mut,
                    .name = let.name,
                    .value = value.copy(self.allocator),
                }) catch panic("OOM", .{});
                return fromBool(false); // TODO void/null or similar
            }
        },
        .set => |set| {
            const value = try self.eval(set.value);
            try self.pathSet(set.path, value.copy(self.allocator));
            return fromBool(false); // TODO void/null or similar
        },
        .@"if" => |@"if"| {
            const cond = try self.toBool(try self.eval(@"if".cond));
            return self.eval(if (cond) @"if".if_true else @"if".if_false);
        },
        .@"while" => |@"while"| {
            while (true) {
                const cond = try self.toBool(try self.eval(@"while".cond));
                if (!cond) return fromBool(false); // TODO void/null or similar
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
        .make => |make| {
            const head = try self.eval(make.head);
            const args = try self.evalObject(make.args);
            switch (head) {
                .repr => |repr| {
                    switch (repr) {
                        .@"struct" => {
                            return self.convert(repr, .{ .@"struct" = args });
                        },
                        else => {
                            if (repr == .only and args.values.len == 0) {
                                return .{ .only = .{ .repr = repr.only } };
                            }
                            if (args.values.len != 1)
                                return self.fail("Wrong number of arguments ({}) to {}", .{ args.values.len, head });
                            if (make.args.muts[0] == true)
                                return self.fail("Can't pass mut arg to repr", .{});
                            return self.convert(repr, args.values[0]);
                        },
                    }
                },
                .repr_kind => |repr_kind| {
                    switch (repr_kind) {
                        .@"struct" => {
                            const reprs = self.allocator.alloc(Repr, args.values.len) catch panic("OOM", .{});
                            for (reprs, args.values) |*repr, value| {
                                if (value != .repr)
                                    return self.fail("Can't pass {} to {}", .{ value, head });
                                repr.* = value.repr;
                            }
                            return .{ .repr = .{ .@"struct" = .{
                                .keys = args.repr.keys,
                                .reprs = reprs,
                            } } };
                        },
                        .list => {
                            if (args.values.len != 1)
                                return self.fail("Wrong number of arguments ({}) to {}", .{ args.values.len, head });
                            if (make.args.muts[0] == true)
                                return self.fail("Can't pass mut arg to repr", .{});
                            const elem = args.values[0];
                            if (elem != .repr)
                                return self.fail("Can't pass {} to {}", .{ elem, head });
                            return .{ .repr = .{ .list = box(self.allocator, elem.repr) } };
                        },
                        .map => {
                            if (args.values.len != 2)
                                return self.fail("Wrong number of arguments ({}) to {}", .{ args.values.len, head });
                            if (make.args.muts[0] == true)
                                return self.fail("Can't pass mut arg to repr", .{});
                            if (make.args.muts[1] == true)
                                return self.fail("Can't pass mut arg to repr", .{});
                            const key = args.values[0];
                            const value = args.values[1];
                            if (key != .repr)
                                return self.fail("Can't pass {} to {}", .{ key, head });
                            if (value != .repr)
                                return self.fail("Can't pass {} to {}", .{ value, head });
                            return .{ .repr = .{ .map = .{ box(self.allocator, key.repr), box(self.allocator, value.repr) } } };
                        },
                        .@"union" => {
                            const reprs = self.allocator.alloc(Repr, args.values.len) catch panic("OOM", .{});
                            for (reprs, args.values) |*repr, member| {
                                if (member != .repr)
                                    return self.fail("Can't pass {} to {}", .{ member, head });
                                repr.* = member.repr;
                            }
                            return .{ .repr = .{ .@"union" = reprs } };
                        },
                        .only => {
                            if (args.values.len != 1)
                                return self.fail("Wrong number of arguments ({}) to {}", .{ args.values.len, head });
                            if (make.args.muts[0] == true)
                                return self.fail("Can't pass mut arg to repr", .{});
                            const only_value = args.values[0];
                            return .{ .repr = .{ .only = box(self.allocator, only_value) } };
                        },
                    }
                },
                else => return self.fail("Cannot make {}", .{head}),
            }
        },
        .call => |call| {
            const head_expr = self.parser.exprs.items[call.head];
            if (head_expr == .builtin) {
                const args = try self.evalObject(call.args);
                switch (head_expr.builtin) {
                    .as => {
                        if (args.values.len != 2)
                            return self.fail("Wrong number of arguments ({}) to {}", .{ args.values.len, head_expr });
                        const value = args.values[0];
                        const repr = args.values[1];
                        if (repr != .repr)
                            return self.fail("Cannot pass {} to {}", .{ args.values.len, head_expr });
                        return self.convert(repr.repr, value);
                    },
                    .get => {
                        if (args.values.len != 2)
                            return self.fail("Wrong number of arguments ({}) to {}", .{ args.values.len, head_expr });
                        if (call.args.muts[0] == true)
                            return self.fail("Can't pass mut arg to {}", .{head_expr});
                        if (call.args.muts[1] == true)
                            return self.fail("Can't pass mut arg to {}", .{head_expr});
                        if (args.repr.keys[0] != .i64 or args.repr.keys[0].i64 != 0)
                            return self.fail("Can't pass named key to {}", .{head_expr});
                        if (args.repr.keys[1] != .i64 or args.repr.keys[1].i64 != 1)
                            return self.fail("Can't pass named key to {}", .{head_expr});
                        const object = args.values[0];
                        const key = args.values[1];
                        return (try self.objectGet(object, key)).*;
                    },
                    .@"try-get" => {
                        if (args.values.len != 2)
                            return self.fail("Wrong number of arguments ({}) to {}", .{ args.values.len, head_expr });
                        if (call.args.muts[0] == true)
                            return self.fail("Can't pass mut arg to {}", .{head_expr});
                        if (call.args.muts[1] == true)
                            return self.fail("Can't pass mut arg to {}", .{head_expr});
                        if (args.repr.keys[0] != .i64 or args.repr.keys[0].i64 != 0)
                            return self.fail("Can't pass named key to {}", .{head_expr});
                        if (args.repr.keys[1] != .i64 or args.repr.keys[1].i64 != 1)
                            return self.fail("Can't pass named key to {}", .{head_expr});
                        const object = args.values[0];
                        const key = args.values[1];
                        if (self.objectGet(object, key)) |value| {
                            return .{ .@"struct" = .{
                                .repr = .{
                                    .keys = self.allocator.dupe(Value, &[_]Value{.{ .string = self.allocator.dupe(u8, "some") catch panic("OOM", .{}) }}) catch panic("OOM", .{}),
                                    .reprs = self.allocator.dupe(Repr, &[_]Repr{value.reprOf()}) catch panic("OOM", .{}),
                                },
                                .values = self.allocator.dupe(Value, &[_]Value{value.*}) catch panic("OOM", .{}),
                            } };
                        } else |_| {
                            return .{ .string = self.allocator.dupe(u8, "none") catch panic("OOM", .{}) };
                        }
                    },
                    .@"get-repr" => {
                        if (args.values.len != 1)
                            return self.fail("Wrong number of arguments ({}) to {}", .{ args.values.len, head_expr });
                        return .{ .repr = args.values[0].reprOf() };
                    },
                    .@"get-only" => {
                        if (args.values.len != 1)
                            return self.fail("Wrong number of arguments ({}) to {}", .{ args.values.len, head_expr });
                        const value = args.values[0];
                        if (value != .repr or value.repr != .only)
                            return self.fail("Cannot pass {} to {}", .{ value, head_expr });
                        return value.repr.only.*;
                    },
                    else => {
                        if (args.values.len != 2)
                            return self.fail("Wrong number of arguments ({}) to {}", .{ args.values.len, head_expr });
                        if (args.repr.keys[0] != .i64 or args.repr.keys[0].i64 != 0)
                            return self.fail("Can't pass named key to {}", .{head_expr});
                        if (args.repr.keys[1] != .i64 or args.repr.keys[1].i64 != 1)
                            return self.fail("Can't pass named key to {}", .{head_expr});
                        switch (head_expr.builtin) {
                            .equal => return fromBool(args.values[0].equal(args.values[1])),
                            .equivalent => {
                                if (self.convert(args.values[0].reprOf(), args.values[1])) |values_1_ish| {
                                    return fromBool(args.values[0].equal(values_1_ish));
                                } else |_| {
                                    // Assuming convert is correct, there is no way to represent the notation of args.values[1] in the repr of args.values[0], so they can't possibly have the same notation.
                                    return fromBool(false);
                                }
                            },
                            .less_than, .less_than_or_equal, .more_than, .more_than_or_equal => panic("TODO", .{}),
                            .add, .subtract, .multiply, .divide => {
                                if (!(args.values[0] == .i64 and args.values[1] == .i64) and
                                    !(args.values[0] == .f64 and args.values[1] == .f64))
                                    return self.fail("Cannot call {} on {} and {}", .{ head_expr, args.values[0], args.values[1] });
                                if (args.values[0] == .f64) {
                                    const result = switch (head_expr.builtin) {
                                        .add => args.values[0].f64 + args.values[1].f64,
                                        .subtract => args.values[0].f64 - args.values[1].f64,
                                        .multiply => args.values[0].f64 * args.values[1].f64,
                                        .divide => args.values[0].f64 / args.values[1].f64,
                                        else => unreachable,
                                    };
                                    return .{ .f64 = result };
                                } else {
                                    const result = switch (head_expr.builtin) {
                                        .add => args.values[0].i64 + args.values[1].i64,
                                        .subtract => args.values[0].i64 - args.values[1].i64,
                                        .multiply => args.values[0].i64 * args.values[1].i64,
                                        .divide => return self.fail("Cannot call {} on {} and {}", .{ head_expr, args.values[0], args.values[1] }),
                                        else => unreachable,
                                    };
                                    return .{ .i64 = result };
                                }
                            },
                            else => unreachable,
                        }
                    },
                }
            } else {
                const head = try self.eval(call.head);
                const args = try self.evalObject(call.args);
                switch (head) {
                    .@"fn" => |@"fn"| {
                        if (args.values.len != @"fn".params.len)
                            return self.fail("Wrong number of arguments ({}) to {}", .{ args.values.len, head });
                        for (call.args.muts, @"fn".muts) |arg_mut, param_mut| {
                            if (arg_mut != param_mut)
                                return self.fail("Expected {s} arg, found {s} arg", .{
                                    if (param_mut) "mut" else "const",
                                    if (arg_mut) "mut" else "const",
                                });
                        }
                        for (args.repr.keys, 0..) |key, i| {
                            if (key != .i64 or key.i64 != i)
                                return self.fail("Can't pass named key to fn", .{});
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
                        for (@"fn".params, call.args.muts, args.values) |param, arg_mut, arg| {
                            fn_scope.append(.{
                                .mut = arg_mut,
                                .name = param,
                                .value = arg.copy(self.allocator),
                            }) catch panic("OOM", .{});
                        }

                        // Call fn.
                        std.mem.swap(Scope, &fn_scope, &self.scope);
                        const return_value = try self.eval(@"fn".body);

                        // Copy mut args back to original locations.
                        for (@"fn".params, call.args.muts, call.args.keys) |param, arg_mut, arg| {
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
            const value = try self.eval(get_static.object);
            const key = switch (get_static.key) {
                .i64 => |int| Value{ .i64 = int },
                .string => |string| Value{ .string = self.allocator.dupe(u8, string) catch panic("OOM", .{}) },
            };
            return (try self.objectGet(value, key)).*;
        },
        .exprs => |exprs| {
            const scope_len = self.scope.items.len;
            var value = Value{ .i64 = 0 }; // TODO void/null or similar
            for (exprs) |subexpr| {
                value = try self.eval(subexpr);
            }
            self.scope.shrinkRetainingCapacity(scope_len);
            return value;
        },
    }
}

fn evalObject(self: *Self, object_expr: ObjectExpr) error{SemantalyzeError}!Struct {
    var keys = self.allocator.alloc(Value, object_expr.keys.len) catch panic("OOM", .{});
    var values = self.allocator.alloc(Value, object_expr.values.len) catch panic("OOM", .{});
    var reprs = self.allocator.alloc(Repr, object_expr.values.len) catch panic("OOM", .{});
    for (keys, values, reprs, object_expr.keys, object_expr.values) |*key, *value, *repr, key_id, value_id| {
        key.* = try self.eval(key_id);
        value.* = try self.eval(value_id);
        repr.* = value.reprOf();
    }
    const struct_unsorted = Struct{
        .repr = .{
            .keys = keys,
            .reprs = reprs,
        },
        .values = values,
    };
    var ixes = self.allocator.alloc(usize, keys.len) catch panic("OOM", .{});
    for (ixes, 0..) |*ix, i| {
        ix.* = i;
    }
    std.sort.heap(usize, ixes, struct_unsorted, (struct {
        fn lessThan(context: Struct, a: usize, b: usize) bool {
            switch (context.repr.keys[a].order(context.repr.keys[b])) {
                .lt => return true,
                .gt => return false,
                .eq => {},
            }
            switch (context.values[a].order(context.values[b])) {
                .lt => return true,
                .gt => return false,
                .eq => {},
            }
            return false;
        }
    }).lessThan);
    const struct_sorted = Struct{
        .repr = .{
            .keys = permute(self.allocator, ixes, Value, struct_unsorted.repr.keys),
            .reprs = permute(self.allocator, ixes, Repr, struct_unsorted.repr.reprs),
        },
        .values = permute(self.allocator, ixes, Value, struct_unsorted.values),
    };
    {
        const sorted_keys = struct_sorted.repr.keys;
        if (sorted_keys.len > 1) {
            for (1..sorted_keys.len) |i| {
                if (sorted_keys[i - 1].equal(sorted_keys[i])) {
                    return self.fail("Duplicate key in map literal: {}", .{sorted_keys[i]});
                }
            }
        }
    }
    return struct_sorted;
}

fn capture(self: *Self, expr_id: ExprId, captures: *Scope, locals: *ArrayList([]const u8)) error{SemantalyzeError}!void {
    const expr = self.parser.exprs.items[expr_id];
    switch (expr) {
        .i64, .f64, .string, .builtin => {},
        .object => |object| {
            for (object.keys) |key| try self.capture(key, captures, locals);
            for (object.values) |value| try self.capture(value, captures, locals);
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
        .make => |make| {
            try self.capture(make.head, captures, locals);
            for (make.args.keys) |key| {
                try self.capture(key, captures, locals);
            }
            for (make.args.values) |value| {
                try self.capture(value, captures, locals);
            }
        },
        .call => |call| {
            try self.capture(call.head, captures, locals);
            for (call.args.keys) |key| {
                try self.capture(key, captures, locals);
            }
            for (call.args.values) |value| {
                try self.capture(value, captures, locals);
            }
        },
        .get_static => |get_static| {
            try self.capture(get_static.object, captures, locals);
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

fn objectGet(self: *Self, object: Value, key: Value) error{SemantalyzeError}!*Value {
    return switch (object) {
        .@"struct" => |@"struct"| self.structGet(@"struct", key),
        .list => |list| self.listGet(list, key),
        .map => |map| self.mapGet(map, key),
        else => return self.fail("Cannot get key {} from non-object {}", .{ key, object }),
    };
}

fn structGet(self: *Self, @"struct": Struct, key: Value) error{SemantalyzeError}!*Value {
    for (@"struct".repr.keys, @"struct".values) |struct_key, *value| {
        if (struct_key.equal(key)) return value;
    } else {
        return self.fail("Key {} not found in {}", .{ key, Value{ .@"struct" = @"struct" } });
    }
}

fn listGet(self: *Self, list: List, key: Value) error{SemantalyzeError}!*Value {
    if (key == .i64 and key.i64 >= 0 and key.i64 < list.elems.items.len) {
        return &list.elems.items[@intCast(key.i64)];
    } else {
        return self.fail("Key {} not found in {}", .{ key, Value{ .list = list } });
    }
}

fn mapGet(self: *Self, map: Map, key: Value) error{SemantalyzeError}!*Value {
    if (map.entries.getPtr(key)) |value| {
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
                    if (call.args.values.len != 1)
                        return self.fail("Wrong number of arguments ({}) to {}", .{ call.args.values.len, head });
                    if (call.args.muts[0] == true)
                        return self.fail("Can't pass mut arg to map", .{});
                    const key_expr = self.parser.exprs.items[call.args.keys[0]];
                    if (key_expr != .i64 or key_expr.i64 != 0)
                        return self.fail("Can't pass named key to map", .{});
                    const value = try self.eval(call.args.values[0]);
                    return self.mapGet(map.*, value);
                },
                else => return self.fail("Cannot call {}", .{head}),
            }
        },
        .get_static => |get_static| {
            const value = try self.evalPath(get_static.object);
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

fn fromBool(b: bool) Value {
    return .{ .i64 = if (b) 1 else 0 };
}

fn toBool(self: *Self, value: Value) error{SemantalyzeError}!bool {
    if (value == .i64) {
        if (value.i64 == 1) return true;
        if (value.i64 == 0) return false;
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
    // TODO We should also resolve repr/repr_kind here, but needs some refactoring.
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
        .@"struct" => |struct_repr| {
            const values = self.allocator.alloc(Value, struct_repr.keys.len) catch panic("OOM", .{});
            switch (value) {
                .@"struct" => |@"struct"| {
                    if (struct_repr.keys.len != @"struct".repr.keys.len)
                        return self.fail("Cannot convert {} to {}", .{ value, repr });
                    for (struct_repr.keys, @"struct".repr.keys) |repr_key, key| {
                        if (!repr_key.equal(key))
                            return self.fail("Cannot convert {} to {}", .{ value, repr });
                    }
                    for (values, @"struct".values, struct_repr.reprs) |*value_new, value_old, repr_new| {
                        value_new.* = try self.convert(repr_new, value_old);
                    }
                },
                .list => |list| {
                    for (struct_repr.keys, values, struct_repr.reprs) |key, *value_new, repr_new| {
                        if (key != .i64 or key.i64 < 0 or key.i64 >= list.elems.items.len)
                            return self.fail("Cannot convert {} to {}", .{ value, repr });
                        value_new.* = try self.convert(repr_new, list.elems.items[@intCast(key.i64)]);
                    }
                },
                .map => |map| {
                    for (struct_repr.keys, values, struct_repr.reprs) |key, *value_new, repr_new| {
                        const value_old = map.entries.get(key) orelse
                            return self.fail("Cannot convert {} to {}", .{ value, repr });
                        value_new.* = try self.convert(repr_new, value_old);
                    }
                },
                else => return self.fail("Cannot convert {} to {}", .{ value, repr }),
            }
            return .{ .@"struct" = .{ .repr = struct_repr, .values = values } };
        },
        .list => |list_repr| {
            var elems = ArrayList(Value).init(self.allocator);
            switch (value) {
                .@"struct" => |@"struct"| {
                    for (0.., @"struct".repr.keys, @"struct".values) |ix, key, elem| {
                        if (key != .i64 or key.i64 != ix)
                            return self.fail("Cannot convert {} to {}", .{ value, repr });
                        elems.append(try self.convert(list_repr.*, elem)) catch panic("OOM", .{});
                    }
                },
                .list => |list| {
                    for (list.elems.items) |elem| {
                        elems.append(try self.convert(list_repr.*, elem)) catch panic("OOM", .{});
                    }
                },
                .map => |map| {
                    for (0..map.entries.count()) |ix| {
                        const elem = map.entries.get(.{ .i64 = @intCast(ix) }) orelse
                            return self.fail("Cannot convert {} to {}", .{ value, repr });
                        elems.append(try self.convert(list_repr.*, elem)) catch panic("OOM", .{});
                    }
                },
                else => return self.fail("Cannot convert {} to {}", .{ value, repr }),
            }
            return .{ .list = .{ .repr = list_repr, .elems = elems } };
        },
        .map => |map_repr| {
            var entries = ValueHashMap.init(self.allocator);
            switch (value) {
                .@"struct" => |@"struct"| {
                    for (@"struct".repr.keys, @"struct".values) |key, val| {
                        entries.putNoClobber(
                            try self.convert(map_repr[0].*, key),
                            try self.convert(map_repr[1].*, val),
                        ) catch panic("OOM", .{});
                    }
                },
                .list => |list| {
                    for (0.., list.elems.items) |ix, elem| {
                        entries.putNoClobber(
                            try self.convert(map_repr[0].*, .{ .i64 = @intCast(ix) }),
                            try self.convert(map_repr[1].*, elem),
                        ) catch panic("OOM", .{});
                    }
                },
                .map => |map| {
                    var iter = map.entries.iterator();
                    while (iter.next()) |entry| {
                        entries.putNoClobber(
                            try self.convert(map_repr[0].*, entry.key_ptr.*),
                            try self.convert(map_repr[1].*, entry.value_ptr.*),
                        ) catch panic("OOM", .{});
                    }
                },
                else => return self.fail("Cannot convert {} to {}", .{ value, repr }),
            }
            return .{ .map = .{ .repr = map_repr, .entries = entries } };
        },
        .@"union" => |union_repr| {
            const value_repr = value.reprOf();
            for (0.., union_repr) |tag, member_repr| {
                if (value_repr.order(member_repr) == .eq) {
                    return .{ .@"union" = .{ .repr = union_repr, .tag = tag, .value = box(self.allocator, value) } };
                }
            }
            return self.fail("Cannot convert {} to {}", .{ value, repr });
        },
        .any => {
            return .{ .any = box(self.allocator, value) };
        },
        .only => |only_repr| {
            if (value.equal(only_repr.*)) {
                return .{ .only = .{ .repr = only_repr } };
            } else {
                return self.fail("Cannot convert {} to {}", .{ value, repr });
            }
        },
        .repr => {
            if (value == .repr) {
                return value;
            } else {
                return self.fail("Cannot convert {} to {}", .{ value, repr });
            }
        },
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

fn permute(allocator: Allocator, ixes: []const usize, comptime T: type, things: []T) []T {
    const things_copy = allocator.dupe(T, things) catch panic("OOM", .{});
    for (things_copy, ixes) |*thing, ix| {
        thing.* = things[ix];
    }
    return things_copy;
}

fn isName(text: []const u8) bool {
    for (text) |char| {
        switch (char) {
            'a'...'z', '0'...'9', '-' => {},
            else => return false,
        }
    }
    return true;
}
