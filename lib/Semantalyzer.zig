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
const ObjectExpr = Parser.ObjectExpr;
const StaticKey = Parser.StaticKey;
const Analyzer = @import("./Analyzer.zig");
const Place = Analyzer.Place;

const Self = @This();
allocator: Allocator,
parser: Parser,
scope: Scope,
next_call_id: usize,
return_to: ?struct {
    call_id: usize,
    value: Value,
},
error_message: ?[]const u8,

pub const Scope = ArrayList(Binding);
pub const Binding = struct {
    mut: bool,
    call_id: ?usize,
    name: []const u8,
    value: Value,
};

const FormatKey = struct {
    key: Value,

    pub fn format(self: FormatKey, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = fmt;
        _ = options;
        if (self.key == .string and isName(self.key.string)) {
            try writer.print("/{s}", .{self.key.string});
        } else {
            try writer.print("/{}", .{self.key});
        }
    }
};

pub const Value = union(enum) {
    i64: i64,
    f64: f64,
    string: []u8,
    @"struct": Struct,
    list: List,
    map: Map,
    @"union": Union,
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
            .@"fn" => {
                panic("Fn used as value", .{});
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
                const self_key = self.@"union".repr.keys[self.@"union".tag];
                const other_key = other.@"union".repr.keys[other.@"union".tag];
                switch (self_key.order(other_key)) {
                    .lt => return .lt,
                    .gt => return .gt,
                    .eq => {},
                }
                return self.@"union".value.order(other.@"union".value.*);
            },
            .@"fn" => {
                panic("Fn used as value", .{});
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
            .@"fn" => {
                panic("Fn used as value", .{});
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
            .@"struct" => |@"struct"| {
                try writer.writeAll("[");
                var first = true;
                var positional = true;
                for (0.., @"struct".repr.keys, @"struct".values) |i, key, value| {
                    if (!first) try writer.writeAll(", ");
                    if (!positional or key != .i64 or key.i64 != i) {
                        try writer.print("{} ", .{FormatKey{ .key = key }});
                        positional = false;
                    }
                    try writer.print("{}", .{value});
                    first = false;
                }
                try writer.writeAll("]");
            },
            .list => |list| {
                try writer.print("{}[", .{Repr{ .list = list.repr }});
                var first = true;
                for (list.elems.items) |elem| {
                    if (!first) try writer.writeAll(", ");
                    try writer.print("{}", .{elem});
                    first = false;
                }
                try writer.writeAll("]");
            },
            .map => |map| {
                try writer.print("{}[", .{Repr{ .map = map.repr }});

                const entries = mapSortedEntries(map);
                defer entries.deinit();

                var first = true;
                for (entries.items) |entry| {
                    if (!first) try writer.writeAll(", ");
                    try writer.print("{} {}", .{ FormatKey{ .key = entry.key_ptr.* }, entry.value_ptr.* });
                    first = false;
                }

                try writer.writeAll("]");
            },
            .@"union" => |@"union"| {
                try writer.print("{}[", .{Repr{ .@"union" = @"union".repr }});
                const key = @"union".repr.keys[@"union".tag];
                const value = @"union".value;
                if (key != .i64 or key.i64 != 0) {
                    try writer.print("{} ", .{FormatKey{ .key = key }});
                }
                try writer.print("{}", .{value});
                try writer.writeAll("]");
            },
            .@"fn" => |@"fn"| {
                try writer.print("{{fn {}}}", .{@"fn"});
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
            .string => |string| return .{ .string = allocator.dupe(u8, string) catch oom() },
            .@"struct" => |@"struct"| {
                var values_copy = allocator.dupe(Value, @"struct".values) catch oom();
                for (values_copy) |*value| {
                    value.copyInPlace(allocator);
                }
                return .{ .@"struct" = .{
                    .repr = @"struct".repr,
                    .values = values_copy,
                } };
            },
            .list => |list| {
                var elems_copy = list.elems.clone() catch oom();
                for (elems_copy.items) |*elem| {
                    elem.copyInPlace(allocator);
                }
                return .{ .list = .{
                    .repr = list.repr,
                    .elems = elems_copy,
                } };
            },
            .map => |map| {
                var entries_copy = map.entries.cloneWithAllocator(allocator) catch oom();
                var iter = entries_copy.iterator();
                while (iter.next()) |entry| {
                    entry.key_ptr.copyInPlace(allocator);
                    entry.value_ptr.copyInPlace(allocator);
                }
                return .{ .map = .{ .repr = map.repr, .entries = entries_copy } };
            },
            .@"union" => |@"union"| {
                const value_copy = @"union".value.copy(allocator);
                return .{ .@"union" = .{ .repr = @"union".repr, .tag = @"union".tag, .value = box(allocator, value_copy) } };
            },
            // TODO
            .@"fn" => return self,
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
            .@"fn" => return .i64, // TODO,
            .repr => return .repr,
            .repr_kind => return .i64, // TODO,
        }
    }

    fn emptyStruct() Value {
        return .{ .@"struct" = .{ .repr = .{ .keys = &.{}, .reprs = &.{} }, .values = &.{} } };
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

pub const Repr = union(enum) {
    i64,
    f64,
    string,
    @"struct": StructRepr,
    list: ListRepr,
    map: MapRepr,
    @"union": UnionRepr,
    @"fn": FnRepr,
    repr,

    pub fn update(self: Repr, hasher: anytype) void {
        hasher.update(&[1]u8{@intFromEnum(std.meta.activeTag(self))});
        switch (self) {
            .i64, .f64, .string, .repr => {},
            .@"struct" => |@"struct"| {
                @"struct".update(hasher);
            },
            .list => |elem| {
                elem.update(hasher);
            },
            .map => |key_value| {
                key_value[0].update(hasher);
                key_value[1].update(hasher);
            },
            .@"union" => |@"union"| {
                for (@"union".keys) |key| {
                    key.update(hasher);
                }
                for (@"union".reprs) |repr| {
                    repr.update(hasher);
                }
            },
            .@"fn" => |@"fn"| {
                hasher.update(std.mem.asBytes(&@"fn".expr_id));
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
            .i64, .f64, .string, .repr => return .eq,
            .@"struct" => return self.@"struct".order(other.@"struct"),
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
                const self_union = self.@"union";
                const other_union = other.@"union";
                for (0..@min(self_union.keys.len, other_union.keys.len)) |i| {
                    switch (self_union.keys[i].order(other_union.keys[i])) {
                        .lt => return .lt,
                        .gt => return .gt,
                        .eq => {},
                    }
                    switch (self_union.reprs[i].order(other_union.reprs[i])) {
                        .lt => return .lt,
                        .gt => return .gt,
                        .eq => {},
                    }
                }
                return std.math.order(self_union.keys.len, other_union.keys.len);
            },
            .@"fn" => panic("TODO", .{}),
        }
    }

    pub fn format(self: Repr, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = fmt;
        _ = options;
        try writer.writeAll(@tagName(self));
        switch (self) {
            .i64, .f64, .string, .repr => {},
            .@"struct" => |@"struct"| {
                try writer.writeAll("[");
                var first = true;
                for (0.., @"struct".keys, @"struct".reprs) |i, key, repr| {
                    if (!first) try writer.writeAll(", ");
                    if (key != .i64 or key.i64 != i)
                        try writer.print("{} ", .{FormatKey{ .key = key }});
                    try writer.print("{}", .{repr});
                    first = false;
                }
                try writer.writeAll("]");
            },
            .list => |elem| try writer.print("[{}]", .{elem.*}),
            .map => |key_value| try writer.print("[{}, {}]", .{ key_value[0].*, key_value[1].* }),
            .@"union" => |@"union"| {
                try writer.writeAll("[");
                var first = true;
                var positional = true;
                for (0.., @"union".keys, @"union".reprs) |i, key, repr| {
                    if (!first) try writer.writeAll(", ");
                    if (!positional or key != .i64 or key.i64 != i) {
                        try writer.print("{} ", .{FormatKey{ .key = key }});
                        positional = false;
                    }
                    try writer.print("{}", .{repr});
                    first = false;
                }
                try writer.writeAll("]");
            },
            .@"fn" => panic("TODO", .{}),
        }
    }

    pub fn emptyStruct() Repr {
        return .{ .@"struct" = .{ .keys = &.{}, .reprs = &.{} } };
    }

    pub fn sizeOf(self: Repr) usize {
        return switch (self) {
            .i64 => 8,
            .@"struct" => |@"struct"| @"struct".sizeOf(),
            else => panic("TODO: {}.sizeOf()", .{self}),
        };
    }
};

pub const StructRepr = struct {
    keys: []Value,
    reprs: []Repr,

    pub fn update(self: StructRepr, hasher: anytype) void {
        for (self.keys, self.reprs) |key, repr| {
            key.update(hasher);
            repr.update(hasher);
        }
    }

    pub fn order(self: StructRepr, other: StructRepr) std.math.Order {
        for (0..@min(self.keys.len, other.keys.len)) |i| {
            switch (self.keys[i].order(other.keys[i])) {
                .lt => return .lt,
                .gt => return .gt,
                .eq => {},
            }
            switch (self.reprs[i].order(other.reprs[i])) {
                .lt => return .lt,
                .gt => return .gt,
                .eq => {},
            }
        }
        return std.math.order(self.keys.len, other.keys.len);
    }

    pub fn sorted(allocator: Allocator, keys: []Value, reprs: []Repr) StructRepr {
        var ixes = allocator.alloc(usize, keys.len) catch oom();
        for (ixes, 0..) |*ix, i| ix.* = i;
        std.sort.heap(usize, ixes, keys, (struct {
            fn lessThan(context: []Value, a: usize, b: usize) bool {
                switch (context[a].order(context[b])) {
                    .lt => return true,
                    .gt, .eq => return false,
                }
            }
        }).lessThan);

        return .{
            .keys = permute(allocator, ixes, Value, keys),
            .reprs = permute(allocator, ixes, Repr, reprs),
        };
    }

    // TODO padding and alignment

    pub fn sizeOf(self: StructRepr) usize {
        var size: usize = 0;
        for (self.reprs) |repr| {
            size += repr.sizeOf();
        }
        return size;
    }

    pub fn offsetOf(self: StructRepr, target_key: Value) ?usize {
        var offset: usize = 0;
        for (self.keys, self.reprs) |key, repr| {
            if (key.equal(target_key)) {
                return offset;
            }
            offset += repr.sizeOf();
        }
        return null;
    }

    pub fn placeOf(self: StructRepr, place: Place, target_key: Value) ?Place {
        assert(place.length == self.sizeOf());
        var offset: usize = 0;
        for (self.keys, self.reprs) |key, repr| {
            if (key.equal(target_key)) {
                return .{ .base = place.base, .offset = place.offset + @as(u32, @intCast(offset)), .length = @intCast(repr.sizeOf()) };
            }
            offset += repr.sizeOf();
        }
        return null;
    }

    pub fn ixOf(self: StructRepr, target_key: Value) ?usize {
        for (self.keys, 0..) |key, ix| {
            if (key.equal(target_key)) {
                return ix;
            }
        }
        return null;
    }
};

pub const ListRepr = *Repr;
pub const MapRepr = [2]*Repr;

pub const UnionRepr = struct {
    keys: []Value,
    reprs: []Repr,
};

pub const FnRepr = struct {
    name: []const u8,
    expr_id: ExprId,
    scope: []Analyzer.Binding,
};

pub const ReprKind = enum {
    @"struct",
    list,
    map,
    @"union",

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
    var entries = ArrayList(ValueHashMap.Entry).initCapacity(map.entries.allocator, map.entries.count()) catch oom();
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
    name: ?[]const u8,
    scope: []Binding,
    params: ObjectPattern,
    body: ExprId,
};

pub const ObjectPattern = struct {
    muts: []bool,
    keys: []Value,
    values: []Pattern,
};

// TODO Flesh out.
pub const Pattern = []const u8;

pub fn init(allocator: Allocator, parser: Parser) Self {
    return .{
        .allocator = allocator,
        .parser = parser,
        .scope = Scope.init(allocator),
        .next_call_id = 0,
        .return_to = null,
        .error_message = null,
    };
}

pub fn semantalyze(self: *Self) error{SemantalyzeError}!Value {
    return self.eval(self.parser.exprs.items.len - 1) catch |err| {
        switch (err) {
            error.SemantalyzeError => |err2| return err2,
            error.ReturnTo => unreachable,
        }
    };
}

fn eval(self: *Self, expr_id: ExprId) error{ ReturnTo, SemantalyzeError }!Value {
    const expr = self.parser.exprs.items[expr_id];
    switch (expr) {
        .i64 => |int| return .{ .i64 = int },
        .f64 => |float| return .{ .f64 = float },
        .string => |string| return .{ .string = self.allocator.dupe(u8, string) catch oom() },
        .object => |object_expr| {
            return .{ .@"struct" = try self.evalObject(object_expr) };
        },
        .builtin => {
            return self.fail("Direct eval of builtin should be unreachable", .{});
        },
        .name => |name| {
            if (self.lookupBinding(name)) |binding| {
                if (binding.value == .@"fn") {
                    const parent_id = self.parser.parents[expr_id];
                    if (parent_id == null or
                        self.parser.exprs.items[parent_id.?] != .call)
                        return self.fail("Functions may only be referenced as the head of a call or an argument to a call", .{});
                }
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
                return err;
            }
        },
        .mut => {
            return self.fail("Mut not allowed here", .{});
        },
        .let => |let| {
            const value = try self.eval(let.value);
            if (self.lookupBinding(let.name)) |_| {
                return self.fail("Name {s} shadows earlier definition", .{let.name});
            } else |_| {
                self.scope.append(.{
                    .mut = let.mut,
                    .call_id = null,
                    .name = let.name,
                    .value = value.copy(self.allocator),
                }) catch oom();
                return Value.emptyStruct();
            }
        },
        .set => |set| {
            const value = try self.eval(set.value);
            try self.pathSet(set.path, value.copy(self.allocator));
            return Value.emptyStruct();
        },
        .@"if" => |@"if"| {
            const cond = try self.toBool(try self.eval(@"if".cond));
            return self.eval(if (cond) @"if".then else @"if".@"else");
        },
        .@"while" => |@"while"| {
            while (true) {
                const cond = try self.toBool(try self.eval(@"while".cond));
                if (!cond) return Value.emptyStruct();
                _ = try self.eval(@"while".body);
            }
        },
        .@"fn" => |@"fn"| {
            const parent_id = self.parser.parents[expr_id];
            if (parent_id == null)
                return self.fail("Functions may only be defined at the top of definitions or call arguments", .{});
            const parent = self.parser.exprs.items[parent_id.?];
            const name = switch (parent) {
                .let => |let| let.name,
                .call => null,
                else => return self.fail("Functions may only be defined at the top of definitions or call arguments", .{}),
            };
            return .{ .@"fn" = .{
                .name = name,
                .scope = self.allocator.dupe(Binding, self.scope.items) catch oom(),
                .params = try self.evalObjectPattern(@"fn".params),
                .body = @"fn".body,
            } };
        },
        .make => |make| {
            const head = try self.eval(make.head);
            const args = try self.evalObject(make.args);
            switch (head) {
                .repr => |repr| {
                    switch (repr) {
                        .@"struct", .list, .map, .@"union" => {
                            return self.convert(repr, .{ .@"struct" = args });
                        },
                        else => {
                            if (args.values.len != 1)
                                return self.fail("Wrong number of arguments ({}) to {}", .{ args.values.len, head });
                            //if (make.args.muts[0] == true)
                            //return self.fail("Can't pass mut arg to repr", .{});
                            return self.convert(repr, args.values[0]);
                        },
                    }
                },
                .repr_kind => |repr_kind| {
                    switch (repr_kind) {
                        .@"struct" => {
                            const reprs = self.allocator.alloc(Repr, args.values.len) catch oom();
                            for (reprs, args.values) |*repr, value| {
                                if (value != .repr)
                                    return self.fail("Can't pass {} to {}", .{ value, head });
                                repr.* = value.repr;
                            }
                            // TODO sort keys
                            return .{ .repr = .{ .@"struct" = .{
                                .keys = args.repr.keys,
                                .reprs = reprs,
                            } } };
                        },
                        .list => {
                            if (args.values.len != 1)
                                return self.fail("Wrong number of arguments ({}) to {}", .{ args.values.len, head });
                            //if (make.args.muts[0] == true)
                            //    return self.fail("Can't pass mut arg to repr", .{});
                            const elem = args.values[0];
                            if (elem != .repr)
                                return self.fail("Can't pass {} to {}", .{ elem, head });
                            return .{ .repr = .{ .list = box(self.allocator, elem.repr) } };
                        },
                        .map => {
                            if (args.values.len != 2)
                                return self.fail("Wrong number of arguments ({}) to {}", .{ args.values.len, head });
                            //if (make.args.muts[0] == true)
                            //return self.fail("Can't pass mut arg to repr", .{});
                            //if (make.args.muts[1] == true)
                            //return self.fail("Can't pass mut arg to repr", .{});
                            const key = args.values[0];
                            const value = args.values[1];
                            if (key != .repr)
                                return self.fail("Can't pass {} to {}", .{ key, head });
                            if (value != .repr)
                                return self.fail("Can't pass {} to {}", .{ value, head });
                            return .{ .repr = .{ .map = .{ box(self.allocator, key.repr), box(self.allocator, value.repr) } } };
                        },
                        .@"union" => {
                            const keys = self.allocator.alloc(Value, args.repr.keys.len) catch oom();
                            const reprs = self.allocator.alloc(Repr, args.values.len) catch oom();
                            for (keys, reprs, args.repr.keys, args.values) |*key, *repr, arg_key, arg_value| {
                                if (arg_value != .repr)
                                    return self.fail("Can't pass {} to {}", .{ arg_value, head });
                                key.* = arg_key;
                                repr.* = arg_value.repr;
                            }
                            return .{ .repr = .{ .@"union" = .{
                                .keys = keys,
                                .reprs = reprs,
                            } } };
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
                        //if (call.args.muts[0] == true)
                        //    return self.fail("Can't pass mut arg to {}", .{head_expr});
                        //if (call.args.muts[1] == true)
                        //    return self.fail("Can't pass mut arg to {}", .{head_expr});
                        if (args.repr.keys[0] != .i64 or args.repr.keys[0].i64 != 0)
                            return self.fail("Can't pass named key to {}", .{head_expr});
                        if (args.repr.keys[1] != .i64 or args.repr.keys[1].i64 != 1)
                            return self.fail("Can't pass named key to {}", .{head_expr});
                        var object = args.values[0];
                        const key = args.values[1];
                        if (self.objectGet(&object, key)) |value| {
                            return .{ .@"struct" = .{
                                .repr = .{
                                    .keys = self.allocator.dupe(Value, &[_]Value{.{ .string = self.allocator.dupe(u8, "some") catch oom() }}) catch oom(),
                                    .reprs = self.allocator.dupe(Repr, &[_]Repr{value.reprOf()}) catch oom(),
                                },
                                .values = self.allocator.dupe(Value, &[_]Value{value.*}) catch oom(),
                            } };
                        } else |_| {
                            return .{ .string = self.allocator.dupe(u8, "none") catch oom() };
                        }
                    },
                    .@"get-repr" => {
                        if (args.values.len != 1)
                            return self.fail("Wrong number of arguments ({}) to {}", .{ args.values.len, head_expr });
                        return .{ .repr = args.values[0].reprOf() };
                    },
                    .@"return-to" => {
                        if (args.values.len != 2)
                            return self.fail("Wrong number of arguments ({}) to {}", .{ args.values.len, head_expr });
                        //if (call.args.muts[0] == true)
                        //    return self.fail("Can't pass mut arg to {}", .{head_expr});
                        //if (call.args.muts[1] == true)
                        //    return self.fail("Can't pass mut arg to {}", .{head_expr});
                        if (args.repr.keys[0] != .i64 or args.repr.keys[0].i64 != 0)
                            return self.fail("Can't pass named key to {}", .{head_expr});
                        if (args.repr.keys[1] != .i64 or args.repr.keys[1].i64 != 1)
                            return self.fail("Can't pass named key to {}", .{head_expr});
                        const to_expr = self.parser.exprs.items[call.args.values[0]];
                        if (to_expr != .name)
                            return self.fail("Can't return to {}", .{to_expr});
                        const to = try self.lookupBinding(to_expr.name);
                        const call_id = to.call_id orelse return self.fail("Can't return to `{s}` from here", .{to_expr.name});
                        self.return_to = .{
                            .call_id = call_id,
                            .value = args.values[1],
                        };
                        return error.ReturnTo;
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
                        if (@"fn".params.values.len != call.args.values.len)
                            return self.fail("Expected {} arguments, found {} arguments", .{ @"fn".params.values.len, call.args.values.len });

                        for (call.args.values, @"fn".params.muts) |arg, param_mut| {
                            const arg_expr = self.parser.exprs.items[arg];
                            const arg_mut = arg_expr == .mut;
                            if (param_mut != arg_mut)
                                return self.fail("Expected {s} arg, found {s} arg", .{
                                    if (param_mut) "mut" else "const",
                                    if (arg_mut) "mut" else "const",
                                });
                        }

                        // Move back to fn scope.
                        const old_scope = self.scope.toOwnedSlice() catch oom();
                        self.scope.appendSlice(@"fn".scope) catch oom();
                        errdefer {
                            self.scope.shrinkRetainingCapacity(0);
                            self.scope.appendSlice(old_scope) catch oom();
                        }

                        // Add call to scope.
                        const call_id = self.next_call_id;
                        self.next_call_id += 1;
                        if (@"fn".name) |name|
                            self.scope.append(.{
                                .mut = false,
                                .name = name,
                                .call_id = call_id,
                                .value = .{ .@"fn" = @"fn" },
                            }) catch oom();

                        // Add args to scope.
                        // TODO check all args are disjoint
                        try self.matchObject(@"fn".params, .{ .@"struct" = args });

                        // Call fn.
                        const return_value = self.eval(@"fn".body) catch |err| return_to: {
                            if (err == error.ReturnTo and self.return_to.?.call_id == call_id) {
                                const value = self.return_to.?.value;
                                self.return_to = null;
                                break :return_to value;
                            }
                            return err;
                        };

                        // Record new values of mut args
                        var args_after = self.allocator.alloc(Value, @"fn".params.values.len) catch oom();
                        for (@"fn".params.muts, @"fn".params.values, args_after) |mut, param, *arg_after| {
                            if (mut) {
                                arg_after.* = (try self.lookupBinding(param)).value.copy(self.allocator);
                            }
                        }

                        // Restore old scope
                        self.scope.shrinkRetainingCapacity(0);
                        self.scope.appendSlice(old_scope) catch oom();

                        // Copy mut args back to original locations.
                        for (@"fn".params.muts, call.args.values, args_after) |mut, arg, arg_after| {
                            if (mut) {
                                const path = self.parser.exprs.items[arg].mut;
                                try self.pathSet(path, arg_after);
                            }
                        }

                        return return_value;
                    },
                    else => return self.fail("Cannot call {}", .{head}),
                }
            }
        },
        .get => |get| {
            var value = try self.eval(get.object);
            const key = try self.evalKey(get.key);
            return (try self.objectGet(&value, key)).*;
        },
        .statements => |statements| {
            const scope_len = self.scope.items.len;
            var value = Value{ .i64 = 0 }; // TODO void/null or similar
            for (statements) |statement| {
                value = try self.eval(statement);
            }
            self.scope.shrinkRetainingCapacity(scope_len);
            return value;
        },
    }
}

fn evalKey(self: *Self, key: ExprId) error{ ReturnTo, SemantalyzeError }!Value {
    return switch (self.parser.exprs.items[key]) {
        .name => |name| Value{ .string = self.allocator.dupe(u8, name) catch oom() },
        else => return self.eval(key),
    };
}

fn evalObjectPattern(self: *Self, object_pattern: ObjectExpr) error{ ReturnTo, SemantalyzeError }!ObjectPattern {
    const keys_unsorted = self.allocator.alloc(Value, object_pattern.keys.len) catch oom();
    for (keys_unsorted, object_pattern.keys) |*key, pattern_key| key.* = try self.evalKey(pattern_key);

    const muts_unsorted = self.allocator.alloc(bool, object_pattern.values.len) catch oom();
    const values_unsorted = self.allocator.alloc([]const u8, object_pattern.values.len) catch oom();
    for (muts_unsorted, values_unsorted, object_pattern.values) |*mut, *value, pattern_value|
        switch (self.parser.exprs.items[pattern_value]) {
            .name => |name| {
                mut.* = false;
                value.* = name;
            },
            .mut => |mut_id| switch (self.parser.exprs.items[mut_id]) {
                .name => |name| {
                    mut.* = true;
                    value.* = name;
                },
                else => return self.fail("Only name patterns are currently supported", .{}),
            },
            else => return self.fail("Only name patterns are currently supported", .{}),
        };

    var ixes = self.allocator.alloc(usize, keys_unsorted.len) catch oom();
    for (ixes, 0..) |*ix, i| ix.* = i;
    std.sort.heap(usize, ixes, keys_unsorted, (struct {
        fn lessThan(context: []Value, a: usize, b: usize) bool {
            switch (context[a].order(context[b])) {
                .lt => return true,
                .gt, .eq => return false,
            }
        }
    }).lessThan);

    return .{
        .muts = permute(self.allocator, ixes, bool, muts_unsorted),
        .keys = permute(self.allocator, ixes, Value, keys_unsorted),
        .values = permute(self.allocator, ixes, []const u8, values_unsorted),
    };
}

fn evalObject(self: *Self, object_expr: ObjectExpr) error{ ReturnTo, SemantalyzeError }!Struct {
    var keys = self.allocator.alloc(Value, object_expr.keys.len) catch oom();
    var values = self.allocator.alloc(Value, object_expr.values.len) catch oom();
    var reprs = self.allocator.alloc(Repr, object_expr.values.len) catch oom();
    for (keys, values, reprs, object_expr.keys, object_expr.values) |*key, *value, *repr, key_id, value_id| {
        key.* = try self.evalKey(key_id);
        const value_expr = self.parser.exprs.items[value_id];
        value.* = try self.eval(if (value_expr == .mut) value_expr.mut else value_id);
        repr.* = value.reprOf();
    }
    const struct_unsorted = Struct{
        .repr = .{
            .keys = keys,
            .reprs = reprs,
        },
        .values = values,
    };
    var ixes = self.allocator.alloc(usize, keys.len) catch oom();
    for (ixes, 0..) |*ix, i| ix.* = i;
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

fn objectGet(self: *Self, object: *Value, key: Value) error{SemantalyzeError}!*Value {
    return switch (object.*) {
        .@"struct" => |*@"struct"| self.structGet(@"struct", key),
        .list => |*list| self.listGet(list, key),
        .map => |*map| self.mapGet(map, key),
        .@"union" => |*@"union"| self.unionGet(@"union", key),
        else => return self.fail("Cannot get key {} from non-object {}", .{ key, object }),
    };
}

fn structGet(self: *Self, @"struct": *Struct, key: Value) error{SemantalyzeError}!*Value {
    for (@"struct".repr.keys, @"struct".values) |struct_key, *value| {
        if (struct_key.equal(key)) return value;
    } else {
        return self.fail("Key {} not found in {}", .{ key, Value{ .@"struct" = @"struct".* } });
    }
}

fn listGet(self: *Self, list: *List, key: Value) error{SemantalyzeError}!*Value {
    if (key == .i64 and key.i64 >= 0 and key.i64 < list.elems.items.len) {
        return &list.elems.items[@intCast(key.i64)];
    } else {
        return self.fail("Key {} not found in {}", .{ key, Value{ .list = list.* } });
    }
}

fn mapGet(self: *Self, map: *Map, key: Value) error{SemantalyzeError}!*Value {
    if (map.entries.getPtr(key)) |value| {
        return value;
    } else {
        return self.fail("Key {} not found in {}", .{ key, Value{ .map = map.* } });
    }
}

fn unionGet(self: *Self, @"union": *Union, key: Value) error{SemantalyzeError}!*Value {
    const union_key = @"union".repr.keys[@"union".tag];
    if (union_key.equal(key))
        return @"union".value
    else
        return self.fail("Key {} not found in {}", .{ key, Value{ .@"union" = @"union".* } });
}

fn pathSet(self: *Self, expr_id: ExprId, value: Value) !void {
    const path = try self.evalPath(expr_id);
    path.* = value;
}

// This should only be called from `set` - too footgunny otherwise.
fn evalPath(self: *Self, expr_id: ExprId) error{ ReturnTo, SemantalyzeError }!*Value {
    const expr = self.parser.exprs.items[expr_id];
    switch (expr) {
        .name => |name| {
            const binding = try self.lookupBinding(name);
            if (!binding.mut) return self.fail("Cannot use a non-mut variable in a path: {}", .{std.zig.fmtEscapes(name)});
            return &binding.value;
        },
        .get => |get| {
            const value = try self.evalPath(get.object);
            const key = try self.evalKey(get.key);
            return self.objectGet(value, key);
        },
        else => return self.fail("Not allowed in set expr {}", .{expr}),
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

fn lookupBinding(self: *Self, name: []const u8) error{SemantalyzeError}!*Binding {
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
            const values = self.allocator.alloc(Value, struct_repr.keys.len) catch oom();
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
                        elems.append(try self.convert(list_repr.*, elem)) catch oom();
                    }
                },
                .list => |list| {
                    for (list.elems.items) |elem| {
                        elems.append(try self.convert(list_repr.*, elem)) catch oom();
                    }
                },
                .map => |map| {
                    for (0..map.entries.count()) |ix| {
                        const elem = map.entries.get(.{ .i64 = @intCast(ix) }) orelse
                            return self.fail("Cannot convert {} to {}", .{ value, repr });
                        elems.append(try self.convert(list_repr.*, elem)) catch oom();
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
                        ) catch oom();
                    }
                },
                .list => |list| {
                    for (0.., list.elems.items) |ix, elem| {
                        entries.putNoClobber(
                            try self.convert(map_repr[0].*, .{ .i64 = @intCast(ix) }),
                            try self.convert(map_repr[1].*, elem),
                        ) catch oom();
                    }
                },
                .map => |map| {
                    var iter = map.entries.iterator();
                    while (iter.next()) |entry| {
                        entries.putNoClobber(
                            try self.convert(map_repr[0].*, entry.key_ptr.*),
                            try self.convert(map_repr[1].*, entry.value_ptr.*),
                        ) catch oom();
                    }
                },
                else => return self.fail("Cannot convert {} to {}", .{ value, repr }),
            }
            return .{ .map = .{ .repr = map_repr, .entries = entries } };
        },
        .@"union" => |union_repr| {
            for (0.., union_repr.keys, union_repr.reprs) |tag, key, value_repr| {
                if (value == .@"struct" and
                    value.@"struct".repr.keys.len == 1 and
                    value.@"struct".repr.keys[0].equal(key))
                {
                    const value_new = try self.convert(value_repr, value.@"struct".values[0]);
                    return .{ .@"union" = .{
                        .repr = union_repr,
                        .tag = tag,
                        .value = box(self.allocator, value_new),
                    } };
                }
            }
            return self.fail("Cannot convert {} to {}", .{ value, repr });
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

fn matchObject(self: *Self, pattern: ObjectPattern, object: Value) !void {
    switch (object) {
        .@"struct" => |@"struct"| {
            const start = self.scope.items.len;
            errdefer self.scope.shrinkRetainingCapacity(start);

            if (pattern.keys.len != @"struct".repr.keys.len)
                return self.fail("Wrong number of keys in {} to match object pattern", .{object});

            for (pattern.muts, pattern.keys, pattern.values, @"struct".repr.keys, @"struct".values) |mut, pattern_key, pattern_value, object_key, object_value| {
                if (!pattern_key.equal(object_key))
                    return self.fail("Key {} in pattern not found in object {}", .{ pattern_key, object });
                self.scope.append(.{
                    .mut = mut,
                    .call_id = null,
                    .name = pattern_value,
                    .value = object_value,
                }) catch oom();
            }
        },
        .map, .list => return self.fail("TODO match object/list", .{}),
        else => return self.fail("Cannot match {} against object pattern", .{object}),
    }
}

fn fail(self: *Self, comptime message: []const u8, args: anytype) error{SemantalyzeError} {
    self.error_message = std.fmt.allocPrint(self.allocator, message, args) catch oom();
    return error.SemantalyzeError;
}

fn box(allocator: Allocator, value: anytype) *@TypeOf(value) {
    const value_ptr = allocator.create(@TypeOf(value)) catch oom();
    value_ptr.* = value;
    return value_ptr;
}

fn permute(allocator: Allocator, ixes: []const usize, comptime T: type, things: []T) []T {
    const things_copy = allocator.dupe(T, things) catch oom();
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
