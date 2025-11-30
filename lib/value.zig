const std = @import("std");
const panic = std.debug.panic;
const assert = std.debug.assert;
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const wasm = std.wasm;

const zest = @import("./zest.zig");
const oom = zest.oom;
const Repr = zest.Repr;
const ReprStruct = zest.ReprStruct;
const ReprUnion = zest.ReprUnion;
const ReprList = zest.ReprList;
const ReprFun = zest.ReprFun;
const ReprKind = zest.ReprKind;
const deepEqual = zest.deepEqual;
const isName = zest.isName;
const dir = zest.dir;

pub const Value = union(enum) {
    u32: u32,
    i64: i64,
    string: []const u8,
    @"struct": ValueStruct,
    @"union": ValueUnion,
    list: ValueList,
    fun: ValueFun,
    namespace: ValueNamespace,
    only: *Value,
    ref: ValueRef,
    repr: Repr,
    repr_kind: ReprKind,

    pub fn reprOf(value: Value) Repr {
        return switch (value) {
            .u32 => .u32,
            .i64 => .i64,
            .string => .string,
            .@"struct" => |@"struct"| .{ .@"struct" = @"struct".repr },
            .@"union" => |@"union"| .{ .@"union" = @"union".repr },
            .list => |list| .{ .list = list.repr },
            .only => |only| .{ .only = only },
            .fun => |fun| .{ .fun = fun.repr },
            .namespace => |namespace| .{ .namespace = .{ .namespace = namespace.namespace } },
            .ref => |ref| .{ .ref = ref.repr },
            .repr => .repr,
            .repr_kind => .repr_kind,
        };
    }

    pub fn emptyStruct() Value {
        return .{ .@"struct" = .{
            .repr = Repr.emptyStruct().@"struct",
            .values = &.{},
        } };
    }

    pub fn equal(self: Value, other: Value) bool {
        // TODO May need to think about this.
        return deepEqual(self, other);
    }

    pub fn get(self: Value, key: Value) ?Value {
        return switch (self) {
            .u32, .i64, .string, .fun, .only, .ref, .repr, .repr_kind, .namespace => null,
            .@"struct" => |@"struct"| @"struct".get(key),
            .@"union" => |@"union"| @"union".get(key),
            .list => |list| list.get(key),
        };
    }

    pub fn getMut(self: *Value, key: Value) ?*Value {
        return switch (self.*) {
            .u32, .i64, .string, .fun, .only, .ref, .repr, .repr_kind, .namespace => null,
            .@"struct" => |*@"struct"| @"struct".getMut(key),
            .@"union" => |*@"union"| @"union".getMut(key),
            .list => |*list| list.getMut(key),
        };
    }

    pub fn format(self: Value, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = fmt;
        _ = options;
        switch (self) {
            .u32 => |i| try writer.print("{}/u32", .{i}),
            .i64 => |i| try writer.print("{}", .{i}),
            .string => |string| try writer.print("'{'}'", .{std.zig.fmtEscapes(string)}),
            .@"struct" => |@"struct"| {
                try writer.writeAll("[");
                var positional = true;
                for (@"struct".repr.keys, @"struct".values, 0..) |key, value, i| {
                    if (i != 0) {
                        try writer.writeAll(", ");
                    }
                    if (positional and key == .i64 and key.i64 == i) {
                        try writer.print("{}", .{value});
                    } else {
                        positional = false;
                        try writer.print("{}: {}", .{ FormatKey{ .key = key }, value });
                    }
                }
                try writer.writeAll("]");
            },
            .@"union" => |@"union"| {
                try writer.print("[{}: {}]/{}", .{
                    FormatKey{ .key = @"union".repr.keys[@"union".tag] },
                    @"union".value.*,
                    self.reprOf(),
                });
            },
            .list => |list| {
                try writer.writeAll("[");
                for (list.elems.items, 0..) |elem, i| {
                    if (i != 0) {
                        try writer.writeAll(", ");
                    }
                    try writer.print("{}", .{elem});
                }
                try writer.print("]/{}", .{self.reprOf()});
            },
            .fun => |fun| {
                try writer.print("{}/{}", .{
                    Value{ .@"struct" = .{ .repr = fun.repr.closure, .values = fun.closure } },
                    self.reprOf(),
                });
            },
            .namespace => {
                try writer.print("[]/{}", .{
                    self.reprOf(),
                });
            },
            .ref => |ref| {
                try writer.print("{}/{}", .{
                    ref.value.*,
                    self.reprOf(),
                });
            },
            inline .repr, .repr_kind => |data| {
                try writer.print("{}", .{data});
            },
            .only => {
                try writer.print("[]/{}", .{self.reprOf()});
            },
        }
    }

    pub fn copy(self: Value, allocator: Allocator) Value {
        return switch (self) {
            .u32, .i64, .repr, .repr_kind, .namespace => self,
            .string => |string| .{
                .string = allocator.dupe(u8, string) catch oom(),
            },
            .@"struct" => |@"struct"| .{ .@"struct" = .{
                .repr = @"struct".repr,
                .values = Value.copySlice(@"struct".values, allocator),
            } },
            .@"union" => |@"union"| .{ .@"union" = .{
                .repr = @"union".repr,
                .tag = @"union".tag,
                .value = Value.copyBox(@"union".value, allocator),
            } },
            .list => |list| .{ .list = .{
                .repr = list.repr,
                .elems = Value.copyArrayList(list.elems, allocator),
            } },
            .only => |only| .{
                .only = Value.copyBox(only, allocator),
            },
            .fun => |fun| .{ .fun = .{
                .repr = fun.repr,
                .closure = Value.copySlice(fun.closure, allocator),
            } },
            .ref => |ref| .{ .ref = .{
                .repr = ref.repr,
                .value = Value.copyBox(ref.value, allocator),
            } },
        };
    }

    pub fn copyBox(self: *Value, allocator: Allocator) *Value {
        const box = allocator.create(Value) catch oom();
        box.* = self.*.copy(allocator);
        return box;
    }

    pub fn copySlice(self: []Value, allocator: Allocator) []Value {
        const values = allocator.alloc(Value, self.len) catch oom();
        for (values, self) |*value, old_value| value.* = old_value.copy(allocator);
        return values;
    }

    pub fn copyArrayList(self: ArrayList(Value), allocator: Allocator) ArrayList(Value) {
        var values = ArrayList(Value).initCapacity(allocator, self.items.len) catch oom();
        _ = values.addManyAsSliceAssumeCapacity(self.items.len);
        for (values.items, self.items) |*value, old_value| value.* = old_value.copy(allocator);
        return values;
    }

    pub fn load(allocator: Allocator, bytes: []const u8, repr: Repr) Value {
        switch (repr) {
            .u32 => {
                return .{ .u32 = std.mem.readInt(u32, bytes[0..@sizeOf(u32)], .little) };
            },
            .i64 => {
                return .{ .i64 = std.mem.readInt(i64, bytes[0..@sizeOf(i64)], .little) };
            },
            .@"struct" => |@"struct"| {
                const values = allocator.alloc(Value, @"struct".keys.len) catch oom();
                var offset: usize = 0;
                for (@"struct".reprs, values) |value_repr, *value| {
                    value.* = load(allocator, bytes[offset..], value_repr);
                    offset += value_repr.sizeOf();
                }
                return .{ .@"struct" = .{ .repr = @"struct", .values = values } };
            },
            .string, .@"union", .list, .only, .fun, .ref, .repr, .repr_kind, .namespace => panic("TODO load: {}", .{repr}),
        }
    }

    pub fn store(self: Value, bytes: []u8) void {
        switch (self) {
            .u32 => |i| {
                std.mem.writeInt(u32, bytes[0..@sizeOf(u32)], i, .little);
            },
            .i64 => |i| {
                std.mem.writeInt(i64, bytes[0..@sizeOf(i64)], i, .little);
            },
            .@"struct" => |@"struct"| {
                var offset: usize = 0;
                for (@"struct".repr.reprs, @"struct".values) |value_repr, value| {
                    value.store(bytes[offset..]);
                    offset += value_repr.sizeOf();
                }
            },
            .string, .@"union", .list, .only, .fun, .ref, .repr, .repr_kind, .namespace => panic("TODO store: {}", .{self}),
        }
    }

    pub fn asBool(self: Value) ?bool {
        return switch (self) {
            .i64 => |i| switch (i) {
                0 => false,
                1 => true,
                else => null,
            },
            .only => |value| value.asBool(),
            else => null,
        };
    }
};

pub const ValueStruct = struct {
    repr: ReprStruct,
    values: []Value,

    pub fn get(self: ValueStruct, key: Value) ?Value {
        return if (self.repr.get(key)) |i| self.values[i] else null;
    }

    pub fn getMut(self: *ValueStruct, key: Value) ?*Value {
        return if (self.repr.get(key)) |i| &self.values[i] else null;
    }
};

pub const ValueUnion = struct {
    repr: ReprUnion,
    tag: usize,
    value: *Value,

    pub fn get(self: ValueUnion, key: Value) ?Value {
        return if (self.repr.get(key)) |i| if (self.tag == i) self.value.* else null else null;
    }

    pub fn getMut(self: *ValueUnion, key: Value) ?*Value {
        return if (self.repr.get(key)) |i| if (self.tag == i) self.value else null else null;
    }
};

pub const ValueList = struct {
    repr: ReprList,
    elems: ArrayList(Value),

    pub fn get(self: ValueList, key: Value) ?Value {
        return if (key == .i64 and key.i64 >= 0 and key.i64 < self.elems.items.len)
            self.elems.items[@intCast(key.i64)]
        else
            null;
    }

    pub fn getMut(self: *ValueList, key: Value) ?*Value {
        return if (key == .i64 and key.i64 >= 0 and key.i64 < self.elems.items.len)
            &self.elems.items[@intCast(key.i64)]
        else
            null;
    }

    pub fn deepEqual(self: ValueList, other: ValueList) bool {
        if (!zest.deepEqual(self.repr, other.repr)) return false;
        if (self.elems.items.len != other.elems.items.len) return false;
        for (self.elems.items, other.elems.items) |self_elem, other_elem|
            if (!zest.deepEqual(self_elem, other_elem)) return false;
        return true;
    }

    pub fn deepHashInto(hasher: anytype, self: ValueList) void {
        zest.deepHashInto(hasher, self.repr);
        zest.deepHashInto(hasher, self.elems.items);
    }
};

pub const ValueFun = struct {
    repr: ReprFun,
    closure: []Value,

    pub fn getClosure(fun: ValueFun) ValueStruct {
        return .{
            .repr = fun.repr.closure,
            .values = fun.closure,
        };
    }
};

pub const ValueNamespace = struct {
    namespace: dir.Namespace,
};

pub const ValueRef = struct {
    repr: *Repr,
    value: *Value,
};

pub const FormatKey = struct {
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
