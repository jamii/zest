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
const ReprFun = zest.ReprFun;
const ReprKind = zest.ReprKind;
const deepEqual = zest.deepEqual;

pub const Value = union(enum) {
    u32: u32,
    i64: i64,
    string: []const u8,
    @"struct": ValueStruct,
    @"union": ValueUnion,
    fun: ValueFun,
    only: *Value,
    ref: ValueRef,
    repr: Repr,
    repr_kind: ReprKind,

    pub fn reprOf(value: Value) Repr {
        switch (value) {
            .u32 => return .u32,
            .i64 => return .i64,
            .string => return .string,
            .@"struct" => |@"struct"| return .{ .@"struct" = @"struct".repr },
            .@"union" => |@"union"| return .{ .@"union" = @"union".repr },
            .only => |only| return .{ .only = only },
            .fun => |fun| return .{ .fun = fun.repr },
            .ref => |ref| return .{ .ref = ref.repr },
            .repr => return .repr,
            .repr_kind => return .repr_kind,
        }
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
            .u32, .i64, .string, .fun, .only, .ref, .repr, .repr_kind => null,
            .@"struct" => |@"struct"| @"struct".get(key),
            .@"union" => |@"union"| @"union".get(key),
        };
    }

    pub fn getMut(self: *Value, key: Value) ?*Value {
        return switch (self.*) {
            .u32, .i64, .string, .fun, .only, .ref, .repr, .repr_kind => null,
            .@"struct" => |*@"struct"| @"struct".getMut(key),
            .@"union" => |*@"union"| @"union".getMut(key),
        };
    }

    pub fn format(self: Value, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = fmt;
        _ = options;
        switch (self) {
            // TODO Need to adjust test.js to print this.
            //.u32 => |i| try writer.print("u32[{}]", .{i}),
            inline .u32, .i64 => |i| try writer.print("{}", .{i}),
            .string => |string| try writer.print("'{s}'", .{string}), // TODO escape
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
                        try writer.print("{}: {}", .{ key, value });
                    }
                }
                try writer.writeAll("]");
            },
            .@"union" => |@"union"| {
                try writer.print("{}[[{}: {}]]", .{
                    Repr{ .@"union" = @"union".repr },
                    @"union".repr.keys[@"union".tag],
                    @"union".value.*,
                });
            },
            .fun => |fun| {
                try writer.print("{}{}", .{ self.reprOf(), Value{ .@"struct" = .{ .repr = fun.repr.closure, .values = fun.closure } } });
            },
            .ref => |ref| {
                try writer.print("{}[{}]", .{ self.reprOf(), ref.value.* });
            },
            inline .repr, .repr_kind => |data| {
                try writer.print("{}", .{data});
            },
            .only => |only| {
                try writer.print("only[{}][]", .{only});
            },
        }
    }

    pub fn copy(self: Value, allocator: Allocator) Value {
        return switch (self) {
            .u32, .i64 => self,
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
            .repr, .repr_kind => self,
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
            .string, .@"union", .only, .fun, .ref, .repr, .repr_kind => panic("TODO load: {}", .{repr}),
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
            .string, .@"union", .only, .fun, .ref, .repr, .repr_kind => panic("TODO store: {}", .{self}),
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

pub const ValueRef = struct {
    repr: *Repr,
    value: *Value,
};
