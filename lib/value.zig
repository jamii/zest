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
const deepEqual = zest.deepEqual;

pub const Value = union(enum) {
    i32: i32,
    string: []const u8,
    @"struct": ValueStruct,
    @"union": ValueUnion,
    fun: ValueFun,
    only: *Value,
    repr: Repr,

    pub fn reprOf(value: Value) Repr {
        switch (value) {
            .i32 => return .i32,
            .string => return .string,
            .@"struct" => |@"struct"| return .{ .@"struct" = @"struct".repr },
            .@"union" => |@"union"| return .{ .@"union" = @"union".repr },
            .only => |only| return .{ .only = only },
            .fun => |fun| return .{ .fun = fun.repr },
            .repr => return .repr,
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
            .i32, .string, .repr, .fun, .only => null,
            .@"struct" => |@"struct"| @"struct".get(key),
            .@"union" => panic("TODO", .{}),
        };
    }

    pub fn format(self: Value, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = fmt;
        _ = options;
        switch (self) {
            .i32 => |i| try writer.print("{}", .{i}),
            .string => |string| try writer.print("'{s}'", .{string}), // TODO escape
            .@"struct" => |@"struct"| {
                try writer.writeAll("[");
                var positional = true;
                for (@"struct".repr.keys, @"struct".values, 0..) |key, value, i| {
                    if (i != 0) {
                        try writer.writeAll(", ");
                    }
                    if (positional and key == .i32 and key.i32 == i) {
                        try writer.print("{}", .{value});
                    } else {
                        positional = false;
                        try writer.print("{}: {}", .{ key, value });
                    }
                }
                try writer.writeAll("]");
            },
            else => try writer.print("TODO {}", .{std.meta.activeTag(self)}),
        }
    }
};

pub const ValueStruct = struct {
    repr: ReprStruct,
    values: []Value,

    pub fn get(self: ValueStruct, key: Value) ?Value {
        return if (self.repr.get(key)) |i| self.values[i] else null;
    }

    // TODO init function that sorts
};

pub const ValueUnion = struct {
    repr: ReprUnion,
    tag: usize,
    value: *Value,
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
