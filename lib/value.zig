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
const deepEqual = zest.deepEqual;

pub const Value = union(enum) {
    i32: i32,
    string: []const u8,
    @"struct": ValueStruct,
    @"union": ValueUnion,
    repr: Repr,

    pub fn reprOf(value: Value) Repr {
        switch (value) {
            .i32 => return .i32,
            .string => return .string,
            .@"struct" => |@"struct"| return .{ .@"struct" = @"struct".repr },
            .@"union" => |@"union"| return .{ .@"union" = @"union".repr },
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
};

pub const ValueStruct = struct {
    repr: ReprStruct,
    values: []Value,
};

pub const ValueUnion = struct {
    repr: ReprUnion,
    tag: usize,
    value: *Value,
};
