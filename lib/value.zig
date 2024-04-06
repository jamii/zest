const std = @import("std");
const panic = std.debug.panic;
const assert = std.debug.assert;
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const wasm = std.wasm;

const zest = @import("./zest.zig");
const oom = zest.oom;
const Repr = zest.Repr;
const ReprAllOf = zest.ReprAllOf;
const ReprOneOf = zest.ReprOneOf;

pub const Value = union(enum) {
    i32: i32,
    allOf: ValueAllOf,
    oneOf: ValueOneOf,

    pub fn reprOf(value: Value) Repr {
        switch (value) {
            .i32 => return .i32,
            .allOf => |allOf| return .{ .allOf = allOf.repr },
            .oneOf => |oneOf| return .{ .oneOf = oneOf.repr },
        }
    }

    pub fn always() Value {
        return .{ .allOf = .{
            .repr = Repr.always().allOf,
            .values = &.{},
        } };
    }
};

pub const ValueAllOf = struct {
    repr: ReprAllOf,
    values: []Value,
};

pub const ValueOneOf = struct {
    repr: ReprOneOf,
    tag: usize,
    value: *Value,
};
