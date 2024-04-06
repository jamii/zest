const zest = @import("./zest.zig");
const Value = zest.Value;

pub const Repr = union(enum) {
    i32,
    allOf: ReprAllOf,
    oneOf: ReprOneOf,

    pub fn always() Repr {
        return .{ .allOf = .{
            .keys = &.{},
            .reprs = &.{},
        } };
    }

    pub fn never() Repr {
        return .{ .oneOf = .{
            .keys = &.{},
            .reprs = &.{},
        } };
    }

    pub fn isNever(self: Repr) bool {
        return (self == .oneOf and self.oneOf.keys.len == 0);
    }
};

pub const ReprAllOf = struct {
    keys: []Value,
    reprs: []Repr,

    pub fn init(keys: []Value, reprs: []Repr) ReprAllOf {
        // TODO sort
        return .{ .keys = keys, .reprs = reprs };
    }
};

pub const ReprOneOf = struct {
    keys: []Value,
    reprs: []Repr,

    pub fn init(keys: []Value, reprs: []Repr) ReprOneOf {
        // TODO sort
        return .{ .keys = keys, .reprs = reprs };
    }
};
