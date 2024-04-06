const zest = @import("./zest.zig");
const Value = zest.Value;

pub const Repr = union(enum) {
    i32,
    allOf: ReprAllOf,

    pub fn always() Repr {
        return .{ .allOf = .{
            .keys = &.{},
            .reprs = &.{},
        } };
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
