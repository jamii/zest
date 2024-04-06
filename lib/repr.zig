const zest = @import("./zest.zig");
const Value = zest.Value;

pub const Repr = union(enum) {
    i32,
    @"struct": ReprStruct,
    @"union": ReprUnion,

    pub fn emptyStruct() Repr {
        return .{ .@"struct" = .{
            .keys = &.{},
            .reprs = &.{},
        } };
    }

    pub fn emptyUnion() Repr {
        return .{ .@"union" = .{
            .keys = &.{},
            .reprs = &.{},
        } };
    }

    pub fn isEmptyUnion(self: Repr) bool {
        return (self == .@"union" and self.@"union".keys.len == 0);
    }
};

pub const ReprStruct = struct {
    keys: []Value,
    reprs: []Repr,

    pub fn init(keys: []Value, reprs: []Repr) ReprStruct {
        // TODO sort
        return .{ .keys = keys, .reprs = reprs };
    }
};

pub const ReprUnion = struct {
    keys: []Value,
    reprs: []Repr,

    pub fn init(keys: []Value, reprs: []Repr) ReprUnion {
        // TODO sort
        return .{ .keys = keys, .reprs = reprs };
    }
};
