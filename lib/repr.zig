const std = @import("std");
const panic = std.debug.panic;

const zest = @import("./zest.zig");
const Value = zest.Value;
const DirFun = zest.DirFun;

pub const Repr = union(enum) {
    i32,
    string,
    @"struct": ReprStruct,
    @"union": ReprUnion,
    fun: ReprFun,
    repr,

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

    pub fn sizeOf(self: Repr) usize {
        return switch (self) {
            .i32 => 4,
            .string => panic("TODO {}", .{self}),
            .@"struct" => |@"struct"| @"struct".sizeOf(),
            .@"union" => |@"union"| @"union".sizeOf(),
            .repr => panic("TODO {}", .{self}),
        };
    }
};

pub const ReprStruct = struct {
    keys: []Value,
    reprs: []Repr,

    pub fn init(keys: []Value, reprs: []Repr) ReprStruct {
        // TODO sort
        return .{ .keys = keys, .reprs = reprs };
    }

    pub fn sizeOf(self: ReprStruct) usize {
        var size: usize = 0;
        for (self.reprs) |repr| size += repr.sizeOf();
        return size;
    }

    pub fn get(self: ReprStruct, key: Value) ?usize {
        for (0.., self.keys) |i, self_key| {
            if (key.equal(self_key)) return i;
        }
        return null;
    }
};

pub const ReprUnion = struct {
    keys: []Value,
    reprs: []Repr,

    pub fn init(keys: []Value, reprs: []Repr) ReprUnion {
        // TODO sort
        return .{ .keys = keys, .reprs = reprs };
    }

    pub fn sizeOf(self: ReprUnion) usize {
        var size: usize = 0;
        for (self.reprs) |repr| size = @max(size, repr.sizeOf());
        size += 4; // An i32 tag.
        return size;
    }
};

pub const ReprFun = struct {
    fun: DirFun,
};
