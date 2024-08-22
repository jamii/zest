const std = @import("std");
const panic = std.debug.panic;

const zest = @import("./zest.zig");
const Value = zest.Value;
const ValueStruct = zest.ValueStruct;
const deepEqual = zest.deepEqual;
const dir = zest.dir;

pub const Repr = union(enum) {
    u32,
    i64,
    string,
    @"struct": ReprStruct,
    @"union": ReprUnion,
    fun: ReprFun,
    only: *Value,
    repr,
    repr_kind,
    ref: *Repr,

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

    pub fn isEmptyStruct(self: Repr) bool {
        return (self == .@"struct" and self.@"struct".keys.len == 0);
    }

    pub fn isEmptyUnion(self: Repr) bool {
        return (self == .@"union" and self.@"union".keys.len == 0);
    }

    pub fn equal(self: Repr, other: Repr) bool {
        // TODO May need to think about this.
        return deepEqual(self, other);
    }

    pub fn sizeOf(self: Repr) usize {
        return switch (self) {
            .u32 => 4,
            .i64 => 8,
            .string => 8, // struct[ptr: u32, len: u32]
            .@"struct" => |@"struct"| @"struct".sizeOf(),
            .@"union" => |@"union"| @"union".sizeOf(),
            .fun => |fun| fun.sizeOf(),
            .only => 0,
            .ref => 4,
            .repr, .repr_kind => panic("TODO {}", .{self}),
        };
    }

    pub fn valueOf(self: Repr) ?Value {
        return switch (self) {
            .only => |value| value.*,
            .@"struct" => |@"struct"| if (@"struct".valueOf()) |value|
                .{ .@"struct" = value }
            else
                null,
            .fun => |fun| if (fun.closure.valueOf()) |closure|
                .{ .fun = .{ .repr = fun, .closure = closure.values } }
            else
                null,
            .u32, .i64, .string, .@"union", .ref, .repr, .repr_kind => null,
        };
    }

    pub fn format(self: Repr, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = fmt;
        _ = options;
        try writer.print("{s}", .{@tagName(self)});
        switch (self) {
            .u32, .i64, .string, .repr, .repr_kind => {},
            .@"struct" => |@"struct"| {
                try writer.writeAll("[");
                var positional = true;
                for (@"struct".keys, @"struct".reprs, 0..) |key, repr, i| {
                    if (i != 0) {
                        try writer.writeAll(", ");
                    }
                    if (positional and key == .i64 and key.i64 == i) {
                        try writer.print("{}", .{repr});
                    } else {
                        positional = false;
                        try writer.print("{}: {}", .{ key, repr });
                    }
                }
                try writer.writeAll("]");
            },
            .@"union" => |@"union"| {
                try writer.writeAll("[");
                var positional = true;
                for (@"union".keys, @"union".reprs, 0..) |key, repr, i| {
                    if (i != 0) {
                        try writer.writeAll(", ");
                    }
                    if (positional and key == .i64 and key.i64 == i) {
                        try writer.print("{}", .{repr});
                    } else {
                        positional = false;
                        try writer.print("{}: {}", .{ key, repr });
                    }
                }
                try writer.writeAll("]");
            },
            .fun => |fun| {
                try writer.print("[id: {}, closure: {}]", .{
                    fun.fun.id,
                    Repr{ .@"struct" = fun.closure },
                });
            },
            .ref => |ref| {
                try writer.print("[{}]", .{ref.*});
            },
            else => try writer.print("TODO {}", .{std.meta.activeTag(self)}),
        }
    }

    pub fn hasRef(self: Repr, kind: enum { any, visible }) bool {
        return switch (self) {
            .u32, .i64, .string => {
                return false;
            },
            .@"struct" => |@"struct"| {
                for (@"struct".keys) |key| {
                    if (key.reprOf().hasRef(kind)) return true;
                }
                for (@"struct".reprs) |repr| {
                    if (repr.hasRef(kind)) return true;
                }
                return false;
            },
            .@"union" => |@"union"| {
                for (@"union".keys) |key| {
                    if (key.reprOf().hasRef(kind)) return true;
                }
                for (@"union".reprs) |repr| {
                    if (repr.hasRef(kind)) return true;
                }
                return false;
            },
            .fun => |fun| {
                switch (kind) {
                    .any => return (Repr{ .@"struct" = fun.closure }).hasRef(kind),
                    .visible => return false,
                }
            },
            .only => |only| {
                return only.reprOf().hasRef(kind);
            },
            .ref => {
                return true;
            },
            .repr, .repr_kind => {
                return false;
            },
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

    pub fn valueOf(self: ReprStruct) ?ValueStruct {
        return if (self.keys.len == 0)
            .{ .repr = self, .values = &.{} }
        else
            null;
    }

    pub fn get(self: ReprStruct, key: Value) ?usize {
        for (0.., self.keys) |i, self_key| {
            if (key.equal(self_key)) return i;
        }
        return null;
    }

    pub fn offsetOf(self: ReprStruct, i: usize) usize {
        var offset: usize = 0;
        for (self.reprs[0..i]) |repr| {
            offset += repr.sizeOf();
        }
        return offset;
    }
};

pub const ReprUnion = struct {
    keys: []Value,
    reprs: []Repr,

    pub fn init(keys: []Value, reprs: []Repr) ReprUnion {
        // TODO sort
        return .{ .keys = keys, .reprs = reprs };
    }

    pub fn tagSizeOf(self: ReprUnion) u32 {
        _ = self;
        // A u32 tag.
        // TODO Switch tag size depending on number of keys.
        return @sizeOf(u32);
    }

    pub fn sizeOf(self: ReprUnion) usize {
        var size: usize = 0;
        for (self.reprs) |repr| size = @max(size, repr.sizeOf());
        size += self.tagSizeOf();
        return size;
    }

    pub fn get(self: ReprUnion, key: Value) ?usize {
        for (0.., self.keys) |i, self_key| {
            if (key.equal(self_key)) return i;
        }
        return null;
    }
};

pub const ReprFun = struct {
    fun: dir.Fun,
    closure: ReprStruct,

    pub fn sizeOf(self: ReprFun) usize {
        return self.closure.sizeOf();
    }
};

pub const ReprKind = enum {
    @"struct",
    @"union",
    only,

    pub fn format(self: ReprKind, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = fmt;
        _ = options;
        try writer.print("{s}", .{@tagName(self)});
    }
};
