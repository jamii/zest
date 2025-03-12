const std = @import("std");
const panic = std.debug.panic;
const assert = std.debug.assert;
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;

pub fn deepEqual(a: anytype, b: @TypeOf(a)) bool {
    const T = @TypeOf(a);
    switch (@typeInfo(T)) {
        .void => return true,
        .int, .@"enum" => return a == b,
        .@"struct" => |info| {
            inline for (info.fields) |field| {
                if (!deepEqual(@field(a, field.name), @field(b, field.name)))
                    return false;
            }
            return true;
        },
        .pointer => |info| {
            switch (info.size) {
                .one => return deepEqual(a.*, b.*),
                .slice => {
                    if (a.len != b.len) return false;
                    for (a, b) |a_item, b_item| {
                        if (!deepEqual(a_item, b_item))
                            return false;
                    }
                    return true;
                },
                .many, .c => @compileError(@typeName(T)),
            }
        },
        .@"union" => |info| {
            if (info.tag_type == null) @compileError(@typeName(T));
            const Tag = info.tag_type.?;
            const a_tag = @as(Tag, a);
            const b_tag = @as(Tag, b);
            if (!deepEqual(a_tag, b_tag))
                return false;
            inline for (@typeInfo(Tag).@"enum".fields) |field| {
                if (a_tag == @as(Tag, @enumFromInt(field.value)))
                    return deepEqual(@field(a, field.name), @field(b, field.name));
            }
            unreachable;
        },
        .optional => {
            if (a == null) return b == null;
            if (b == null) return false;
            return deepEqual(a.?, b.?);
        },
        else => @compileError(@typeName(T)),
    }
}

pub fn deepHash(key: anytype) u64 {
    var hasher = std.hash.Wyhash.init(0);
    deepHashInto(&hasher, key);
    return hasher.final();
}

pub fn deepHashInto(hasher: anytype, key: anytype) void {
    const T = @TypeOf(key);
    switch (@typeInfo(T)) {
        .void => {},
        .int, .@"enum" => hasher.update(std.mem.asBytes(&key)),
        .@"struct" => |info| {
            inline for (info.fields) |field| {
                deepHashInto(hasher, @field(key, field.name));
            }
        },
        .pointer => |info| {
            switch (info.size) {
                .one => deepHashInto(hasher, key.*),
                .slice => {
                    deepHashInto(hasher, key.len);
                    for (key) |key_item| {
                        deepHashInto(hasher, key_item);
                    }
                },
                .many, .c => @compileError(@typeName(T)),
            }
        },
        .@"union" => |info| {
            if (info.tag_type == null) @compileError(@typeName(T));
            const Tag = info.tag_type.?;
            const key_tag = @as(Tag, key);
            deepHashInto(hasher, key_tag);
            inline for (@typeInfo(Tag).@"enum".fields) |field| {
                if (key_tag == @as(Tag, @enumFromInt(field.value)))
                    return deepHashInto(hasher, @field(key, field.name));
            }
            unreachable;
        },
        else => @compileError(@typeName(T)),
    }
}
