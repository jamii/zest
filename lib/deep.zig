const std = @import("std");
const panic = std.debug.panic;
const assert = std.debug.assert;
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;

pub fn deepEqual(a: anytype, b: @TypeOf(a)) bool {
    const T = @TypeOf(a);
    switch (@typeInfo(T)) {
        .Void => return true,
        .Int, .Enum => return a == b,
        .Struct => |info| {
            inline for (info.fields) |field| {
                if (!deepEqual(@field(a, field.name), @field(b, field.name)))
                    return false;
            }
            return true;
        },
        .Pointer => |info| {
            switch (info.size) {
                .One => return deepEqual(a.*, b.*),
                .Slice => {
                    if (a.len != b.len) return false;
                    for (a, b) |a_item, b_item| {
                        if (!deepEqual(a_item, b_item))
                            return false;
                    }
                    return true;
                },
                .Many, .C => @compileError(@typeName(T)),
            }
        },
        .Union => |info| {
            if (info.tag_type == null) @compileError(@typeName(T));
            const Tag = info.tag_type.?;
            const a_tag = @as(Tag, a);
            const b_tag = @as(Tag, b);
            if (!deepEqual(a_tag, b_tag))
                return false;
            inline for (@typeInfo(Tag).Enum.fields) |field| {
                if (a_tag == @as(Tag, @enumFromInt(field.value)))
                    return deepEqual(@field(a, field.name), @field(b, field.name));
            }
            unreachable;
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
        .Void => {},
        .Int, .Enum => hasher.update(std.mem.asBytes(&key)),
        .Struct => |info| {
            inline for (info.fields) |field| {
                deepHashInto(hasher, @field(key, field.name));
            }
        },
        .Pointer => |info| {
            switch (info.size) {
                .One => deepHashInto(hasher, key.*),
                .Slice => {
                    deepHashInto(hasher, key.len);
                    for (key) |key_item| {
                        deepHashInto(hasher, key_item);
                    }
                },
                .Many, .C => @compileError(@typeName(T)),
            }
        },
        .Union => |info| {
            if (info.tag_type == null) @compileError(@typeName(T));
            const Tag = info.tag_type.?;
            const key_tag = @as(Tag, key);
            deepHashInto(hasher, key_tag);
            inline for (@typeInfo(Tag).Enum.fields) |field| {
                if (key_tag == @as(Tag, @enumFromInt(field.value)))
                    return deepHashInto(hasher, @field(key, field.name));
            }
            unreachable;
        },
        else => @compileError(@typeName(T)),
    }
}
