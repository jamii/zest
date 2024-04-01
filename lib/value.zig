const std = @import("std");
const panic = std.debug.panic;
const assert = std.debug.assert;
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const wasm = std.wasm;

const zest = @import("./zest.zig");
const oom = zest.oom;
const Repr = zest.Repr;

pub const Value = union(enum) {
    i32: i32,

    pub fn reprOf(value: Value, allocator: Allocator) Repr {
        _ = allocator;
        switch (value) {
            .i32 => return Repr.i32,
        }
    }

    pub fn emptyStruct() Value {
        // TODO
        return .{ .i32 = 0 };
    }
};
