const std = @import("std");
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const panic = std.debug.panic;
const assert = std.debug.assert;

const zest = @import("../lib/zest.zig");
const Compiler = zest.Compiler;
const oom = zest.oom;

export fn LLVMFuzzerTestOneInput(buf: [*]const u8, len: usize) void {
    if (len > 0 and buf[0] == 'x') panic("Oh no", .{});
}
