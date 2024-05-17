const std = @import("std");
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const panic = std.debug.panic;
const assert = std.debug.assert;

const zest = @import("../lib/zest.zig");
const Compiler = zest.Compiler;
const oom = zest.oom;
const evalLax = @import("../lib/test.zig").evalLax;
const evalStrict = @import("../lib/test.zig").evalStrict;

export fn LLVMFuzzerTestOneInput(buf: [*]const u8, len: usize) c_int {
    var arena = std.heap.ArenaAllocator.init(std.heap.c_allocator);
    defer arena.deinit();

    const allocator = arena.allocator();

    const source = buf[0..len];

    var compiler = Compiler.init(allocator, source);
    const actual_lax = std.mem.trim(u8, evalLax(allocator, &compiler) catch zest.formatError(&compiler), "\n");

    compiler = Compiler.init(allocator, source);
    const actual_strict = std.mem.trim(u8, evalStrict(allocator, &compiler) catch zest.formatError(&compiler), "\n");

    if (!std.mem.eql(u8, actual_lax, actual_strict))
        panic(
            \\--- lax ---
            \\{s}
            \\--- strict ---
            \\{s}
        , .{ actual_lax, actual_strict });

    return 0;
}
