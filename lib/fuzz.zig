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
const readWat = @import("../lib/test.zig").readWat;

pub fn fuzz(source: []const u8) void {
    var arena = std.heap.ArenaAllocator.init(std.heap.c_allocator);
    defer arena.deinit();

    const allocator = arena.allocator();

    var compiler = Compiler.init(allocator, source);
    const actual_lax = std.mem.trim(u8, evalLax(allocator, &compiler) catch zest.formatError(&compiler), "\n");

    compiler = Compiler.init(allocator, source);
    const actual_strict = std.mem.trim(u8, evalStrict(allocator, &compiler) catch zest.formatError(&compiler), "\n");

    // Currently can only print i32 from strict - anything else will print 'undefined'.
    if (std.fmt.parseInt(i32, actual_strict, 10)) |_| {
        if (!std.mem.eql(u8, actual_lax, actual_strict)) {
            std.debug.print(
                \\--- wat ---
                \\ {s}
                \\
            , .{readWat(allocator)});
            panic(
                \\--- lax ---
                \\{s}
                \\--- strict ---
                \\{s}
            , .{ actual_lax, actual_strict });
        }
    } else |_| {}
}

export fn LLVMFuzzerTestOneInput(buf: [*]const u8, len: usize) c_int {
    fuzz(buf[0..len]);
    return 0;
}
