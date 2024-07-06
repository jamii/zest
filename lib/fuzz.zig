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
const evalWasm = @import("../lib/test.zig").evalWasm;
const readWat = @import("../lib/test.zig").readWat;

pub fn fuzz(source: []const u8) void {
    var arena = std.heap.ArenaAllocator.init(std.heap.c_allocator);
    defer arena.deinit();

    const allocator = arena.allocator();

    var compiler = Compiler.init(allocator, source);
    const lax_or_err = evalLax(allocator, &compiler) catch |err| switch (err) {
        error.EvalError => error.EvalError,
        else => return,
    };

    zest.compileStrict(&compiler) catch return;
    const strict = evalWasm(allocator, &compiler);

    if (lax_or_err) |lax| {
        const lax_trimmed = std.mem.trim(u8, lax, "\n");
        const strict_trimmed = std.mem.trim(u8, strict, "\n");
        // Currently can only print i32 from strict - anything else will print 'undefined'.
        if (compiler.tir_fun_data.get(compiler.tir_fun_main.?).return_repr.one == .i32) {
            if (!std.mem.eql(u8, lax_trimmed, strict_trimmed)) {
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
                , .{ lax_trimmed, strict_trimmed });
            }
        }
    } else |_| {
        // There are currently no runtime errors, but eventually we'll want to check that lax and strict produce the same error.
        panic("Lax err: {s}", .{zest.formatError(&compiler)});
    }
}

export fn LLVMFuzzerTestOneInput(buf: [*]const u8, len: usize) c_int {
    fuzz(buf[0..len]);
    return 0;
}
