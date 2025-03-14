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
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
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
        const lax_trimmed = std.fmt.allocPrint(allocator, "{s}{s}", .{
            compiler.printed.items,
            std.mem.trim(u8, lax, "\n"),
        }) catch oom();
        const strict_trimmed = std.mem.trim(u8, strict, "\n");

        const result_repr = compiler.tir_fun_data.get(compiler.tir_fun_main.?).return_repr.one;
        if (
        // Currently can only print integers from strict - anything else will print 'undefined'.
        (result_repr == .u32 or result_repr == .i64) and
            // Some intrinsics have observably different behaviour depending on the backend.
            std.mem.indexOf(u8, source, "%memory-size(") == null and
            std.mem.indexOf(u8, source, "%memory-grow(") == null and
            std.mem.indexOf(u8, source, "%heap-start(") == null and
            std.mem.indexOf(u8, source, "%load(") == null and
            std.mem.indexOf(u8, source, "%store(") == null and
            std.mem.indexOf(u8, source, "%panic(") == null)
        {
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
        // TODO Eventually we'll want to check that lax and strict produce the same error.
    }
}

export fn LLVMFuzzerTestOneInput(buf: [*]const u8, len: usize) c_int {
    fuzz(buf[0..len]);
    return 0;
}
