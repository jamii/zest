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

const Mode = enum {
    lax,
    strict,
};

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const allocator = arena.allocator();

    const cwd = std.fs.cwd();
    var args = try std.process.argsAlloc(allocator);
    args = args[1..];

    var mode: ?Mode = null;
    for (args) |arg| {
        if (std.mem.eql(u8, arg, "--lax")) {
            mode = .lax;
        } else if (std.mem.eql(u8, arg, "--strict")) {
            mode = .strict;
        } else {
            const file = try cwd.openFile(arg, .{ .mode = .read_only });
            defer file.close();

            const source = try file.readToEndAlloc(allocator, std.math.maxInt(usize));

            var compiler = Compiler.init(allocator, source);

            switch (mode orelse panic("Specify --lax or --strict", .{})) {
                .lax => {
                    const lax_or_err = evalLax(allocator, &compiler);
                    std.debug.print("{s}{s}", .{
                        compiler.printed.items,
                        lax_or_err catch
                            zest.formatError(&compiler),
                    });
                },
                .strict => {
                    zest.compileLax(&compiler) catch {
                        std.debug.print("{s}", .{
                            zest.formatError(&compiler),
                        });
                        break;
                    };
                    const strict_or_err = evalStrict(allocator, &compiler);
                    std.debug.print(
                        \\--- wat ---
                        \\{s}
                        \\
                    , .{readWat(allocator)});
                    std.debug.print("{s}", .{
                        strict_or_err catch
                            zest.formatError(&compiler),
                    });
                },
            }
        }
    }
}
