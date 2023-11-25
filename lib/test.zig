const std = @import("std");
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const panic = std.debug.panic;
const assert = std.debug.assert;

fn eval(
    allocator: Allocator,
    source: []const u8,
) ![]const u8 {
    _ = allocator;
    _ = source;
    return "unimplemented";
}

fn run(
    allocator: Allocator,
    source: []const u8,
) []const u8 {
    if (eval(allocator, source)) |result| {
        return result;
    } else |err| {
        _ = err;
        return "error";
    }
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();

    const cwd = std.fs.cwd();
    var args = try std.process.argsAlloc(allocator);
    args = args[1..];

    var rewrite = false;
    var failures: usize = 0;

    if (std.mem.eql(u8, args[0], "--rewrite")) {
        rewrite = true;
        args = args[1..];
    }

    for (args) |path| {
        std.debug.print("Opening {s}\n", .{path});

        const file = try cwd.openFile(path, .{ .mode = .read_write });
        defer file.close();

        var rewritten = ArrayList(u8).init(allocator);
        const writer = rewritten.writer();

        const text = try file.readToEndAlloc(allocator, std.math.maxInt(usize));

        var cases = std.mem.split(u8, text, "```");
        while (true) {
            const not_case = cases.next().?;
            try writer.writeAll(not_case);
            try writer.writeAll("```\n");
            const case = cases.next() orelse break;
            var parts = std.mem.split(u8, case, "\n\n");
            const source = std.mem.trim(u8, parts.next().?, "\n");
            const expected = std.mem.trim(u8, parts.next().?, "\n");
            assert(parts.next() == null);
            const actual = run(allocator, source);
            if (!std.mem.eql(
                u8,
                expected,
                std.mem.trim(u8, actual, "\n"),
            )) {
                std.debug.print(
                    \\{s}
                    \\   --- expected ---
                    \\{s}
                    \\   --- actual ---
                    \\{s}
                    \\
                    \\
                , .{ source, expected, actual });
                failures += 1;
            }
            try writer.print(
                \\{s}
                \\
                \\{s}
                \\```
            , .{ source, actual });
        }

        if (rewrite) {
            try file.seekTo(0);
            try file.setEndPos(0);
            try file.writer().writeAll(rewritten.items);
        }
    }

    if (failures == 0) {
        std.debug.print("Ok!", .{});
    } else {
        std.debug.print("Failures: {}!", .{failures});
        std.os.exit(1);
    }
}
