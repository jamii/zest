const std = @import("std");
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const panic = std.debug.panic;
const assert = std.debug.assert;

const zest = @import("./zest.zig");
const Compiler = zest.Compiler;
const oom = zest.oom;

fn eval_wasm(
    allocator: Allocator,
    wasm: []const u8,
) []const u8 {
    const file = std.fs.cwd().createFile("test.wasm", .{ .truncate = true }) catch |err|
        panic("Error opening test.wasm: {}", .{err});
    defer file.close();

    file.writeAll(wasm) catch |err|
        panic("Error writing test.wasm: {}", .{err});

    if (std.ChildProcess.exec(.{
        .allocator = allocator,
        .argv = &.{ "deno", "run", "--allow-read", "test.js" },
        .max_output_bytes = std.math.maxInt(usize),
    })) |result| {
        return std.mem.concat(allocator, u8, &.{ result.stdout, result.stderr }) catch oom();
    } else |err| {
        panic("Error running test.js: {}", .{err});
    }
}

fn eval(
    allocator: Allocator,
    compiler: *Compiler,
) ![]const u8 {
    try zest.compile(compiler);
    const wasm = &[_]u8{};
    return eval_wasm(allocator, wasm);
}

fn run(
    allocator: Allocator,
    source: []const u8,
) []const u8 {
    var compiler = Compiler.init(allocator, source);
    if (eval(allocator, &compiler)) |result| {
        return result;
    } else |_| {
        return compiler.error_message;
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
            const source_untrimmed = parts.next().?;
            const source = std.mem.trim(u8, source_untrimmed, "\n");
            const expected = std.mem.trim(u8, case[source_untrimmed.len..], "\n");
            //assert(parts.next() == null);
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
