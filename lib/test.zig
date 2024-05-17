const std = @import("std");
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const panic = std.debug.panic;
const assert = std.debug.assert;

const zest = @import("../lib/zest.zig");
const Compiler = zest.Compiler;
const oom = zest.oom;

pub fn evalLax(
    allocator: Allocator,
    compiler: *Compiler,
) ![]const u8 {
    try zest.compileLax(compiler);
    const value = try zest.evalMain(compiler);
    return std.fmt.allocPrint(allocator, "{}", .{value});
}

pub fn evalStrict(
    allocator: Allocator,
    compiler: *Compiler,
) ![]const u8 {
    try zest.compileLax(compiler);
    try zest.compileStrict(compiler);

    const file = std.fs.cwd().createFile("test.wasm", .{ .truncate = true }) catch |err|
        panic("Error opening test.wasm: {}", .{err});
    defer file.close();

    file.writeAll(compiler.wasm.items) catch |err|
        panic("Error writing test.wasm: {}", .{err});

    if (std.ChildProcess.run(.{
        .allocator = allocator,
        .argv = &.{ "deno", "run", "--allow-read", "test.js" },
        .max_output_bytes = std.math.maxInt(usize),
    })) |result| {
        return std.mem.concat(allocator, u8, &.{ result.stdout, result.stderr }) catch oom();
    } else |err| {
        panic("Error running test.js: {}", .{err});
    }
}

fn read_wat(allocator: Allocator) []const u8 {
    if (std.ChildProcess.run(.{
        .allocator = allocator,
        .argv = &.{ "wasm2wat", "--no-check", "-f", "test.wasm" },
        .max_output_bytes = std.math.maxInt(usize),
    })) |result| {
        return std.mem.concat(allocator, u8, &.{ result.stdout, result.stderr }) catch oom();
    } else |err| {
        panic("Error running wasm2wat: {}", .{err});
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
            const case = cases.next() orelse break;
            var parts = std.mem.split(u8, case, "\n\n");
            const source = std.mem.trim(u8, parts.next().?, "\n");
            const expected_lax = std.mem.trim(u8, parts.next() orelse "", "\n");
            const expected_strict = std.mem.trim(u8, parts.next() orelse "", "\n");
            assert(parts.next() == null);

            var compiler = Compiler.init(allocator, source);
            const actual_lax = std.mem.trim(u8, evalLax(allocator, &compiler) catch zest.formatError(&compiler), "\n");

            compiler = Compiler.init(allocator, source);
            const actual_strict = std.mem.trim(u8, evalStrict(allocator, &compiler) catch zest.formatError(&compiler), "\n");

            std.debug.print(
                \\--- wat ---
                \\ {s}
                \\
            , .{read_wat(allocator)});

            const correct_lax = std.mem.eql(u8, expected_lax, actual_lax);
            const correct_strict = std.mem.eql(u8, expected_strict, actual_strict);
            if (!correct_lax or !correct_strict) {
                std.debug.print(
                    \\=== source ===
                    \\{s}
                    \\
                , .{source});
                if (!correct_lax)
                    std.debug.print(
                        \\--- expected lax ---
                        \\{s}
                        \\--- actual lax ---
                        \\{s}
                        \\
                    , .{ expected_lax, actual_lax });
                if (!correct_strict)
                    std.debug.print(
                        \\--- expected strict ---
                        \\{s}
                        \\--- actual strict ---
                        \\{s}
                        \\
                    , .{ expected_strict, actual_strict });
                failures += 1;
                std.debug.print("\n", .{});
            }
            try writer.print(
                \\```
                \\{s}
                \\
                \\{s}
                \\
                \\{s}
                \\```
            , .{ source, actual_lax, actual_strict });
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
        std.process.exit(1);
    }
}
