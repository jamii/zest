const std = @import("std");
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const panic = std.debug.panic;
const assert = std.debug.assert;

const zest = @import("../lib/zest.zig");
const Compiler = zest.Compiler;
const oom = zest.oom;

fn wasmFilename(allocator: Allocator) []const u8 {
    return std.fmt.allocPrint(
        allocator,
        "test.{}.wasm",
        .{std.Thread.getCurrentId()},
    ) catch oom();
}

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
    try zest.compileStrict(compiler);
    return evalWasm(allocator, compiler);
}

pub fn evalWasm(
    allocator: Allocator,
    compiler: *Compiler,
) []const u8 {
    const wasm_filename = wasmFilename(allocator);

    {
        const file = std.fs.cwd().createFile(wasm_filename, .{ .truncate = true }) catch |err|
            panic("Error opening {s}: {}", .{ wasm_filename, err });
        defer file.close();

        file.writeAll(compiler.wasm.items) catch |err|
            panic("Error writing {s}: {}", .{ wasm_filename, err });

        std.os.linux.sync();
    }

    if (std.process.Child.run(.{
        .allocator = allocator,
        .argv = &.{ "deno", "run", "--allow-read", "test.js", wasm_filename },
        .max_output_bytes = std.math.maxInt(usize),
    })) |result| {
        return std.mem.concat(allocator, u8, &.{ result.stdout, result.stderr }) catch oom();
    } else |err| {
        panic("Error running test.js: {}", .{err});
    }
}

pub fn readWat(allocator: Allocator) []const u8 {
    const wasm_filename = wasmFilename(allocator);
    if (std.process.Child.run(.{
        .allocator = allocator,
        .argv = &.{ "wasm2wat", "--no-check", "-f", wasm_filename },
        .max_output_bytes = std.math.maxInt(usize),
    })) |result| {
        return std.mem.concat(allocator, u8, &.{ wasm_filename, "\n", result.stdout, result.stderr }) catch oom();
    } else |err| {
        panic("Error running wasm2wat: {}", .{err});
    }
}

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const allocator = arena.allocator();

    const cwd = std.fs.cwd();
    var args = try std.process.argsAlloc(allocator);
    args = args[1..];

    var rewrite = false;
    var seed_corpus = false;
    var failures: usize = 0;

    if (std.mem.eql(u8, args[0], "--rewrite")) {
        rewrite = true;
        args = args[1..];
    }
    if (std.mem.eql(u8, args[0], "--seed-corpus")) {
        seed_corpus = true;
        args = args[1..];
    }

    for (args) |path| {
        std.debug.print("Opening {s}\n", .{path});

        const file = try cwd.openFile(path, .{ .mode = .read_write });
        defer file.close();

        var rewritten = ArrayList(u8).init(allocator);
        const writer = rewritten.writer();

        const text = try file.readToEndAlloc(allocator, std.math.maxInt(usize));

        var cases = std.mem.splitSequence(u8, text, "```");
        while (true) {
            const not_case = cases.next().?;
            try writer.writeAll(not_case);
            const case = cases.next() orelse break;
            var parts = std.mem.splitSequence(u8, case, "\n\n");
            const source = std.mem.trim(u8, parts.next().?, "\n");
            const expected_lax = std.mem.trim(u8, parts.next() orelse "", "\n");
            const expected_strict = std.mem.trim(u8, parts.next() orelse expected_lax, "\n");
            assert(parts.next() == null);

            if (seed_corpus) {
                const corpus_filename = std.fmt.allocPrint(
                    allocator,
                    "./honggfuzz-corpus/{}.zest",
                    .{std.hash.Wyhash.hash(42, source)},
                ) catch oom();

                const corpus_file = std.fs.cwd().createFile(
                    corpus_filename,
                    .{ .truncate = true },
                ) catch unreachable;
                defer corpus_file.close();

                corpus_file.writeAll(source) catch unreachable;
            }

            var compiler = Compiler.init(allocator, source);
            const lax_or_err = evalLax(allocator, &compiler);
            const actual_lax =
                std.fmt.allocPrint(allocator, "{s}{s}", .{
                    compiler.printed.items,
                    std.mem.trim(
                        u8,
                        lax_or_err catch
                            zest.formatError(&compiler),
                        "\n",
                    ),
                }) catch oom();
            const should_run_strict = if (lax_or_err) |_| true else |err| switch (err) {
                error.EvalError => true,
                else => false,
            };
            const actual_strict = if (should_run_strict)
                std.mem.trim(u8, evalStrict(allocator, &compiler) catch zest.formatError(&compiler), "\n")
            else
                actual_lax;

            std.debug.print(
                \\--- wat ---
                \\{s}
                \\
            , .{readWat(allocator)});

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
            , .{ source, actual_lax });
            if (!std.mem.eql(u8, actual_lax, actual_strict)) {
                try writer.print(
                    \\
                    \\
                    \\{s}
                , .{actual_strict});
            }
            try writer.print(
                \\
                \\```
            , .{});
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
