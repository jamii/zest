const std = @import("std");
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const panic = std.debug.panic;
const assert = std.debug.assert;

const util = @import("./util.zig");
const oom = util.oom;

const Tokenizer = @import("./Tokenizer.zig");
const Parser = @import("./Parser.zig");
const Semantalyzer = @import("./Semantalyzer.zig");
const Analyzer = @import("./Analyzer.zig");
const Compiler = @import("./Compiler.zig");

const Baton = struct {
    tokenizer: ?Tokenizer = null,
    parser: ?Parser = null,
    semantalyzer: ?Semantalyzer = null,
    analyzer: ?Analyzer = null,
    compiler: ?Compiler = null,
};

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
    source: []const u8,
    baton: *Baton,
) ![]const u8 {
    baton.tokenizer = Tokenizer.init(allocator, source);
    try baton.tokenizer.?.tokenize();
    baton.parser = Parser.init(allocator, baton.tokenizer.?);
    try baton.parser.?.parse();
    baton.analyzer = Analyzer.init(allocator, baton.parser.?);
    try baton.analyzer.?.analyze();
    baton.compiler = Compiler.init(allocator, baton.parser.?, baton.analyzer.?);
    const wasm = try baton.compiler.?.compile();
    const value_compiled = eval_wasm(allocator, wasm);
    // TODO Assert same results
    //baton.semantalyzer = Semantalyzer.init(allocator, baton.parser.?);
    //const value_interpreted = try baton.semantalyzer.?.semantalyze();
    //return std.fmt.allocPrint(allocator, "{}", .{value});
    return value_compiled;
}

fn run(
    allocator: Allocator,
    source: []const u8,
) []const u8 {
    var baton = Baton{};
    if (eval(allocator, source, &baton)) |result| {
        return result;
    } else |err| {
        //if (baton.tokenizer) |tokenizer|
        //    std.debug.print("{any}\n\n", .{tokenizer.tokens.items});
        //if (baton.parser) |parser|
        //    std.debug.print("{any}\n\n", .{parser.exprs.items});
        //if (@errorReturnTrace()) |trace|
        //    std.debug.dumpStackTrace(trace.*);
        return switch (err) {
            error.TokenizeError => baton.tokenizer.?.error_message.?,
            error.ParseError => baton.parser.?.error_message.?,
            error.AnalyzeError => baton.analyzer.?.error_message.?,
            error.CompileError => baton.compiler.?.error_message.?,
            //error.SemantalyzeError => baton.semantalyzer.?.error_message.?,
            //error.OutOfMemory => oom(),
        };
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
