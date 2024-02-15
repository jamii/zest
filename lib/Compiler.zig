const std = @import("std");
const panic = std.debug.panic;
const assert = std.debug.assert;
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;

const util = @import("./util.zig");
const oom = util.oom;
const Parser = @import("./Parser.zig");
const Analyzer = @import("./Analyzer.zig");

const Self = @This();
allocator: Allocator,
parser: Parser,
analyzer: Analyzer,
wasm: ArrayList(u8),
error_message: ?[]const u8,

pub fn init(allocator: Allocator, parser: Parser, analyzer: Analyzer) Self {
    return .{
        .allocator = allocator,
        .parser = parser,
        .analyzer = analyzer,
        .wasm = ArrayList(u8).init(allocator),
        .error_message = null,
    };
}

pub fn compile(self: *Self) error{CompileError}!void {
    _ = self;
}

pub fn generate(self: *Self) []u8 {
    // Magic
    self.emit(&.{ 0x00, 0x61, 0x73, 0x6D });

    // Version
    self.emit(&.{ 0x01, 0x00, 0x00, 0x00 });

    // Function types
    self.emit(&.{1});
    // Bytes in section
    self.leb_u32(4);
    // Number of fn types.
    self.leb_u32(1);
    // Fn type.
    self.emit(&.{0x60});
    // Number of param types.
    self.leb_u32(0);
    // Number of result types.
    self.leb_u32(0);

    // Functions
    self.emit(&.{3});
    // Bytes in section
    self.leb_u32(2);
    // Number of fns.
    self.leb_u32(1);
    // Type 0.
    self.leb_u32(0);

    // Exports
    self.emit(&.{7});
    // Bytes in section
    self.leb_u32(8);
    // Number of exports.
    self.leb_u32(1);
    // Name
    self.name("main");
    // Fn export.
    self.emit(&.{0x00});
    // Fn 0.
    self.leb_u32(0);

    // Code
    self.emit(&.{10});
    // Bytes in section
    self.leb_u32(4);
    // Number of fns.
    self.leb_u32(1);
    // Bytes in fn.
    self.leb_u32(2);
    // Number of locals.
    self.leb_u32(0);
    // Body = end.
    self.emit(&.{0x0B});

    return self.wasm.items;
}

fn emit(self: *Self, bytes: []const u8) void {
    self.wasm.appendSlice(bytes) catch oom();
}

fn leb_u32(self: *Self, i: u32) void {
    // https://webassembly.github.io/spec/core/binary/values.html#integers
    var n = i;
    while (n >= (1 << 7)) {
        self.wasm.append(@as(u8, @truncate(n)) | (1 << 7)) catch oom();
        n = n >> 7;
    }
    self.wasm.append(@truncate(n)) catch oom();
}

fn name(self: *Self, string: []const u8) void {
    self.leb_u32(@intCast(string.len));
    self.emit(string);
}
