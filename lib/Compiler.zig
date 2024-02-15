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
error_message: ?[]const u8,

pub fn init(allocator: Allocator, parser: Parser, analyzer: Analyzer) Self {
    return .{
        .allocator = allocator,
        .parser = parser,
        .analyzer = analyzer,
        .error_message = null,
    };
}

pub fn compile(self: *Self) error{CompileError}!void {
    _ = self;
}

pub fn generate(self: *Self) []u8 {
    _ = self;
    return &.{};
}
