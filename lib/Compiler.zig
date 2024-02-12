const std = @import("std");
const panic = std.debug.panic;
const assert = std.debug.assert;
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;

const util = @import("./util.zig");
const oom = util.oom;

const Self = @This();
allocator: Allocator,
parser: Parser,
analyzer: Analyzer,
error_message: ?[]const u8,

pub fn compile(self: *Self) error{CompileError}!void {}

pub fn generate(self: *Self) []u8 {
    return &.{};
}
