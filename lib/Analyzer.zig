const std = @import("std");
const panic = std.debug.panic;
const assert = std.debug.assert;
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;

const util = @import("./util.zig");
const oom = util.oom;
const Parser = @import("./Parser.zig");

const Self = @This();
allocator: Allocator,
parser: Parser,
error_message: ?[]const u8,

pub fn init(allocator: Allocator, parser: Parser) Self {
    return .{
        .allocator = allocator,
        .parser = parser,
        .error_message = null,
    };
}

pub fn analyze(self: *Self) error{AnalyzeError}!void {
    _ = self;
}
