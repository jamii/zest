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
error_message: ?[]const u8,

pub fn analyze(self: *Self) error{AnalyzeError}!void {}
