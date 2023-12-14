const std = @import("std");
const panic = std.debug.panic;

pub fn oom() noreturn {
    panic("OOM", .{});
}
