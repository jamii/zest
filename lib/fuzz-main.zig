const std = @import("std");

const fuzz = @import("./fuzz.zig");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();

    const args = try std.process.argsAlloc(allocator);
    const path = args[1];
    const cwd = std.fs.cwd();
    const file = try cwd.openFile(path, .{ .mode = .read_only });
    const input = try file.readToEndAlloc(allocator, std.math.maxInt(usize));

    fuzz.fuzz(input);
}
