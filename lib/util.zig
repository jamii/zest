const std = @import("std");
const panic = std.debug.panic;

pub fn oom() noreturn {
    panic("OOM", .{});
}

pub fn Context(comptime T: type) type {
    return struct {
        pub fn hash(_: anytype, key: T) u64 {
            var hasher = std.hash.Wyhash.init(42);
            key.update(&hasher);
            return hasher.final();
        }

        pub fn eql(_: anytype, a: T, b: T) bool {
            return a.equal(b);
        }
    };
}

pub fn HashMap(comptime K: type, comptime V: type) type {
    return std.HashMap(K, V, Context(K), std.hash_map.default_max_load_percentage);
}
