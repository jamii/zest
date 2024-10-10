//! Syntax IR

const zest = @import("./zest.zig");
const Builtin = zest.Builtin;
const Compiler = zest.Compiler;

pub const Expr = struct { id: usize };

pub const ExprData = union(enum) {
    i64: i64,
    f64: f64,
    string: []const u8,
    name: struct {
        name: []const u8,
        mut: bool,
    },
    object: struct { count: usize },
    key_value,
    pos_value: i64,
    let,
    get,
    fun,
    call,
    call_slash,
    call_builtin: Builtin,
    repr_of,
    make,
    make_slash,
    block: struct {
        count: usize,
    },
    ref_to,
    @"if",
    @"while",
    module,

    pub fn childCount(expr_data: ExprData, c: *Compiler) usize {
        _ = c;
        return switch (expr_data) {
            .i64, .f64, .string, .name => 0,
            .pos_value, .repr_of, .ref_to, .module => 1,
            .key_value, .let, .get, .fun, .call, .make, .make_slash, .@"while" => 2,
            .call_slash, .@"if" => 3,
            .object => |object| object.count,
            .block => |block| block.count,
            .call_builtin => |builtin| builtin.argCount(),
        };
    }
};
