//! Syntax IR

const zest = @import("./zest.zig");
const Builtin = zest.Builtin;

pub const Expr = struct { id: usize };

pub const ExprData = union(enum) {
    i64: i64,
    f64: f64,
    string: []const u8,
    name: struct {
        name: []const u8,
        mut: bool,
    },

    indirect: Expr,

    object_begin,
    object_end,
    let_begin, // value, path
    let_end,
    get_begin,
    get_end,
    fun_begin,
    fun_end,
    call_begin,
    call_end,
    call_builtin_begin: Builtin,
    call_builtin_end,
    make_begin,
    make_end,
    block_begin,
    block_last,
    block_end,
    ref_to_begin,
    ref_to_end,

    if_begin,
    if_then,
    if_else,
    if_end,
    while_begin,
    while_body,
    while_end,
};
