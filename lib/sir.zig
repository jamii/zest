//! Syntax IR

const zest = @import("./zest.zig");
const Builtin = zest.Builtin;

pub const Expr = struct { id: usize };

pub const ExprData = union(enum) {
    // TODO Replace i32/f32 with bigInt/bigDec
    i32: i32,
    f32: f32,
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
    call_slash_begin,
    call_slash_end,
    call_builtin_begin,
    call_builtin_end: Builtin,
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

pub const Next = struct {
    expr: Expr,
    ends_remaining: usize,
};
