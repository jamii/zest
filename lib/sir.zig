//! Syntax IR

const zest = @import("./zest.zig");
const Builtin = zest.Builtin;

pub const Expr = struct { id: usize };

pub const ExprData = union(enum) {
    // TODO Replace i32/f32 with bigInt/bigDec
    i32: i32,
    f32: f32,
    string: []const u8,
    object: Object,
    name: struct {
        name: []const u8,
        mut: bool,
    },
    mut: Expr,
    builtin: Builtin,
    let: struct {
        path: Expr,
        value: Expr,
    },
    get: struct {
        object: Expr,
        key: Expr,
    },
    @"if": struct {
        cond: Expr,
        then: Expr,
        @"else": Expr,
    },
    @"while": struct {
        cond: Expr,
        body: Expr,
    },
    fun: struct {
        params: Object,
        body: Expr,
    },
    make: struct {
        head: Expr,
        args: Object,
    },
    call: struct {
        head: Expr,
        args: Object,
    },
    block: []Expr,
    ref_to: Expr,
};

pub const Object = struct {
    keys: []Expr,
    values: []Expr,
};
