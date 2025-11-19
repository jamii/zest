//! Syntax IR

const std = @import("std");
const Allocator = std.mem.Allocator;

const zest = @import("./zest.zig");
const Builtin = zest.Builtin;
const Compiler = zest.Compiler;
const List = zest.List;

pub const Token = struct { id: usize };

pub const TokenData = enum {
    number,
    string,
    name,
    @"if",
    @"else",
    @"while",
    mut,
    namespace,
    @"@",
    @"(",
    @")",
    @"[",
    @"]",
    @"{",
    @"}",
    @",",
    @".",
    @"..",
    @":",
    @";",
    @"%",
    @"=",
    @"==",
    @"!=",
    @"~=",
    @"<",
    @"<=",
    @">",
    @">=",
    @"+",
    @"-",
    @"/",
    @"*",
    @"<<",
    comment,
    space,
    newline,
    eof,
};

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
    namespace_get,
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
    namespace,

    pub fn childCount(expr_data: ExprData, c: *Compiler) usize {
        _ = c;
        return switch (expr_data) {
            .i64, .f64, .string, .name => 0,
            .pos_value, .repr_of, .ref_to, .namespace => 1,
            .key_value, .let, .get, .namespace_get, .fun, .call, .make, .make_slash, .@"while" => 2,
            .call_slash, .@"if" => 3,
            .object => |object| object.count,
            .block => |block| block.count,
            .call_builtin => |builtin| builtin.argCount(),
        };
    }
};

pub const Source = struct { id: usize };

pub const SourceData = struct {
    origin: Origin,
    text: []const u8,
    token_data: List(Token, TokenData),
    token_to_text: List(Token, [2]usize),
    expr_data_pre: List(Expr, ExprData),
    expr_data_post: List(Expr, ExprData),

    pub fn init(allocator: Allocator, origin: Origin, text: []const u8) SourceData {
        return .{
            .origin = origin,
            .text = text,
            .token_data = .init(allocator),
            .token_to_text = .init(allocator),
            .expr_data_pre = .init(allocator),
            .expr_data_post = .init(allocator),
        };
    }
};

pub const Origin = union(enum) {
    std,
    import: []const u8,
    main,
};
