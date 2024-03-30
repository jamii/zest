const std = @import("std");
const panic = std.debug.panic;
const assert = std.debug.assert;
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;

pub fn oom() noreturn {
    panic("OOM", .{});
}

pub fn fieldType(comptime T: type, comptime field_enum: std.meta.FieldEnum(T)) type {
    return std.meta.fieldInfo(T, field_enum).type;
}

pub fn List(comptime K: type, comptime V: type) type {
    return struct {
        data: ArrayList(V),

        const Self = @This();

        pub fn init(allocator: Allocator) Self {
            return .{ .data = ArrayList(V).init(allocator) };
        }

        pub fn append(self: *Self, value: V) K {
            const id = self.data.items.len;
            self.data.append(value) catch oom();
            return .{ .id = id };
        }

        pub fn appendNTimes(self: *Self, value: V, n: usize) void {
            self.data.appendNTimes(value, n) catch oom();
        }

        pub fn get(self: Self, key: K) V {
            return self.data.items[key.id];
        }

        pub fn count(self: Self) usize {
            return self.data.items.len;
        }
    };
}

pub const Token = struct { id: usize };

pub const TokenData = enum {
    number,
    string,
    name,
    @"if",
    @"else",
    @"while",
    @"@",
    @"(",
    @")",
    @"[",
    @"]",
    @"{",
    @"}",
    @",",
    @".",
    @";",
    @"=",
    @"==",
    @"~=",
    @"<",
    @"<=",
    @">",
    @">=",
    @"+",
    @"-",
    @"/",
    @"*",
    comment,
    space,
    newline,
    eof,
};

pub const Expr = struct { id: usize };

pub const ExprData = union(enum) {
    // TODO Replace i64/f64 with bigInt/bigDec
    i64: i64,
    f64: f64,
    string: []const u8,
    object: ObjectExprData,
    builtin: Builtin,
    name: []const u8,
    mut: Expr,
    let: struct {
        mut: bool,
        name: []const u8,
        value: Expr,
    },
    set: struct {
        path: Expr,
        value: Expr,
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
    @"fn": struct {
        params: ObjectExprData,
        body: Expr,
    },
    make: struct {
        head: Expr,
        args: ObjectExprData,
    },
    call: struct {
        head: Expr,
        args: ObjectExprData,
    },
    get: struct {
        object: Expr,
        key: Expr,
    },
    statements: []Expr,
};

pub const ObjectExprData = struct {
    keys: []Expr,
    values: []Expr,
};

pub const Builtin = enum {
    // operators
    equal,
    equivalent,
    less_than,
    less_than_or_equal,
    more_than,
    more_than_or_equal,
    add,
    subtract,
    multiply,
    divide,

    // named functions
    as,
    get,
    @"get-repr",
    @"return-to",
};

pub const Compiler = struct {
    allocator: Allocator,
    source: []const u8,

    token_data: List(Token, TokenData),
    token_to_source: List(Token, [2]usize),

    token_next: Token,
    expr_data: List(Expr, ExprData),

    error_message: []const u8,

    pub fn init(allocator: Allocator, source: []const u8) Compiler {
        return .{
            .allocator = allocator,
            .source = source,

            .token_data = fieldType(Compiler, .token_data).init(allocator),
            .token_to_source = fieldType(Compiler, .token_to_source).init(allocator),

            .token_next = .{ .id = 0 },
            .expr_data = fieldType(Compiler, .expr_data).init(allocator),

            .error_message = "",
        };
    }
};

pub const tokenize = @import("./tokenize.zig").tokenize;

pub const parse = @import("./parse.zig").parse;

pub fn compile(c: *Compiler) error{ TokenizeError, ParseError }!void {
    try tokenize(c);
    assert(c.token_data.count() == c.token_to_source.count());
    try parse(c);
}
