const std = @import("std");
const panic = std.debug.panic;
const assert = std.debug.assert;
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;

const Tokenizer = @import("./Tokenizer.zig");
const Token = Tokenizer.Token;

const Self = @This();
allocator: Allocator,
tokenizer: Tokenizer,
token_ix: usize,
exprs: ArrayList(Expr),
error_message: ?[]const u8,

pub const ExprId = usize;
pub const Expr = union(enum) {
    // TODO Replace i64/f64 with bigInt/bigDec
    i64: i64,
    f64: f64,
    string: []const u8,
    object: struct {
        keys: []ExprId,
        values: []ExprId,
    },
    builtin: Builtin,
    name: []const u8,
    let: struct {
        mut: bool,
        name: []const u8,
        value: ExprId,
    },
    set: struct {
        path: ExprId,
        value: ExprId,
    },
    @"if": struct {
        cond: ExprId,
        if_true: ExprId,
        if_false: ExprId,
    },
    @"while": struct {
        cond: ExprId,
        body: ExprId,
    },
    @"fn": struct {
        muts: []bool,
        params: [][]const u8,
        body: ExprId,
    },
    call: struct {
        head: ExprId,
        muts: []bool,
        args: []ExprId,
    },
    get_static: struct {
        object: ExprId,
        key: StaticKey,
    },
    exprs: []ExprId,
};

pub const StaticKey = union(enum) {
    i64: i64,
    string: []const u8,
};

pub const Builtin = enum {
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
};

pub fn init(allocator: Allocator, tokenizer: Tokenizer) Self {
    return Self{
        .allocator = allocator,
        .tokenizer = tokenizer,
        .token_ix = 0,
        .exprs = ArrayList(Expr).init(allocator),
        .error_message = null,
    };
}

pub fn parse(self: *Self) !void {
    _ = try self.parseExpr0(.eof);
    try self.expect(.eof);
}

fn parseExpr0(self: *Self, end: Token) error{ParseError}!ExprId {
    var exprs = ArrayList(ExprId).init(self.allocator);
    while (true) {
        if (self.peek() == end) break;
        exprs.append(try self.parseExpr1()) catch panic("OOM", .{});
        if (!self.takeIf(.@";")) break;
    }
    return self.expr(.{ .exprs = exprs.toOwnedSlice() catch panic("OOM", .{}) });
}

fn parseExpr1(self: *Self) error{ParseError}!ExprId {
    var head = try self.parseExpr2();
    var prev_builtin: ?Builtin = null;
    while (true) {
        const token = self.take();
        switch (token) {
            .@"==", .@"~=", .@"<", .@">", .@"<=", .@">=", .@"+", .@"-", .@"/", .@"*" => {
                const builtin = switch (token) {
                    .@"==" => Builtin.equal,
                    .@"~=" => Builtin.equivalent,
                    .@"<" => Builtin.less_than,
                    .@"<=" => Builtin.less_than_or_equal,
                    .@">" => Builtin.more_than,
                    .@">=" => Builtin.more_than_or_equal,
                    .@"+" => Builtin.add,
                    .@"-" => Builtin.subtract,
                    .@"*" => Builtin.multiply,
                    .@"/" => Builtin.divide,
                    else => unreachable,
                };
                if (prev_builtin != null and prev_builtin == builtin) {
                    return self.fail("Ambiguous precedence: {} vs {}", .{ prev_builtin.?, builtin });
                }
                prev_builtin = builtin;
                const right = try self.parseExpr2();
                head = self.expr(.{ .call = .{
                    .head = self.expr(.{ .builtin = builtin }),
                    .muts = self.allocator.dupe(bool, &.{ false, false }) catch panic("OOM", .{}),
                    .args = self.allocator.dupe(ExprId, &.{ head, right }) catch panic("OOM", .{}),
                } });
            },
            else => {
                self.token_ix -= 1;
                break;
            },
        }
    }
    return head;
}

fn parseExpr2(self: *Self) error{ParseError}!ExprId {
    var head = try self.parseExpr3();
    while (true) {
        const token = self.take();
        switch (token) {
            .@"[" => {
                if (self.prevToken() == .whitespace) {
                    // `foo [bar]` is a syntax error, not a call
                    self.token_ix -= 1;
                    break;
                }
                var muts = ArrayList(bool).init(self.allocator);
                var args = ArrayList(ExprId).init(self.allocator);
                while (true) {
                    if (self.peek() == .@"]") break;
                    muts.append(self.takeIf(.mut)) catch panic("OOM", .{});
                    args.append(try self.parseExpr1()) catch panic("OOM", .{});
                    if (!self.takeIf(.@",")) break;
                }
                try self.expect(.@"]");
                head = self.expr(.{ .call = .{
                    .head = head,
                    .muts = muts.toOwnedSlice() catch panic("OOM", .{}),
                    .args = args.toOwnedSlice() catch panic("OOM", .{}),
                } });
            },
            .@"." => {
                const static_key = switch (self.take()) {
                    .name => StaticKey{ .string = self.lastTokenText() },
                    .string => StaticKey{ .string = try self.parseString(self.lastTokenText()) },
                    .number => number: {
                        const text = self.lastTokenText();
                        if (std.mem.indexOfScalar(u8, text, '.') == null) {
                            break :number StaticKey{ .i64 = try self.parseInt64(self.lastTokenText()) };
                        } else {
                            return self.fail("Can't use float as key {s}", .{text});
                        }
                    },
                    else => |other_token| return self.fail("Expected name/string/number, found {}", .{other_token}),
                };
                head = self.expr(.{ .get_static = .{
                    .object = head,
                    .key = static_key,
                } });
            },
            else => {
                self.token_ix -= 1;
                break;
            },
        }
    }
    return head;
}

fn parseExpr3(self: *Self) error{ParseError}!ExprId {
    const token = self.take();
    switch (token) {
        .number => {
            const text = self.lastTokenText();
            if (std.mem.indexOfScalar(u8, text, '.') == null) {
                return self.expr(.{ .i64 = try self.parseInt64(self.lastTokenText()) });
            } else {
                return self.expr(.{ .f64 = try self.parseFloat64(self.lastTokenText()) });
            }
        },
        .string => {
            const string = try self.parseString(self.lastTokenText());
            return self.expr(.{ .string = string });
        },
        .@"[" => {
            var keys = ArrayList(ExprId).init(self.allocator);
            var values = ArrayList(ExprId).init(self.allocator);
            var key_ix: ?i64 = 0;
            while (true) {
                if (self.peek() == .@"]") break;
                var key = try self.parseExpr1();
                var value: ?ExprId = null;
                if (self.takeIf(.@"=")) {
                    value = try self.parseExpr1();
                    key_ix = null;
                    const key_expr = self.exprs.items[key];
                    if (key_expr == .name) {
                        key = self.expr(.{ .string = key_expr.name });
                    }
                } else {
                    if (key_ix == null)
                        return self.fail("Positional elems must be before key/value elems", .{});
                    value = key;
                    key = self.expr(.{ .i64 = key_ix.? });
                    key_ix.? += 1;
                }
                keys.append(key) catch panic("OOM", .{});
                values.append(value.?) catch panic("OOM", .{});
                if (!self.takeIf(.@",")) break;
            }
            try self.expect(.@"]");
            return self.expr(.{ .object = .{
                .keys = keys.toOwnedSlice() catch panic("OOM", .{}),
                .values = values.toOwnedSlice() catch panic("OOM", .{}),
            } });
        },
        .name => {
            const name = self.lastTokenText();
            return self.expr(.{ .name = name });
        },
        .let => {
            const mut = self.takeIf(.mut);
            try self.expect(.name);
            const name = self.lastTokenText();
            try self.expect(.@"=");
            const value = try self.parseExpr1();
            return self.expr(.{ .let = .{
                .mut = mut,
                .name = name,
                .value = value,
            } });
        },
        .set => {
            const path = try self.parseExpr1();
            try self.expect(.@"=");
            const value = try self.parseExpr1();
            return self.expr(.{ .set = .{
                .path = path,
                .value = value,
            } });
        },
        .@"if" => {
            const cond = try self.parseExpr1();
            const if_true = try self.parseExpr1();
            try self.expect(.@"else");
            const if_false = try self.parseExpr1();
            return self.expr(.{ .@"if" = .{
                .cond = cond,
                .if_true = if_true,
                .if_false = if_false,
            } });
        },
        .@"while" => {
            const cond = try self.parseExpr1();
            const body = try self.parseExpr1();
            return self.expr(.{ .@"while" = .{
                .cond = cond,
                .body = body,
            } });
        },
        .@"fn" => {
            try self.expect(.@"[");
            var muts = ArrayList(bool).init(self.allocator);
            var params = ArrayList([]const u8).init(self.allocator);
            while (true) {
                if (self.peek() == .@"]") break;
                muts.append(self.takeIf(.mut)) catch panic("OOM", .{});
                try self.expect(.name);
                params.append(self.lastTokenText()) catch panic("OOM", .{});
                if (!self.takeIf(.@",")) break;
            }
            try self.expect(.@"]");
            const body = try self.parseExpr1();
            return self.expr(.{ .@"fn" = .{
                .muts = muts.toOwnedSlice() catch panic("OOM", .{}),
                .params = params.toOwnedSlice() catch panic("OOM", .{}),
                .body = body,
            } });
        },
        .@"(" => {
            const inner = self.parseExpr0(.@")");
            try self.expect(.@")");
            return inner;
        },
        else => {
            return self.fail("Expected start of expression, found {}", .{token});
        },
    }
}

fn parseInt64(self: *Self, text: []const u8) !i64 {
    return std.fmt.parseInt(i64, text, 10) catch |err|
        self.fail("Can't parse i64 because {}: {s}", .{ err, text });
}

fn parseFloat64(self: *Self, text: []const u8) !f64 {
    return std.fmt.parseFloat(f64, text) catch |err|
        self.fail("Can't parse f64 because {}: {s}", .{ err, text });
}

fn parseString(self: *Self, text: []const u8) ![]u8 {
    var chars = ArrayList(u8).initCapacity(self.allocator, text.len) catch panic("OOM", .{});
    var escaped = false;
    for (text[1 .. text.len - 1]) |char| {
        if (escaped) {
            switch (char) {
                'n' => chars.appendAssumeCapacity('\n'),
                '\'' => chars.appendAssumeCapacity('\''),
                '\\' => chars.appendAssumeCapacity('\\'),
                else => return self.fail("Invalid string escape: {}", .{char}),
            }
            escaped = false;
        } else {
            switch (char) {
                '\\' => escaped = true,
                else => chars.appendAssumeCapacity(char),
            }
        }
    }
    assert(escaped == false);
    return chars.toOwnedSlice() catch panic("OOM", .{});
}

fn prevToken(self: *Self) Token {
    return self.tokenizer.tokens.items[self.token_ix - 2];
}

fn peek(self: *Self) Token {
    while (true) {
        const tokens = self.tokenizer.tokens.items;
        const token = if (self.token_ix > tokens.len) .eof else tokens[self.token_ix];
        switch (token) {
            .whitespace, .comment => self.token_ix += 1,
            else => return token,
        }
    }
}

fn take(self: *Self) Token {
    const token = self.peek();
    self.token_ix += 1;
    return token;
}

fn takeIf(self: *Self, wanted: Token) bool {
    const found = self.take();
    if (found != wanted) {
        self.token_ix -= 1;
    }
    return found == wanted;
}

fn expect(self: *Self, expected: Token) !void {
    const found = self.take();
    if (found != expected) {
        return self.fail("Expected {}, found {}", .{ expected, found });
    }
}

fn lastTokenText(self: *Self) []const u8 {
    const range = self.tokenizer.ranges.items[self.token_ix - 1];
    return self.tokenizer.source[range[0]..range[1]];
}

fn expr(self: *Self, expr_value: Expr) ExprId {
    const id = self.exprs.items.len;
    self.exprs.append(expr_value) catch panic("OOM", .{});
    return id;
}

fn fail(self: *Self, comptime message: []const u8, args: anytype) error{ParseError} {
    const source_ix = self.tokenizer.ranges.items[self.token_ix - 1][0];
    self.error_message = std.fmt.allocPrint(
        self.allocator,
        "At {}. " ++
            message ++
            "\n{s}",
        .{source_ix} ++
            args ++
            .{self.tokenizer.source[source_ix..@min(source_ix + 100, self.tokenizer.source.len)]},
    ) catch panic("OOM", .{});
    return error.ParseError;
}
