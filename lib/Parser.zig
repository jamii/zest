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
parents: []?ExprId,
error_message: ?[]const u8,

pub const ExprId = usize;
pub const Expr = union(enum) {
    // TODO Replace i64/f64 with bigInt/bigDec
    i64: i64,
    f64: f64,
    string: []const u8,
    object: ObjectExpr,
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
        params: ObjectPattern,
        body: ExprId,
    },
    make: struct {
        head: ExprId,
        args: ObjectExpr,
    },
    call: struct {
        head: ExprId,
        args: ObjectExpr,
    },
    get_static: struct {
        object: ExprId,
        key: StaticKey,
    },
    exprs: []ExprId,
};

pub const ObjectExpr = struct {
    muts: []bool,
    keys: []ExprId,
    values: []ExprId,
};

pub const ObjectPattern = struct {
    keys: []StaticKey,
    values: []Pattern,
};

// TODO Flesh out.
pub const Pattern = []const u8;

// TODO Expand to Value.
pub const StaticKey = union(enum) {
    i64: i64,
    string: []const u8,
    name: []const u8,
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
    @"try-get",
    @"get-repr",
    @"get-only",
};

pub fn init(allocator: Allocator, tokenizer: Tokenizer) Self {
    return Self{
        .allocator = allocator,
        .tokenizer = tokenizer,
        .token_ix = 0,
        .exprs = ArrayList(Expr).init(allocator),
        .parents = &[_]?ExprId{},
        .error_message = null,
    };
}

pub fn parse(self: *Self) !void {
    _ = try self.parseExpr0(.eof);
    try self.expect(.eof);

    self.parents = self.allocator.alloc(?ExprId, self.exprs.items.len) catch panic("OOM", .{});
    for (self.parents) |*parent| parent.* = null;
    for (0.., self.exprs.items) |expr_id, an_expr| {
        switch (an_expr) {
            .i64, .f64, .string, .builtin, .name => {},
            .object => |object| {
                for (object.keys) |key| self.parents[key] = expr_id;
                for (object.values) |value| self.parents[value] = expr_id;
            },
            .let => |let| {
                self.parents[let.value] = expr_id;
            },
            .set => |set| {
                self.parents[set.path] = expr_id;
                self.parents[set.value] = expr_id;
            },
            .@"if" => |@"if"| {
                self.parents[@"if".cond] = expr_id;
                self.parents[@"if".if_true] = expr_id;
                self.parents[@"if".if_false] = expr_id;
            },
            .@"while" => |@"while"| {
                self.parents[@"while".cond] = expr_id;
                self.parents[@"while".body] = expr_id;
            },
            .@"fn" => |@"fn"| {
                self.parents[@"fn".body] = expr_id;
            },
            inline .make, .call => |make| {
                self.parents[make.head] = expr_id;
                for (make.args.keys) |key| self.parents[key] = expr_id;
                for (make.args.values) |value| self.parents[value] = expr_id;
            },
            .get_static => |get_static| {
                self.parents[get_static.object] = expr_id;
            },
            .exprs => |exprs| {
                for (exprs) |child_expr_id| self.parents[child_expr_id] = expr_id;
            },
        }
    }
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
            .@"=", .@"~", .@"<", .@">", .@"<=", .@">=", .@"+", .@"-", .@"/", .@"*" => {
                if (self.prevToken() != .whitespace or !self.peekWhitespace()) {
                    // Operators need to have whitespace on both sides.
                    self.token_ix -= 1;
                    break;
                }
                const builtin = switch (token) {
                    .@"=" => Builtin.equal,
                    .@"~" => Builtin.equivalent,
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
                const zero = self.expr(.{ .i64 = 0 });
                const one = self.expr(.{ .i64 = 1 });
                head = self.expr(.{ .call = .{
                    .head = self.expr(.{ .builtin = builtin }),
                    .args = .{
                        .muts = self.allocator.dupe(bool, &.{ false, false }) catch panic("OOM", .{}),
                        .keys = self.allocator.dupe(ExprId, &.{ zero, one }) catch panic("OOM", .{}),
                        .values = self.allocator.dupe(ExprId, &.{ head, right }) catch panic("OOM", .{}),
                    },
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
                const object = try self.parseObject(true, .@"]");
                head = self.expr(.{ .make = .{
                    .head = head,
                    .args = object,
                } });
            },
            .@"(" => {
                if (self.prevToken() == .whitespace) {
                    // `foo (bar)` is a syntax error, not a call
                    self.token_ix -= 1;
                    break;
                }
                const object = try self.parseObject(true, .@")");
                head = self.expr(.{ .call = .{
                    .head = head,
                    .args = object,
                } });
            },
            .@"/" => {
                if (self.prevToken() == .whitespace) {
                    // `foo / bar` is a division, not a call
                    self.token_ix -= 1;
                    break;
                }
                const actual_head = try self.parseExpr3();

                var muts = ArrayList(bool).init(self.allocator);
                var keys = ArrayList(ExprId).init(self.allocator);
                var values = ArrayList(ExprId).init(self.allocator);
                muts.append(false) catch panic("OOM", .{}); // TODO Do we want to allow implicit mut?
                keys.append(self.expr(.{ .i64 = 0 })) catch panic("OOM", .{});
                values.append(head) catch panic("OOM", .{});

                if (self.takeIf(.@"(")) {
                    const object = try self.parseObject(true, .@")");
                    for (object.keys) |key| {
                        const key_expr = &self.exprs.items[key];
                        if (key_expr.* == .i64)
                            key_expr.i64 += 1;
                    }
                    muts.appendSlice(object.muts) catch panic("OOM", .{});
                    keys.appendSlice(object.keys) catch panic("OOM", .{});
                    values.appendSlice(object.values) catch panic("OOM", .{});
                }

                head = self.expr(.{ .call = .{
                    .head = actual_head,
                    .args = .{
                        .muts = muts.toOwnedSlice() catch panic("OOM", .{}),
                        .keys = keys.toOwnedSlice() catch panic("OOM", .{}),
                        .values = values.toOwnedSlice() catch panic("OOM", .{}),
                    },
                } });
            },
            .@":" => {
                if (self.peekWhitespace()) {
                    self.token_ix -= 1;
                    break;
                }
                const static_key = try self.parseStaticKey();
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
            const object = try self.parseObject(false, .@"]");
            return self.expr(.{ .object = object });
        },
        .name => {
            const name = self.lastTokenText();
            const start = self.token_ix;
            if (self.takeIf(.@":") and self.peekWhitespace()) {
                const mut = self.takeIf(.mut);
                const value = try self.parseExpr1();
                return self.expr(.{ .let = .{
                    .mut = mut,
                    .name = name,
                    .value = value,
                } });
            } else {
                self.token_ix = start;
                if (std.mem.eql(u8, name, "as")) {
                    return self.expr(.{ .builtin = .as });
                }
                if (std.mem.eql(u8, name, "get")) {
                    return self.expr(.{ .builtin = .get });
                }
                if (std.mem.eql(u8, name, "try-get")) {
                    return self.expr(.{ .builtin = .@"try-get" });
                }
                if (std.mem.eql(u8, name, "get-repr")) {
                    return self.expr(.{ .builtin = .@"get-repr" });
                }
                if (std.mem.eql(u8, name, "get-only")) {
                    return self.expr(.{ .builtin = .@"get-only" });
                }
                return self.expr(.{ .name = name });
            }
        },
        .set => {
            const path = try self.parseExpr1();
            try self.expect(.@":");
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
        .@"(" => {
            self.token_ix -= 1;
            return self.parseFn();
        },
        .@"{" => {
            const inner = self.parseExpr0(.@"}");
            try self.expect(.@"}");
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

fn parseStaticKey(self: *Self) !StaticKey {
    switch (self.take()) {
        .name => return StaticKey{ .name = self.lastTokenText() },
        .string => return StaticKey{ .string = try self.parseString(self.lastTokenText()) },
        .number => {
            const text = self.lastTokenText();
            if (std.mem.indexOfScalar(u8, text, '.') == null) {
                return StaticKey{ .i64 = try self.parseInt64(self.lastTokenText()) };
            } else {
                // TODO Is this still necessary?
                return self.fail("Can't use float as key {s}", .{text});
            }
        },
        else => |other_token| return self.fail("Expected name/string/number, found {}", .{other_token}),
    }
}

fn parseObject(self: *Self, allow_mut: bool, end: Token) !ObjectExpr {
    var muts = ArrayList(bool).init(self.allocator);
    var keys = ArrayList(ExprId).init(self.allocator);
    var values = ArrayList(ExprId).init(self.allocator);
    var key_ix: ?i64 = 0;
    while (true) {
        if (self.peek() == end) break;
        const mut = allow_mut and self.takeIf(.mut);
        var key: ?ExprId = null;
        var value: ?ExprId = null;

        const start = self.token_ix;
        if (self.takeIf(.name) and self.takeIf(.@":")) {
            const name = self.tokenText(self.token_ix - 2);
            key_ix = null;
            key = self.expr(.{ .string = name });
            if (self.peek() == .@"," or self.peek() == end) {
                value = self.expr(.{ .name = name });
            } else {
                value = try self.parseExpr1();
            }
        } else {
            self.token_ix = start;
            value = try self.parseExpr1();
            if (self.takeIf(.@":")) {
                key_ix = null;
                const key_expr = self.exprs.items[value.?];
                key = switch (key_expr) {
                    .name => |name| self.expr(.{ .string = name }),
                    else => value,
                };
                value = if (key_expr == .name and (self.peek() == .@"," or self.peek() == end))
                    self.expr(.{ .name = key_expr.name })
                else
                    try self.parseExpr1();
            } else {
                if (key_ix == null)
                    return self.fail("Positional elems must be before key/value elems", .{});
                key = self.expr(.{ .i64 = key_ix.? });
                key_ix.? += 1;
            }
        }

        muts.append(mut) catch panic("OOM", .{});
        keys.append(key.?) catch panic("OOM", .{});
        values.append(value.?) catch panic("OOM", .{});
        if (!self.takeIf(.@",")) break;
    }
    try self.expect(end);

    if (end == .@")") {
        while (true) {
            {
                const start = self.token_ix;
                if (self.peekWhitespace() and self.peek() == .@"(") {
                    const value = try self.parseFn();

                    if (key_ix == null)
                        // TODO Should we just slot them in after existing positional elems instead?
                        return self.fail("Positional elems must be before key/value elems", .{});
                    const key = self.expr(.{ .i64 = key_ix.? });
                    key_ix.? += 1;

                    muts.append(false) catch panic("OOM", .{});
                    keys.append(key) catch panic("OOM", .{});
                    values.append(value) catch panic("OOM", .{});

                    continue;
                }
                self.token_ix = start;
            }

            {
                const start = self.token_ix;
                if (self.peekWhitespace() and self.takeIf(.name) and self.takeIf(.@":") and self.takeIf(.@"(")) {
                    key_ix = null;

                    self.token_ix = start;
                    try self.expect(.name);
                    const name = self.lastTokenText();
                    try self.expect(.@":");

                    const value = try self.parseFn();

                    const key = self.expr(.{ .string = name });

                    muts.append(false) catch panic("OOM", .{});
                    keys.append(key) catch panic("OOM", .{});
                    values.append(value) catch panic("OOM", .{});

                    continue;
                }
                self.token_ix = start;
            }

            break;
        }
    }

    return ObjectExpr{
        .muts = muts.toOwnedSlice() catch panic("OOM", .{}),
        .keys = keys.toOwnedSlice() catch panic("OOM", .{}),
        .values = values.toOwnedSlice() catch panic("OOM", .{}),
    };
}

fn parseFn(self: *Self) !ExprId {
    try self.expect(.@"(");
    const params = try self.parseObjectPattern(.@")");
    try self.expect(.@")");

    // TODO design mut patterns.
    var muts = ArrayList(bool).init(self.allocator);
    for (params.values) |_| muts.append(false) catch panic("OOM", .{});

    const body = try self.parseExpr1();

    return self.expr(.{ .@"fn" = .{
        .muts = muts.toOwnedSlice() catch panic("OOM", .{}),
        .params = params,
        .body = body,
    } });
}

fn parseObjectPattern(self: *Self, end: Token) !ObjectPattern {
    var keys = ArrayList(StaticKey).init(self.allocator);
    var values = ArrayList([]const u8).init(self.allocator);
    var key_ix: ?i64 = 0;
    while (true) {
        if (self.peek() == end) break;

        var key = try self.parseStaticKey();
        var value: ?[]const u8 = null;
        if (self.takeIf(.@":")) {
            key_ix = null;
            if (self.peek() == .@"," or self.peek() == end) {
                if (key != .name)
                    return self.fail("Cannot use {} as parameter name", .{key});
                value = key.name;
            } else {
                try self.expect(.name);
                value = self.lastTokenText();
            }
        } else {
            if (key != .name)
                return self.fail("Cannot use {} as parameter name", .{key});
            if (key_ix == null)
                return self.fail("Positional elems must be before key/value elems", .{});
            value = key.name;
            key = .{ .i64 = key_ix.? };
            key_ix.? += 1;
        }
        keys.append(key) catch panic("OOM", .{});
        values.append(value.?) catch panic("OOM", .{});
        if (!self.takeIf(.@",")) break;
    }
    return .{
        .keys = keys.toOwnedSlice() catch panic("OOM", .{}),
        .values = values.toOwnedSlice() catch panic("OOM", .{}),
    };
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

fn peekWhitespace(self: *Self) bool {
    return self.tokenizer.tokens.items[self.token_ix] == .whitespace;
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
    return self.tokenText(self.token_ix - 1);
}

fn tokenText(self: *Self, token_ix: usize) []const u8 {
    const range = self.tokenizer.ranges.items[token_ix];
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
