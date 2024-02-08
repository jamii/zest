const std = @import("std");
const panic = std.debug.panic;
const assert = std.debug.assert;
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;

const util = @import("./util.zig");
const oom = util.oom;

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
    mut: ExprId,
    let: struct {
        path: ExprId,
        value: ExprId,
    },
    @"if": struct {
        cond: ExprId,
        then: ExprId,
        @"else": ExprId,
    },
    @"while": struct {
        cond: ExprId,
        body: ExprId,
    },
    @"fn": struct {
        params: ObjectExpr,
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
    get: struct {
        object: ExprId,
        key: ExprId,
    },
    statements: []ExprId,
};

pub const ObjectExpr = struct {
    keys: []ExprId,
    values: []ExprId,
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
    _ = try self.parseStatements(.eof);
    try self.expect(.eof);

    self.parents = self.allocator.alloc(?ExprId, self.exprs.items.len) catch oom();
    for (self.parents) |*parent| parent.* = null;
    for (0.., self.exprs.items) |expr_id, an_expr| {
        switch (an_expr) {
            // TODO fill in
            .i64, .f64, .string, .builtin, .name => {},
            .object => |object| {
                for (object.keys) |key| self.parents[key] = expr_id;
                for (object.values) |value| self.parents[value] = expr_id;
            },
            .mut => |mut| {
                self.parents[mut] = expr_id;
            },
            .let => |let| {
                self.parents[let.value] = expr_id;
            },
            .@"if" => |@"if"| {
                self.parents[@"if".cond] = expr_id;
                self.parents[@"if".then] = expr_id;
                self.parents[@"if".@"else"] = expr_id;
            },
            .@"while" => |@"while"| {
                self.parents[@"while".cond] = expr_id;
                self.parents[@"while".body] = expr_id;
            },
            .@"fn" => |@"fn"| {
                for (@"fn".params.keys) |key| self.parents[key] = expr_id;
                for (@"fn".params.values) |value| self.parents[value] = expr_id;
                self.parents[@"fn".body] = expr_id;
            },
            inline .make, .call => |make| {
                self.parents[make.head] = expr_id;
                for (make.args.keys) |key| self.parents[key] = expr_id;
                for (make.args.values) |value| self.parents[value] = expr_id;
            },
            .get => |get| {
                self.parents[get.object] = expr_id;
                self.parents[get.key] = expr_id;
            },
            .statements => |statements| {
                for (statements) |child_expr_id| self.parents[child_expr_id] = expr_id;
            },
        }
    }
}

fn parseStatements(self: *Self, end: Token) error{ParseError}!ExprId {
    var statements = ArrayList(ExprId).init(self.allocator);

    self.allowNewline();

    while (true) {
        if (self.peek() == end) break;
        const statement = try self.parseExpr();
        if (self.peek() == .@"=") {
            try self.expectSpace();
            try self.expect(.@"=");
            try self.expectSpace();
            const value = try self.parseExpr();
            const let = self.expr(.{ .let = .{ .path = statement, .value = value } });
            statements.append(let) catch oom();
        } else {
            statements.append(statement) catch oom();
        }
        if (!(self.takeIf(.@";") or self.takeIf(.newline))) break;
        self.allowNewline();
    }
    return self.expr(.{ .statements = statements.toOwnedSlice() catch oom() });
}

fn parseExpr(self: *Self) error{ParseError}!ExprId {
    switch (self.peek()) {
        .@"(" => return self.parseFn(),
        .@"if" => return self.parseIf(),
        .@"while" => return self.parseWhile(),
        else => return self.parseExprLoose(),
    }
}

fn parseFn(self: *Self) error{ParseError}!ExprId {
    try self.expect(.@"(");
    const params = try self.parseArgs(.@")", 0);
    try self.expect(.@")");
    const body = try self.parseExpr();
    return self.expr(.{ .@"fn" = .{ .params = params, .body = body } });
}

fn parseIf(self: *Self) error{ParseError}!ExprId {
    try self.expect(.@"if");
    const cond = try self.parseExprAtom();
    const then = try self.parseExpr();
    try self.expect(.@"else");
    const @"else" = try self.parseExpr();
    return self.expr(.{ .@"if" = .{ .cond = cond, .then = then, .@"else" = @"else" } });
}

fn parseWhile(self: *Self) error{ParseError}!ExprId {
    try self.expect(.@"while");
    const cond = try self.parseExprAtom();
    const body = try self.parseExpr();
    return self.expr(.{ .@"while" = .{ .cond = cond, .body = body } });
}

fn parseExprLoose(self: *Self) error{ParseError}!ExprId {
    var head = try self.parseExprTight();
    var prev_builtin: ?Builtin = null;
    while (true) {
        if (!self.peekSpace()) break;
        switch (self.peek()) {
            .@"==", .@"~=", .@"<", .@">", .@"<=", .@">=", .@"+", .@"-", .@"/", .@"*" => {
                const token = self.take();
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
                if (prev_builtin != null and prev_builtin != builtin) {
                    return self.fail("Ambiguous precedence: {} vs {}", .{ prev_builtin.?, builtin });
                }
                prev_builtin = builtin;

                try self.expectSpaceOrNewline();
                self.allowNewline();
                const right = try self.parseExprTight();
                const zero = self.expr(.{ .i64 = 0 });
                const one = self.expr(.{ .i64 = 1 });
                head = self.expr(.{ .call = .{
                    .head = self.expr(.{ .builtin = builtin }),
                    .args = .{
                        .keys = self.allocator.dupe(ExprId, &.{ zero, one }) catch oom(),
                        .values = self.allocator.dupe(ExprId, &.{ head, right }) catch oom(),
                    },
                } });
            },
            else => break,
        }
    }
    return head;
}

fn parseExprTight(self: *Self) error{ParseError}!ExprId {
    var head = try self.parseExprPath();
    while (true) {
        if (self.peekSpace()) break;
        switch (self.peek()) {
            .@"/" => head = try self.parseGet(head),
            .@"(" => head = try self.parseCall(head),
            .@"[" => head = try self.parseMake(head),
            .@"." => head = try self.parseCallDot(head),
            else => break,
        }
    }
    return head;
}

fn parseGet(self: *Self, object: ExprId) error{ParseError}!ExprId {
    try self.expect(.@"/");
    try self.expectNoSpace();
    const key = try self.parseExprAtom();
    return self.expr(.{ .get = .{ .object = object, .key = key } });
}

fn parseCall(self: *Self, head: ExprId) error{ParseError}!ExprId {
    try self.expect(.@"(");
    const args = try self.parseArgs(.@")", 0);
    try self.expect(.@")");
    return self.expr(.{ .call = .{ .head = head, .args = args } });
}

fn parseMake(self: *Self, head: ExprId) error{ParseError}!ExprId {
    try self.expect(.@"[");
    const args = try self.parseArgs(.@"]", 0);
    try self.expect(.@"]");
    return self.expr(.{ .make = .{ .head = head, .args = args } });
}

fn parseCallDot(self: *Self, arg: ExprId) error{ParseError}!ExprId {
    try self.expect(.@".");
    self.allowNewline();
    const head = try self.parseName();
    try self.expectNoSpace();
    try self.expect(.@"(");
    const args = try self.parseArgs(.@")", 1);
    try self.expect(.@")");

    var keys = ArrayList(ExprId).init(self.allocator);
    keys.append(self.expr(.{ .i64 = 0 })) catch oom();
    keys.appendSlice(args.keys) catch oom();

    var values = ArrayList(ExprId).init(self.allocator);
    values.append(arg) catch oom();
    values.appendSlice(args.values) catch oom();

    return self.expr(.{ .call = .{
        .head = head,
        .args = .{
            .keys = keys.toOwnedSlice() catch oom(),
            .values = values.toOwnedSlice() catch oom(),
        },
    } });
}

fn parseExprPath(self: *Self) error{ParseError}!ExprId {
    const mut = self.takeIf(.@"@");
    var head = try self.parseExprAtom();
    while (true) {
        if (self.peekSpace()) break;
        switch (self.peek()) {
            .@"/" => head = try self.parseGet(head),
            else => break,
        }
    }
    if (mut) head = self.expr(.{ .mut = head });
    return head;
}

fn parseExprAtom(self: *Self) error{ParseError}!ExprId {
    switch (self.peek()) {
        .name => return self.parseName(),
        .number => return self.parseNumber(),
        .string => return self.parseString(),
        .@"[" => return self.parseObject(),
        .@"{" => return self.parseGroup(),
        else => {
            const token = self.take();
            return self.fail("Expected expr-atom, found {}", .{token});
        },
    }
}

fn parseName(self: *Self) error{ParseError}!ExprId {
    try self.expect(.name);
    const name = self.lastTokenText();
    if (std.mem.eql(u8, name, "as")) {
        return self.expr(.{ .builtin = .as });
    }
    if (std.mem.eql(u8, name, "get")) {
        return self.expr(.{ .builtin = .get });
    }
    if (std.mem.eql(u8, name, "get-repr")) {
        return self.expr(.{ .builtin = .@"get-repr" });
    }
    if (std.mem.eql(u8, name, "return-to")) {
        return self.expr(.{ .builtin = .@"return-to" });
    }
    return self.expr(.{ .name = name });
}

fn parseNumber(self: *Self) error{ParseError}!ExprId {
    try self.expect(.number);
    const text = self.lastTokenText();
    if (std.mem.indexOfScalar(u8, text, '.') == null) {
        const num = std.fmt.parseInt(i64, text, 10) catch |err|
            return self.fail("Can't parse i64 because {}: {s}", .{ err, text });
        return self.expr(.{ .i64 = num });
    } else {
        const num = std.fmt.parseFloat(f64, text) catch |err|
            return self.fail("Can't parse f64 because {}: {s}", .{ err, text });
        return self.expr(.{ .f64 = num });
    }
}

fn parseString(self: *Self) error{ParseError}!ExprId {
    try self.expect(.string);
    const text = self.lastTokenText();
    var chars = ArrayList(u8).initCapacity(self.allocator, text.len) catch oom();
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
    return self.expr(.{ .string = chars.toOwnedSlice() catch oom() });
}

fn parseObject(self: *Self) error{ParseError}!ExprId {
    try self.expect(.@"[");
    const fields = try self.parseArgs(.@"]", 0);
    try self.expect(.@"]");
    return self.expr(.{ .object = fields });
}

fn parseGroup(self: *Self) error{ParseError}!ExprId {
    try self.expect(.@"{");
    const statements = try self.parseStatements(.@"}");
    try self.expect(.@"}");
    return statements;
}

fn parseArgs(self: *Self, end: Token, start_ix: i64) error{ParseError}!ObjectExpr {
    var keys = ArrayList(ExprId).init(self.allocator);
    var values = ArrayList(ExprId).init(self.allocator);

    self.allowNewline();

    // positional args
    var ix: i64 = start_ix;
    while (self.peek() != end and self.peek() != .@"/") {
        const key = self.expr(.{ .i64 = ix });
        ix += 1;
        const value = try self.parseExpr();
        keys.append(key) catch oom();
        values.append(value) catch oom();
        if (!self.takeIf(.@",")) break;
        self.allowNewline();
    }

    // keyed args
    while (self.peek() != end) {
        try self.expect(.@"/");
        var key = try self.parseExprAtom();
        var value: ?ExprId = null;
        if (self.peek() == .@"," or self.peek() == end) {
            value = key;
            switch (self.exprs.items[key]) {
                .name => {},
                .mut => |mut| {
                    switch (self.exprs.items[mut]) {
                        .name => key = mut,
                        else => return self.fail("Short keys must be either /name or /@name", .{}),
                    }
                },
                else => return self.fail("Short keys must be either /name or /@name", .{}),
            }
        } else {
            value = try self.parseExpr();
        }
        keys.append(key) catch oom();
        values.append(value.?) catch oom();
        if (!self.takeIf(.@",")) break;
        self.allowNewline();
    }

    return .{
        .keys = keys.toOwnedSlice() catch oom(),
        .values = values.toOwnedSlice() catch oom(),
    };
}

fn peek(self: *Self) Token {
    const start = self.token_ix;
    const token = self.take();
    self.token_ix = start;
    return token;
}

fn peekSpace(self: *Self) bool {
    return self.tokenizer.tokens.items[self.token_ix] == .space;
}

fn peekNewline(self: *Self) bool {
    return self.tokenizer.tokens.items[self.token_ix] == .newline;
}

fn take(self: *Self) Token {
    const tokens = self.tokenizer.tokens.items;
    while (true) {
        const token = if (self.token_ix > tokens.len) .eof else tokens[self.token_ix];
        self.token_ix += 1;
        switch (token) {
            .space, .comment => {},
            else => return token,
        }
    }
}

fn takeIf(self: *Self, wanted: Token) bool {
    const start = self.token_ix;
    const found = self.take();
    if (found != wanted) {
        self.token_ix = start;
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

fn allowNewline(self: *Self) void {
    while (self.takeIf(.space) or self.takeIf(.newline)) {}
}

fn expectSpace(self: *Self) !void {
    if (!self.peekSpace()) {
        const token = self.take();
        return self.fail("Expected space, found {}", .{token});
    }
}

fn expectSpaceOrNewline(self: *Self) !void {
    if (!self.peekSpace() and !self.peekNewline()) {
        const token = self.take();
        return self.fail("Expected space or newline, found {}", .{token});
    }
}

fn expectNoSpace(self: *Self) !void {
    if (self.peekSpace()) {
        _ = self.take();
        return self.fail("Unexpected space", .{});
    }
}

fn expr(self: *Self, expr_value: Expr) ExprId {
    const id = self.exprs.items.len;
    self.exprs.append(expr_value) catch oom();
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
    ) catch oom();
    return error.ParseError;
}
