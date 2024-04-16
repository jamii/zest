const std = @import("std");
const panic = std.debug.panic;
const assert = std.debug.assert;
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;

const zest = @import("./zest.zig");
const oom = zest.oom;
const Compiler = zest.Compiler;
const TokenData = zest.TokenData;
const Expr = zest.Expr;
const ExprData = zest.ExprData;
const ObjectExprData = zest.ObjectExprData;
const Builtin = zest.Builtin;

pub fn parse(c: *Compiler) !void {
    _ = try parseBlock(c, .eof);
    try expect(c, .eof);
}

fn parseBlock(c: *Compiler, end: TokenData) error{ParseError}!Expr {
    var exprs = ArrayList(Expr).init(c.allocator);

    allowNewline(c);

    while (true) {
        if (peek(c) == end) break;
        const expr = try parseExpr(c);
        if (peek(c) == .@"=") {
            try expectSpace(c);
            try expect(c, .@"=");
            try expectSpace(c);
            const value = try parseExpr(c);
            exprs.append(c.expr_syntax.append(.{ .let_or_set = .{ .path = expr, .value = value } })) catch oom();
        } else {
            exprs.append(expr) catch oom();
        }
        if (!(takeIf(c, .@";") or takeIf(c, .newline))) break;
        allowNewline(c);
    }
    return c.expr_syntax.append(.{ .block = exprs.toOwnedSlice() catch oom() });
}

fn parseExpr(c: *Compiler) error{ParseError}!Expr {
    switch (peek(c)) {
        .@"(" => return parseFn(c),
        .@"if" => return parseIf(c),
        .@"while" => return parseWhile(c),
        else => return parseExprLoose(c),
    }
}

fn parseFn(c: *Compiler) error{ParseError}!Expr {
    try expect(c, .@"(");
    const params = try parseArgs(c, .@")", 0);
    try expect(c, .@")");
    const body = try parseExpr(c);
    return c.expr_syntax.append(.{ .@"fn" = .{ .params = params, .body = body } });
}

fn parseIf(c: *Compiler) error{ParseError}!Expr {
    try expect(c, .@"if");
    const cond = try parseExprAtom(c, .{});
    const then = try parseExpr(c);
    try expect(c, .@"else");
    const @"else" = try parseExpr(c);
    return c.expr_syntax.append(.{ .@"if" = .{ .cond = cond, .then = then, .@"else" = @"else" } });
}

fn parseWhile(c: *Compiler) error{ParseError}!Expr {
    try expect(c, .@"while");
    const cond = try parseExprAtom(c, .{});
    const body = try parseExpr(c);
    return c.expr_syntax.append(.{ .@"while" = .{ .cond = cond, .body = body } });
}

fn parseExprLoose(c: *Compiler) error{ParseError}!Expr {
    var head = try parseExprTight(c);
    var prev_op: ?Builtin = null;
    while (true) {
        if (!peekSpace(c)) break;
        switch (peek(c)) {
            .@"==", .@"~=", .@"<", .@">", .@"<=", .@">=", .@"+", .@"-", .@"/", .@"*" => {
                const token = take(c);
                const op: Builtin = switch (token) {
                    .@"==" => .equal,
                    .@"~=" => .equivalent,
                    .@"<" => .less_than,
                    .@"<=" => .less_than_or_equal,
                    .@">" => .more_than,
                    .@">=" => .more_than_or_equal,
                    .@"+" => .add,
                    .@"-" => .subtract,
                    .@"*" => .multiply,
                    .@"/" => .divide,
                    else => unreachable,
                };
                if (prev_op != null and prev_op != op) {
                    return fail(c, .{ .ambiguous_precedence = .{ prev_op.?, op } });
                }
                prev_op = op;

                try expectSpaceOrNewline(c);
                allowNewline(c);
                const right = try parseExprTight(c);
                const zero = c.expr_syntax.append(.{ .i32 = 0 });
                const one = c.expr_syntax.append(.{ .i32 = 1 });
                head = c.expr_syntax.append(.{ .call = .{
                    .head = c.expr_syntax.append(.{ .builtin = op }),
                    .args = .{
                        .keys = c.allocator.dupe(Expr, &.{ zero, one }) catch oom(),
                        .values = c.allocator.dupe(Expr, &.{ head, right }) catch oom(),
                    },
                } });
            },
            else => break,
        }
    }
    return head;
}

fn parseExprTight(c: *Compiler) error{ParseError}!Expr {
    var head = try parseExprAtom(c, .{});
    while (true) {
        if (peekSpace(c)) break;
        switch (peek(c)) {
            .@"." => head = try parseGet(c, head),
            .@"(" => head = try parseCall(c, head),
            .@"[" => head = try parseMake(c, head),
            .@"/" => head = try parseCallSlash(c, head),
            else => break,
        }
    }
    return head;
}

fn parseGet(c: *Compiler, object: Expr) error{ParseError}!Expr {
    try expect(c, .@".");
    try expectNoSpace(c);
    const key = try parseExprAtom(c, .{
        // We want to parse `x.4.2` as `{x.4}.2`, not `x.{4.2}`.
        .allow_floats = false,
    });
    const zero = c.expr_syntax.append(.{ .i32 = 0 });
    const one = c.expr_syntax.append(.{ .i32 = 1 });
    return c.expr_syntax.append(.{ .call = .{
        .head = c.expr_syntax.append(.{ .builtin = .get }),
        .args = .{
            .keys = c.allocator.dupe(Expr, &.{ zero, one }) catch oom(),
            .values = c.allocator.dupe(Expr, &.{ object, key }) catch oom(),
        },
    } });
}

fn parseCall(c: *Compiler, head: Expr) error{ParseError}!Expr {
    try expect(c, .@"(");
    const args = try parseArgs(c, .@")", 0);
    try expect(c, .@")");
    return c.expr_syntax.append(.{ .call = .{ .head = head, .args = args } });
}

fn parseMake(c: *Compiler, head: Expr) error{ParseError}!Expr {
    try expect(c, .@"[");
    const args = try parseArgs(c, .@"]", 0);
    try expect(c, .@"]");
    return c.expr_syntax.append(.{ .make = .{ .head = head, .args = args } });
}

fn parseCallSlash(c: *Compiler, arg: Expr) error{ParseError}!Expr {
    try expect(c, .@"/");
    allowNewline(c);
    const head = try parseName(c);
    try expectNoSpace(c);
    try expect(c, .@"(");
    const args = try parseArgs(c, .@")", 1);
    try expect(c, .@")");

    var keys = ArrayList(Expr).init(c.allocator);
    keys.append(c.expr_syntax.append(.{ .i32 = 0 })) catch oom();
    keys.appendSlice(args.keys) catch oom();

    var values = ArrayList(Expr).init(c.allocator);
    values.append(arg) catch oom();
    values.appendSlice(args.values) catch oom();

    return c.expr_syntax.append(.{ .call = .{
        .head = head,
        .args = .{
            .keys = keys.toOwnedSlice() catch oom(),
            .values = values.toOwnedSlice() catch oom(),
        },
    } });
}

const ExprAtomOptions = struct {
    allow_floats: bool = true,
};

fn parseExprAtom(c: *Compiler, options: ExprAtomOptions) error{ParseError}!Expr {
    switch (peek(c)) {
        .name => return parseName(c),
        .number => return parseNumber(c, options),
        .string => return parseString(c),
        .@"[" => return parseObject(c),
        .@"{" => return parseGroup(c),
        else => {
            const token = take(c);
            return fail(c, .{ .unexpected = .{ .expected = "expr-atom", .found = token } });
        },
    }
}

fn parseName(c: *Compiler) error{ParseError}!Expr {
    try expect(c, .name);
    const name = lastTokenText(c);
    inline for (@typeInfo(Builtin).Enum.fields) |field| {
        if (std.mem.eql(u8, name, field.name)) {
            return c.expr_syntax.append(.{ .builtin = @as(Builtin, @enumFromInt(field.value)) });
        }
    }
    return c.expr_syntax.append(.{ .name = name });
}

fn parseNumber(c: *Compiler, options: ExprAtomOptions) error{ParseError}!Expr {
    try expect(c, .number);
    if (options.allow_floats and !peekSpace(c) and takeIf(c, .@".")) {
        try expectNoSpace(c);
        try expect(c, .number);
        const range0 = c.token_to_source.get(.{ .id = c.token_next.id - 3 });
        const range1 = c.token_to_source.get(.{ .id = c.token_next.id - 1 });
        const text = c.source[range0[0]..range1[1]];
        const num = std.fmt.parseFloat(f32, text) catch |err|
            return fail(c, .{ .parse_f32 = switch (err) {
            error.InvalidCharacter => .invalid_character,
        } });
        return c.expr_syntax.append(.{ .f32 = num });
    } else {
        const text = lastTokenText(c);
        const num = std.fmt.parseInt(i32, text, 10) catch |err|
            return fail(c, .{ .parse_i32 = switch (err) {
            error.Overflow => .overflow,
            error.InvalidCharacter => .invalid_character,
        } });
        return c.expr_syntax.append(.{ .i32 = num });
    }
}

fn parseString(c: *Compiler) error{ParseError}!Expr {
    try expect(c, .string);
    const text = lastTokenText(c);
    var chars = ArrayList(u8).initCapacity(c.allocator, text.len) catch oom();
    var escaped = false;
    for (text[1 .. text.len - 1]) |char| {
        if (escaped) {
            switch (char) {
                'n' => chars.appendAssumeCapacity('\n'),
                '\'' => chars.appendAssumeCapacity('\''),
                '\\' => chars.appendAssumeCapacity('\\'),
                else => return fail(c, .{ .invalid_string_escape = char }),
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
    return c.expr_syntax.append(.{ .string = chars.toOwnedSlice() catch oom() });
}

fn parseObject(c: *Compiler) error{ParseError}!Expr {
    try expect(c, .@"[");
    const fields = try parseArgs(c, .@"]", 0);
    try expect(c, .@"]");
    return c.expr_syntax.append(.{ .object = fields });
}

fn parseGroup(c: *Compiler) error{ParseError}!Expr {
    try expect(c, .@"{");
    const exprs = try parseBlock(c, .@"}");
    try expect(c, .@"}");
    return exprs;
}

fn parseArgs(c: *Compiler, end: TokenData, start_ix: i32) error{ParseError}!ObjectExprData {
    var keys = ArrayList(Expr).init(c.allocator);
    var values = ArrayList(Expr).init(c.allocator);

    allowNewline(c);

    // ix set to null when positional args are no longer allowed
    var ix: ?i32 = start_ix;
    while (peek(c) != end) {
        if (takeIf(c, .@":")) {
            // Looks like `:name`
            ix = null;
            try expect(c, .name);
            const name = lastTokenText(c);
            keys.append(c.expr_syntax.append(.{ .name = name })) catch oom();
            values.append(c.expr_syntax.append(.{ .name = name })) catch oom();
        } else {
            const expr = try parseExpr(c);
            if (takeIf(c, .@":")) {
                // Looks like `key: value`
                ix = null;
                const value = try parseExpr(c);
                keys.append(expr) catch oom();
                values.append(value) catch oom();
            } else {
                // Looks like `value`
                if (ix) |key| {
                    keys.append(c.expr_syntax.append(.{ .i32 = key })) catch oom();
                    values.append(expr) catch oom();
                } else {
                    return fail(c, .positional_args_after_keyed_args);
                }
            }
        }
        if (!takeIf(c, .@",")) break;
        allowNewline(c);
    }

    return .{
        .keys = keys.toOwnedSlice() catch oom(),
        .values = values.toOwnedSlice() catch oom(),
    };
}

fn peek(c: *Compiler) TokenData {
    const start = c.token_next;
    const token = take(c);
    c.token_next = start;
    return token;
}

fn peekSpace(c: *Compiler) bool {
    return c.token_data.get(c.token_next) == .space;
}

fn peekNewline(c: *Compiler) bool {
    return c.token_data.get(c.token_next) == .newline;
}

fn take(c: *Compiler) TokenData {
    while (true) {
        const token = if (c.token_next.id > c.token_data.count())
            .eof
        else
            c.token_data.get(c.token_next);
        c.token_next.id += 1;
        switch (token) {
            .space, .comment => {},
            else => return token,
        }
    }
}

fn takeIf(c: *Compiler, wanted: TokenData) bool {
    const start = c.token_next;
    const found = take(c);
    if (found != wanted) {
        c.token_next = start;
    }
    return found == wanted;
}

fn expect(c: *Compiler, expected: TokenData) !void {
    const found = take(c);
    if (found != expected) {
        return fail(c, .{ .unexpected = .{ .expected = @tagName(expected), .found = found } });
    }
}

fn lastTokenText(c: *Compiler) []const u8 {
    const range = c.token_to_source.get(.{ .id = c.token_next.id - 1 });
    return c.source[range[0]..range[1]];
}

fn allowNewline(c: *Compiler) void {
    while (takeIf(c, .space) or takeIf(c, .newline)) {}
}

fn expectSpace(c: *Compiler) !void {
    if (!peekSpace(c)) {
        const token = take(c);
        return fail(c, .{ .unexpected = .{ .expected = "space", .found = token } });
    }
}

fn expectSpaceOrNewline(c: *Compiler) !void {
    if (!peekSpace(c) and !peekNewline(c)) {
        const token = take(c);
        return fail(c, .{ .unexpected = .{ .expected = "space or newline", .found = token } });
    }
}

fn expectNoSpace(c: *Compiler) !void {
    if (peekSpace(c)) {
        _ = take(c);
        return fail(c, .unexpected_space);
    }
}

fn fail(c: *Compiler, data: ParseErrorData) error{ParseError} {
    c.error_data = .{ .parse = data };
    return error.ParseError;
}

pub const ParseErrorData = union(enum) {
    unexpected: struct {
        expected: []const u8,
        found: TokenData,
    },
    unexpected_space,
    ambiguous_precedence: [2]Builtin,
    invalid_string_escape: u8,
    positional_args_after_keyed_args,
    parse_i32: enum { overflow, invalid_character },
    parse_f32: enum { invalid_character },
};
