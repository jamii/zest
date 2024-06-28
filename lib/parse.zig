const std = @import("std");
const panic = std.debug.panic;
const assert = std.debug.assert;
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;

const zest = @import("./zest.zig");
const oom = zest.oom;
const Compiler = zest.Compiler;
const TokenData = zest.TokenData;
const Builtin = zest.Builtin;
const sir = @import("./sir.zig");

pub fn parse(c: *Compiler) !void {
    _ = try parseBlock(c, .eof);
    try expect(c, .eof);
}

fn parseBlock(c: *Compiler, end: TokenData) error{ParseError}!void {
    allowNewline(c);

    _ = c.sir_expr_data.append(.block_begin);
    defer _ = c.sir_expr_data.append(.block_end);

    while (true) {
        if (peek(c) == end) break;
        const start = c.sir_expr_data.count();
        try parseExpr(c);
        if (peek(c) == .@"=") {
            try expectSpace(c);
            try expect(c, .@"=");
            try expectSpace(c);

            _ = c.sir_expr_data.insert(start, .let_begin);
            defer _ = c.sir_expr_data.append(.let_end);

            try parseExpr(c);
        }
        if (!(takeIf(c, .@";") or takeIf(c, .newline))) break;
        allowNewline(c);
    }
}

fn parseExpr(c: *Compiler) error{ParseError}!void {
    switch (peek(c)) {
        .@"(" => return parseFn(c),
        .@"if" => return parseIf(c),
        .@"while" => return parseWhile(c),
        else => return parseExprLoose(c),
    }
}

fn parseFn(c: *Compiler) error{ParseError}!void {
    _ = c.sir_expr_data.append(.fun_begin);
    defer _ = c.sir_expr_data.append(.fun_end);

    try expect(c, .@"(");
    try parseArgs(c, .@")", 0);
    try expect(c, .@")");
    try parseExpr(c);
}

fn parseIf(c: *Compiler) error{ParseError}!void {
    _ = c.sir_expr_data.append(.if_begin);
    defer _ = c.sir_expr_data.append(.if_end);

    try expect(c, .@"if");
    try parseExprAtom(c, .{});
    _ = c.sir_expr_data.append(.if_then);
    try parseExpr(c);
    try expect(c, .@"else");
    _ = c.sir_expr_data.append(.if_else);
    try parseExpr(c);
}

fn parseWhile(c: *Compiler) error{ParseError}!void {
    _ = c.sir_expr_data.append(.while_begin);
    defer _ = c.sir_expr_data.append(.while_end);

    try expect(c, .@"while");
    try parseExprAtom(c, .{});
    _ = c.sir_expr_data.append(.while_body);
    try parseExpr(c);
}

fn parseExprLoose(c: *Compiler) error{ParseError}!void {
    const start = c.sir_expr_data.count();
    try parseExprTight(c);
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

                c.sir_expr_data.insertSlice(start, &.{
                    .call_builtin_begin,
                    .object_begin,
                    .{ .i32 = 0 },
                });
                defer _ = c.sir_expr_data.appendSlice(&.{
                    .object_end,
                    .{ .call_builtin_end = op },
                });

                _ = c.sir_expr_data.append(.{ .i32 = 1 });
                try parseExprTight(c);
            },
            else => break,
        }
    }
}

fn parseExprTight(c: *Compiler) error{ParseError}!void {
    const start = c.sir_expr_data.count();
    try parseExprAtom(c, .{});
    while (true) {
        if (peekSpace(c)) break;
        switch (peek(c)) {
            .@"." => try parseGet(c, start),
            .@"(" => try parseCall(c, start),
            .@"[" => try parseMake(c, start),
            .@"/" => try parseCallSlash(c, start),
            .@"@" => try parseRefTo(c, start),
            else => break,
        }
    }
}

fn parseGet(c: *Compiler, start: usize) error{ParseError}!void {
    _ = c.sir_expr_data.insert(start, .get_begin);
    defer _ = c.sir_expr_data.append(.get_end);

    try expect(c, .@".");
    try expectNoSpace(c);
    try parseExprAtom(c, .{
        // We want to parse `x.4.2` as `{x.4}.2`, not `x.{4.2}`.
        .allow_floats = false,
    });
}

fn parseCall(c: *Compiler, start: usize) error{ParseError}!void {
    _ = c.sir_expr_data.insert(start, .call_begin);
    defer _ = c.sir_expr_data.append(.call_end);

    try expect(c, .@"(");
    try parseArgs(c, .@")", 0);
    try expect(c, .@")");
}

fn parseMake(c: *Compiler, start: usize) error{ParseError}!void {
    _ = c.sir_expr_data.insert(start, .make_begin);
    defer _ = c.sir_expr_data.append(.make_end);

    try expect(c, .@"[");
    try parseArgs(c, .@"]", 0);
    try expect(c, .@"]");
}

fn parseCallSlash(c: *Compiler, start: usize) error{ParseError}!void {
    _ = c.sir_expr_data.insert(start, .call_slash_begin);
    defer _ = c.sir_expr_data.append(.call_slash_end);

    try expect(c, .@"/");
    allowNewline(c);
    try parseName(c);
    try expectNoSpace(c);
    try expect(c, .@"(");
    try parseArgs(c, .@")", 1);
    try expect(c, .@")");
}

fn parseRefTo(c: *Compiler, start: usize) error{ParseError}!void {
    _ = c.sir_expr_data.insert(start, .ref_to_begin);
    defer _ = c.sir_expr_data.append(.ref_to_end);

    try expect(c, .@"@");
}

const ExprAtomOptions = struct {
    allow_floats: bool = true,
};

fn parseExprAtom(c: *Compiler, options: ExprAtomOptions) error{ParseError}!void {
    switch (peek(c)) {
        .name => return parseName(c),
        .number => return parseNumber(c, options),
        .string => return parseString(c),
        .@"[" => return parseObject(c),
        .@"{" => return parseGroup(c),
        .@"-" => return parseNegate(c),
        else => {
            const token = take(c);
            return fail(c, .{ .unexpected = .{ .expected = "expr-atom", .found = token } });
        },
    }
}

fn parseName(c: *Compiler) error{ParseError}!void {
    try expect(c, .name);
    const name = lastTokenText(c);
    const mut = takeIf(c, .mut);
    _ = c.sir_expr_data.append(.{ .name = .{ .name = name, .mut = mut } });
}

fn parseNumber(c: *Compiler, options: ExprAtomOptions) error{ParseError}!void {
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
        _ = c.sir_expr_data.append(.{ .f32 = num });
    } else {
        const text = lastTokenText(c);
        const num = std.fmt.parseInt(i32, text, 10) catch |err|
            return fail(c, .{ .parse_i32 = switch (err) {
            error.Overflow => .overflow,
            error.InvalidCharacter => .invalid_character,
        } });
        _ = c.sir_expr_data.append(.{ .i32 = num });
    }
}

fn parseString(c: *Compiler) error{ParseError}!void {
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
    _ = c.sir_expr_data.append(.{ .string = chars.toOwnedSlice() catch oom() });
}

fn parseObject(c: *Compiler) error{ParseError}!void {
    try expect(c, .@"[");
    try parseArgs(c, .@"]", 0);
    try expect(c, .@"]");
}

fn parseGroup(c: *Compiler) error{ParseError}!void {
    try expect(c, .@"{");
    const exprs = try parseBlock(c, .@"}");
    try expect(c, .@"}");
    return exprs;
}

fn parseNegate(c: *Compiler) error{ParseError}!void {
    _ = c.sir_expr_data.append(.call_builtin_begin);
    defer _ = c.sir_expr_data.append(.{ .call_builtin_end = .subtract });

    _ = c.sir_expr_data.append(.object_begin);
    defer _ = c.sir_expr_data.append(.object_end);

    try expect(c, .@"-");
    _ = c.sir_expr_data.append(.{ .i32 = 0 });
    _ = c.sir_expr_data.append(.{ .i32 = 0 });
    _ = c.sir_expr_data.append(.{ .i32 = 1 });
    try parseExprAtom(c, .{});
}

fn parseArgs(c: *Compiler, end: TokenData, start_ix: i32) error{ParseError}!void {
    _ = c.sir_expr_data.append(.object_begin);
    defer _ = c.sir_expr_data.append(.object_end);

    allowNewline(c);

    // ix set to null when positional args are no longer allowed
    var ix: ?i32 = start_ix;
    while (peek(c) != end) {
        if (takeIf(c, .@":")) {
            // Looks like `:name`
            ix = null;
            try expect(c, .name);
            const name = lastTokenText(c);
            _ = c.sir_expr_data.append(.{ .name = .{ .name = name, .mut = false } });
            _ = c.sir_expr_data.append(.{ .name = .{ .name = name, .mut = false } });
        } else {
            const start = c.sir_expr_data.count();
            try parseExpr(c);
            if (takeIf(c, .@":")) {
                // Looks like `key: value`
                ix = null;
                try parseExpr(c);
            } else {
                // Looks like `value`, desugar to `{ix}: value`
                if (ix) |key| {
                    c.sir_expr_data.insert(start, .{ .i32 = key });
                    ix = key + 1;
                } else {
                    return fail(c, .positional_args_after_keyed_args);
                }
            }
        }
        if (!takeIf(c, .@",")) break;
        allowNewline(c);
    }
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
        const token = c.token_data.get(c.token_next);
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
