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
const convertPostorderToPreorder = zest.convertPostorderToPreorder;
const sir = @import("./sir.zig");

pub fn parse(c: *Compiler) !void {
    try parseBlock(c, .eof);
    try expect(c, .eof);
    convertPostorderToPreorder(c, sir.Expr, sir.ExprData, c.sir_expr_data_post, &c.sir_expr_data_pre);
}

fn parseBlock(c: *Compiler, end: TokenData) error{ParseError}!void {
    allowNewline(c);
    var count: usize = 0;
    while (true) {
        if (peek(c) == end) break;
        try parseExpr(c);
        if (peek(c) == .@"=") {
            try expectSpace(c);
            try expect(c, .@"=");
            try expectSpace(c);
            try parseExpr(c);
            emit(c, .let);
        }
        count += 1;
        if (!(takeIf(c, .@";") or takeIf(c, .newline))) break;
        allowNewline(c);
    }
    emit(c, .{ .block = .{ .count = count } });
}

fn parseExpr(c: *Compiler) error{ParseError}!void {
    switch (peek(c)) {
        .@"(" => return parseFun(c),
        .@"if" => return parseIf(c),
        .@"while" => return parseWhile(c),
        else => return parseExprLoose(c),
    }
}

fn parseFun(c: *Compiler) error{ParseError}!void {
    try expect(c, .@"(");
    try parseArgs(c, .@")");
    try expect(c, .@")");
    try parseExpr(c);
    emit(c, .fun);
}

fn parseIf(c: *Compiler) error{ParseError}!void {
    try expect(c, .@"if");
    try parseExprAtom(c, .{});
    try parseExpr(c);
    try expect(c, .@"else");
    try parseExpr(c);
    emit(c, .@"if");
}

fn parseWhile(c: *Compiler) error{ParseError}!void {
    try expect(c, .@"while");
    try parseExprAtom(c, .{});
    try parseExpr(c);
    emit(c, .@"while");
}

fn parseExprLoose(c: *Compiler) error{ParseError}!void {
    try parseExprTight(c, .{});
    var prev_op: ?Builtin = null;
    while (true) {
        if (!peekSpace(c)) break;
        const op: Builtin = switch (peek(c)) {
            .@"==" => .equal,
            .@"!=" => .@"not-equal",
            .@"~=" => .equivalent,
            .@"<" => .@"less-than",
            .@"<=" => .@"less-than-or-equal",
            .@">" => .@"more-than",
            .@">=" => .@"more-than-or-equal",
            .@"+" => .add,
            .@"-" => .subtract,
            .@"*" => .multiply,
            .@"/" => .divide,
            .@"%" => .remainder,
            .@"<<" => .@"bit-shift-left",
            else => break,
        };
        _ = take(c);
        if (prev_op != null and prev_op != op) {
            return fail(c, .{ .ambiguous_precedence = .{ prev_op.?, op } });
        }
        prev_op = op;
        try expectSpaceOrNewline(c);
        allowNewline(c);
        try parseExprTight(c, .{});
        emit(c, .{ .call_builtin = op });
    }
}

const ExprTightOptions = struct {
    allow_call: bool = true,
};

fn parseExprTight(c: *Compiler, options: ExprTightOptions) error{ParseError}!void {
    try parseExprAtom(c, .{});
    try parseExprTightTail(c, options);
}

fn parseExprTightTail(c: *Compiler, options: ExprTightOptions) error{ParseError}!void {
    while (true) {
        if (peekSpace(c)) break;
        switch (peek(c)) {
            .@"(" => if (options.allow_call)
                try parseCall(c)
            else
                break,
            .@"." => try parseGet(c),
            .@"[" => try parseMake(c),
            .@"/" => try parseCallSlash(c),
            .@"@" => try parseRefTo(c),
            else => break,
        }
    }
}

fn parseGet(c: *Compiler) error{ParseError}!void {
    try expect(c, .@".");
    try expectNoSpace(c);
    try parseExprAtom(c, .{
        // We want to parse `x.4.2` as `{x.4}.2`, not `x.{4.2}`.
        .allow_floats = false,
    });
    emit(c, .get);
}

fn parseCall(c: *Compiler) error{ParseError}!void {
    try expect(c, .@"(");
    try parseArgs(c, .@")");
    try expect(c, .@")");
    emit(c, .call);
}

fn parseMake(c: *Compiler) error{ParseError}!void {
    try expect(c, .@"[");
    try parseArgs(c, .@"]");
    try expect(c, .@"]");
    emit(c, .make);
}

fn parseCallSlash(c: *Compiler) error{ParseError}!void {
    try expect(c, .@"/");
    allowNewline(c);
    try parseExprTight(c, .{ .allow_call = false });
    if (!peekSpace(c) and takeIf(c, .@"(")) {
        const count = try parseArgsInner(c, .@")", 1);
        try expect(c, .@")");
        emit(c, .{ .object = .{ .count = count } });
        emit(c, .call_slash);
    } else {
        emit(c, .make_slash);
    }
}

fn parseRefTo(c: *Compiler) error{ParseError}!void {
    try expect(c, .@"@");
    emit(c, .ref_to);
}

const ExprAtomOptions = struct {
    allow_floats: bool = true,
};

fn parseExprAtom(c: *Compiler, options: ExprAtomOptions) error{ParseError}!void {
    switch (peek(c)) {
        .name => try parseName(c),
        .number => try parseNumber(c, options),
        .string => try parseString(c),
        .@"[" => try parseObject(c),
        .@"{" => try parseGroup(c),
        .@"-" => try parseNegate(c),
        .@"%" => try parseBuiltinCall(c),
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
    emit(c, .{ .name = .{ .name = name, .mut = mut } });
}

fn parseNumber(c: *Compiler, options: ExprAtomOptions) error{ParseError}!void {
    try expect(c, .number);
    if (options.allow_floats and !peekSpace(c) and takeIf(c, .@".")) {
        try expectNoSpace(c);
        try expect(c, .number);
        const range0 = c.token_to_source.get(.{ .id = c.token_next.id - 3 });
        const range1 = c.token_to_source.get(.{ .id = c.token_next.id - 1 });
        const text = c.source[range0[0]..range1[1]];
        const num = std.fmt.parseFloat(f64, text) catch |err|
            return fail(c, .{ .parse_f64 = switch (err) {
            error.InvalidCharacter => .invalid_character,
        } });
        emit(c, .{ .f64 = num });
    } else {
        const text = lastTokenText(c);
        const num = std.fmt.parseInt(i64, text, 10) catch |err|
            return fail(c, .{ .parse_i64 = switch (err) {
            error.Overflow => .overflow,
            error.InvalidCharacter => .invalid_character,
        } });
        emit(c, .{ .i64 = num });
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
    emit(c, .{ .string = chars.toOwnedSlice() catch oom() });
}

fn parseObject(c: *Compiler) error{ParseError}!void {
    try expect(c, .@"[");
    try parseArgs(c, .@"]");
    try expect(c, .@"]");
}

fn parseGroup(c: *Compiler) error{ParseError}!void {
    try expect(c, .@"{");
    const exprs = try parseBlock(c, .@"}");
    try expect(c, .@"}");
    return exprs;
}

fn parseNegate(c: *Compiler) error{ParseError}!void {
    try expect(c, .@"-");
    try parseExprAtom(c, .{});
    emit(c, .{ .call_builtin = .negate });
}

fn parseBuiltinCall(c: *Compiler) error{ParseError}!void {
    try expect(c, .@"%");
    const builtin = try parseBuiltin(c);
    var count: usize = 0;
    try expect(c, .@"(");
    while (peek(c) != .@")") {
        try parseExpr(c);
        count += 1;
        if (!takeIf(c, .@",")) break;
    }
    try expect(c, .@")");
    if (count != builtin.argCount())
        return fail(c, .{ .wrong_builtin_arg_count = .{
            .expected = builtin.argCount(),
            .found = count,
        } });
    emit(c, switch (builtin) {
        .@"repr-of" => .repr_of,
        else => .{ .call_builtin = builtin },
    });
}

fn parseBuiltin(c: *Compiler) error{ParseError}!Builtin {
    try expect(c, .name);
    const name = lastTokenText(c);
    inline for (@typeInfo(Builtin).Enum.fields) |field| {
        if (std.mem.eql(u8, field.name, name))
            return @enumFromInt(field.value);
    }
    return fail(c, .{ .not_a_builtin = name });
}

fn parseArgs(c: *Compiler, end: TokenData) error{ParseError}!void {
    const count = try parseArgsInner(c, end, 0);
    emit(c, .{ .object = .{ .count = count } });
}

fn parseArgsInner(c: *Compiler, end: TokenData, start_ix: i64) error{ParseError}!usize {
    allowNewline(c);

    var count: usize = 0;
    var ix: ?i64 = start_ix;
    while (peek(c) != end) {
        if (takeIf(c, .@":")) {
            // Looks like `:name`
            try expect(c, .name);
            const name = lastTokenText(c);
            emit(c, .{ .name = .{ .name = name, .mut = false } });
            emit(c, .{ .name = .{ .name = name, .mut = false } });
            try parseExprTightTail(c, .{});
            emit(c, .key_value);
            ix = null;
        } else {
            try parseExpr(c);
            if (takeIf(c, .@":")) {
                // Looks like `key: value`
                try parseExpr(c);
                emit(c, .key_value);
                ix = null;
            } else {
                // Looks like `value`, desugar to `{ix}: value`
                if (ix == null)
                    return fail(c, .positional_args_after_keyed_args);
                emit(c, .{ .pos_value = ix.? });
                ix.? += 1;
            }
        }
        count += 1;
        if (!takeIf(c, .@",")) break;
        allowNewline(c);
    }
    return count;
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
            .eof => {
                c.token_next.id -= 1;
                return token;
            },
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

fn emit(c: *Compiler, expr_data: sir.ExprData) void {
    _ = c.sir_expr_data_post.append(expr_data);
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
    parse_i64: enum { overflow, invalid_character },
    parse_f64: enum { invalid_character },
    not_a_builtin: []const u8,
    wrong_builtin_arg_count: struct { expected: usize, found: usize },
};
