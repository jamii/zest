const std = @import("std");
const panic = std.debug.panic;
const assert = std.debug.assert;
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;

const zest = @import("./zest.zig");
const oom = zest.oom;
const Compiler = zest.Compiler;
const Builtin = zest.Builtin;
const convertPostorderToPreorder = zest.convertPostorderToPreorder;
const sir = @import("./sir.zig");
const TokenData = sir.TokenData;

pub fn parse(c: *Compiler, source: sir.Source) !void {
    c.source_current = source;
    defer c.source_current = null;

    c.token_next = .{ .id = 0 };

    const s = c.sir_source_data.getPtr(source);
    try parseBlock(c, s, .eof);
    try expect(c, s, .eof);
    convertPostorderToPreorder(c, sir.Expr, sir.ExprData, s.expr_data_post, &s.expr_data_pre);
}

fn parseBlock(c: *Compiler, s: *sir.SourceData, end: TokenData) error{ParseError}!void {
    allowNewline(c, s);
    var count: usize = 0;
    while (true) {
        if (peek(c, s) == end) break;
        try parseExpr(c, s);
        if (peek(c, s) == .@"=") {
            try expectSpace(c, s);
            try expect(c, s, .@"=");
            try expectSpace(c, s);
            try parseExpr(c, s);
            emit(c, s, .let);
        }
        count += 1;
        if (!(takeIf(c, s, .@";") or takeIf(c, s, .newline))) break;
        allowNewline(c, s);
    }
    emit(c, s, .{ .block = .{ .count = count } });
}

fn parseExpr(c: *Compiler, s: *sir.SourceData) error{ParseError}!void {
    switch (peek(c, s)) {
        .@"(" => try parseFun(c, s),
        .@"if" => try parseIf(c, s),
        .@"while" => try parseWhile(c, s),
        else => try parseExprLoose(c, s),
    }
}

fn parseFun(c: *Compiler, s: *sir.SourceData) error{ParseError}!void {
    try expect(c, s, .@"(");
    try parseArgs(c, s, .@")");
    try expect(c, s, .@")");

    // awkward hack
    const start = s.expr_data_post.count();
    while (peek(c, s) == .@"/") {
        try parseCallSlash(c, s);
    }
    const return_annotations = c.dupe(sir.ExprData, s.expr_data_post.data.items[start..]);
    s.expr_data_post.data.shrinkRetainingCapacity(start);

    try parseExpr(c, s);

    s.expr_data_post.appendSlice(return_annotations);
    emit(c, s, .fun);
}

fn parseIf(c: *Compiler, s: *sir.SourceData) error{ParseError}!void {
    try expect(c, s, .@"if");
    try parseExprAtom(c, s, .{});
    try parseExpr(c, s);
    try expect(c, s, .@"else");
    try parseExpr(c, s);
    emit(c, s, .@"if");
}

fn parseWhile(c: *Compiler, s: *sir.SourceData) error{ParseError}!void {
    try expect(c, s, .@"while");
    try parseExprAtom(c, s, .{});
    try parseExpr(c, s);
    emit(c, s, .@"while");
}

fn parseExprLoose(c: *Compiler, s: *sir.SourceData) error{ParseError}!void {
    try parseExprTight(c, s, .{});
    var prev_op: ?Builtin = null;
    while (true) {
        if (!peekSpace(c, s)) break;
        const op: Builtin = switch (peek(c, s)) {
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
        _ = take(c, s);
        if (prev_op != null and prev_op != op) {
            return fail(c, s, .{ .ambiguous_precedence = .{ prev_op.?, op } });
        }
        prev_op = op;
        try expectSpaceOrNewline(c, s);
        allowNewline(c, s);
        try parseExprTight(c, s, .{});
        emit(c, s, .{ .call_builtin = op });
    }
}

const ExprTightOptions = struct {
    allow_call: bool = true,
};

fn parseExprTight(c: *Compiler, s: *sir.SourceData, options: ExprTightOptions) error{ParseError}!void {
    try parseExprAtom(c, s, .{});
    try parseExprTightTail(c, s, options);
}

fn parseExprTightTail(c: *Compiler, s: *sir.SourceData, options: ExprTightOptions) error{ParseError}!void {
    while (true) {
        if (peekSpace(c, s)) break;
        switch (peek(c, s)) {
            .@"(" => if (options.allow_call)
                try parseCall(c, s)
            else
                break,
            .@"." => try parseGet(c, s),
            .@".." => try parseNamespaceGet(c, s),
            .@"[" => try parseMake(c, s),
            .@"/" => try parseCallSlash(c, s),
            .@"@" => try parseRefTo(c, s),
            else => break,
        }
    }
}

fn parseGet(c: *Compiler, s: *sir.SourceData) error{ParseError}!void {
    try expect(c, s, .@".");
    try expectNoSpace(c, s);
    try parseExprAtom(c, s, .{
        // We want to parse `x.4.2` as `{x.4}.2`, not `x.{4.2}`.
        .allow_floats = false,
    });
    emit(c, s, .get);
}

fn parseNamespaceGet(c: *Compiler, s: *sir.SourceData) error{ParseError}!void {
    try expect(c, s, .@"..");
    try expectNoSpace(c, s);
    try parseExprAtom(c, s, .{
        // We want to parse `x.4.2` as `{x.4}.2`, not `x.{4.2}`.
        .allow_floats = false,
    });
    emit(c, s, .namespace_get);
}

fn parseCall(c: *Compiler, s: *sir.SourceData) error{ParseError}!void {
    try expect(c, s, .@"(");
    try parseArgs(c, s, .@")");
    try expect(c, s, .@")");
    emit(c, s, .call);
}

fn parseMake(c: *Compiler, s: *sir.SourceData) error{ParseError}!void {
    try expect(c, s, .@"[");
    try parseArgs(c, s, .@"]");
    try expect(c, s, .@"]");
    emit(c, s, .make);
}

fn parseCallSlash(c: *Compiler, s: *sir.SourceData) error{ParseError}!void {
    try expect(c, s, .@"/");
    allowNewline(c, s);
    try parseExprTight(c, s, .{ .allow_call = false });
    if (!peekSpace(c, s) and takeIf(c, s, .@"(")) {
        const count = try parseArgsInner(c, s, .@")", 1);
        try expect(c, s, .@")");
        emit(c, s, .{ .object = .{ .count = count } });
        emit(c, s, .call_slash);
    } else {
        emit(c, s, .make_slash);
    }
}

fn parseRefTo(c: *Compiler, s: *sir.SourceData) error{ParseError}!void {
    try expect(c, s, .@"@");
    emit(c, s, .ref_to);
}

const ExprAtomOptions = struct {
    allow_floats: bool = true,
};

fn parseExprAtom(c: *Compiler, s: *sir.SourceData, options: ExprAtomOptions) error{ParseError}!void {
    switch (peek(c, s)) {
        .name => try parseName(c, s),
        .number => try parseNumber(c, s, options),
        .string => try parseString(c, s),
        .@"[" => try parseObject(c, s),
        .@"{" => try parseGroup(c, s),
        .@"-" => try parseNegate(c, s),
        .namespace => try parseNamespace(c, s),
        .@"%" => try parseBuiltinCall(c, s),
        else => {
            const token = take(c, s);
            return fail(c, s, .{ .unexpected = .{ .expected = "expr-atom", .found = token } });
        },
    }
}

fn parseName(c: *Compiler, s: *sir.SourceData) error{ParseError}!void {
    try expect(c, s, .name);
    const name = lastTokenText(c, s);
    const mut = takeIf(c, s, .mut);
    emit(c, s, .{ .name = .{ .name = name, .mut = mut } });
}

fn parseNumber(c: *Compiler, s: *sir.SourceData, options: ExprAtomOptions) error{ParseError}!void {
    try expect(c, s, .number);
    if (options.allow_floats and !peekSpace(c, s) and takeIf(c, s, .@".")) {
        try expectNoSpace(c, s);
        try expect(c, s, .number);
        const range0 = s.token_to_text.get(.{ .id = c.token_next.id - 3 });
        const range1 = s.token_to_text.get(.{ .id = c.token_next.id - 1 });
        const text = s.text[range0[0]..range1[1]];
        const num = std.fmt.parseFloat(f64, text) catch |err|
            return fail(c, s, .{ .parse_f64 = switch (err) {
                error.InvalidCharacter => .invalid_character,
            } });
        emit(c, s, .{ .f64 = num });
    } else {
        const text = lastTokenText(c, s);
        const num = std.fmt.parseInt(i64, text, 10) catch |err|
            return fail(c, s, .{ .parse_i64 = switch (err) {
                error.Overflow => .overflow,
                error.InvalidCharacter => .invalid_character,
            } });
        emit(c, s, .{ .i64 = num });
    }
}

fn parseString(c: *Compiler, s: *sir.SourceData) error{ParseError}!void {
    try expect(c, s, .string);
    const text = lastTokenText(c, s);
    var chars = ArrayList(u8).initCapacity(c.allocator, text.len) catch oom();
    var escaped = false;
    for (text[1 .. text.len - 1]) |char| {
        if (escaped) {
            switch (char) {
                'n' => chars.appendAssumeCapacity('\n'),
                '\'' => chars.appendAssumeCapacity('\''),
                '\\' => chars.appendAssumeCapacity('\\'),
                else => return fail(c, s, .{ .invalid_string_escape = char }),
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
    emit(c, s, .{ .string = chars.toOwnedSlice() catch oom() });
}

fn parseObject(c: *Compiler, s: *sir.SourceData) error{ParseError}!void {
    try expect(c, s, .@"[");
    try parseArgs(c, s, .@"]");
    try expect(c, s, .@"]");
}

fn parseGroup(c: *Compiler, s: *sir.SourceData) error{ParseError}!void {
    try expect(c, s, .@"{");
    const exprs = try parseBlock(c, s, .@"}");
    try expect(c, s, .@"}");
    return exprs;
}

fn parseNegate(c: *Compiler, s: *sir.SourceData) error{ParseError}!void {
    try expect(c, s, .@"-");
    try parseExprAtom(c, s, .{});
    emit(c, s, .{ .call_builtin = .negate });
}
fn parseNamespace(c: *Compiler, s: *sir.SourceData) error{ParseError}!void {
    try expect(c, s, .namespace);
    if (!peekSpace(c, s) and peek(c, s) == .@"{") {
        try parseGroup(c, s);
        emit(c, s, .namespace);
    } else {
        emit(c, s, .{ .name = .{ .name = "namespace", .mut = false } });
    }
}

fn parseBuiltinCall(c: *Compiler, s: *sir.SourceData) error{ParseError}!void {
    try expect(c, s, .@"%");
    const builtin = try parseBuiltin(c, s);
    var count: usize = 0;
    try expect(c, s, .@"(");
    while (peek(c, s) != .@")") {
        try parseExpr(c, s);
        count += 1;
        if (!takeIf(c, s, .@",")) break;
    }
    try expect(c, s, .@")");
    if (count != builtin.argCount())
        return fail(c, s, .{ .wrong_builtin_arg_count = .{
            .expected = builtin.argCount(),
            .found = count,
        } });
    emit(c, s, switch (builtin) {
        .@"repr-of" => .repr_of,
        else => .{ .call_builtin = builtin },
    });
}

fn parseBuiltin(c: *Compiler, s: *sir.SourceData) error{ParseError}!Builtin {
    try expect(c, s, .name);
    const name = lastTokenText(c, s);
    inline for (@typeInfo(Builtin).@"enum".fields) |field| {
        if (std.mem.eql(u8, field.name, name))
            return @enumFromInt(field.value);
    }
    return fail(c, s, .{ .not_a_builtin = name });
}

fn parseArgs(c: *Compiler, s: *sir.SourceData, end: TokenData) error{ParseError}!void {
    const count = try parseArgsInner(c, s, end, 0);
    emit(c, s, .{ .object = .{ .count = count } });
}

fn parseArgsInner(c: *Compiler, s: *sir.SourceData, end: TokenData, start_ix: i64) error{ParseError}!usize {
    allowNewline(c, s);

    var count: usize = 0;
    var ix: ?i64 = start_ix;
    while (peek(c, s) != end) {
        if (takeIf(c, s, .@":")) {
            // Looks like `:name`
            try expect(c, s, .name);
            const name = lastTokenText(c, s);
            emit(c, s, .{ .name = .{ .name = name, .mut = false } });
            emit(c, s, .{ .name = .{ .name = name, .mut = false } });
            try parseExprTightTail(c, s, .{});
            emit(c, s, .key_value);
            ix = null;
        } else {
            try parseExpr(c, s);
            if (takeIf(c, s, .@":")) {
                // Looks like `key: value`
                try parseExpr(c, s);
                emit(c, s, .key_value);
                ix = null;
            } else {
                // Looks like `value`, desugar to `{ix}: value`
                if (ix == null)
                    return fail(c, s, .positional_args_after_keyed_args);
                emit(c, s, .{ .pos_value = ix.? });
                ix.? += 1;
            }
        }
        count += 1;
        if (!takeIf(c, s, .@",")) break;
        allowNewline(c, s);
    }
    return count;
}

fn peek(c: *Compiler, s: *sir.SourceData) TokenData {
    const start = c.token_next;
    const token = take(c, s);
    c.token_next = start;
    return token;
}

fn peekSpace(c: *Compiler, s: *sir.SourceData) bool {
    return s.token_data.get(c.token_next) == .space;
}

fn peekNewline(c: *Compiler, s: *sir.SourceData) bool {
    return s.token_data.get(c.token_next) == .newline;
}

fn take(c: *Compiler, s: *sir.SourceData) TokenData {
    while (true) {
        const token = s.token_data.get(c.token_next);
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

fn takeIf(c: *Compiler, s: *sir.SourceData, wanted: TokenData) bool {
    const start = c.token_next;
    const found = take(c, s);
    if (found != wanted) {
        c.token_next = start;
    }
    return found == wanted;
}

fn expect(c: *Compiler, s: *sir.SourceData, expected: TokenData) !void {
    const found = take(c, s);
    if (found != expected) {
        return fail(c, s, .{ .unexpected = .{ .expected = @tagName(expected), .found = found } });
    }
}

fn lastTokenText(c: *Compiler, s: *sir.SourceData) []const u8 {
    const range = s.token_to_text.get(.{ .id = c.token_next.id - 1 });
    return s.text[range[0]..range[1]];
}

fn allowNewline(c: *Compiler, s: *sir.SourceData) void {
    while (takeIf(c, s, .space) or takeIf(c, s, .newline)) {}
}

fn expectSpace(c: *Compiler, s: *sir.SourceData) !void {
    if (!peekSpace(c, s)) {
        const token = take(c, s);
        return fail(c, s, .{ .unexpected = .{ .expected = "space", .found = token } });
    }
}

fn expectSpaceOrNewline(c: *Compiler, s: *sir.SourceData) !void {
    if (!peekSpace(c, s) and !peekNewline(c, s)) {
        const token = take(c, s);
        return fail(c, s, .{ .unexpected = .{ .expected = "space or newline", .found = token } });
    }
}

fn expectNoSpace(c: *Compiler, s: *sir.SourceData) !void {
    if (peekSpace(c, s)) {
        _ = take(c, s);
        return fail(c, s, .unexpected_space);
    }
}

fn emit(c: *Compiler, s: *sir.SourceData, expr_data: sir.ExprData) void {
    _ = c;
    _ = s.expr_data_post.append(expr_data);
}

fn fail(c: *Compiler, s: *sir.SourceData, data: ParseErrorData) error{ParseError} {
    _ = s;
    c.error_data = .{ .parse = .{ .source = c.source_current.?, .data = data } };
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
