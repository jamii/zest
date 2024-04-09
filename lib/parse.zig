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
const Intrinsic = zest.Intrinsic;
const BinaryOp = zest.BinaryOp;
const Builtin = zest.Builtin;

pub fn parse(c: *Compiler) !void {
    _ = try parseStatements(c, .eof);
    try expect(c, .eof);
}

fn parseStatements(c: *Compiler, end: TokenData) error{ParseError}!Expr {
    var statements = ArrayList(Expr).init(c.allocator);

    allowNewline(c);

    while (true) {
        if (peek(c) == end) break;
        const statement = try parseExpr(c);
        if (peek(c) == .@"=") {
            try expectSpace(c);
            try expect(c, .@"=");
            try expectSpace(c);
            const value = try parseExpr(c);

            // Validation.
            const path_data = c.expr_data.get(statement);
            const value_data = c.expr_data.get(value);
            switch (path_data) {
                .mut => |mut| {
                    const set = c.expr_data.append(.{ .set = .{ .path = mut, .value = value } });
                    statements.append(set) catch oom();
                },
                .name => |name| {
                    const mut = value_data == .mut;
                    const let = c.expr_data.append(.{ .let = .{
                        .mut = mut,
                        .name = name,
                        .value = if (mut) value_data.mut else value,
                    } });
                    statements.append(let) catch oom();
                },
                else => return fail(c, "Invalid LHS of assignment: {}", .{path_data}),
            }
        } else {
            statements.append(statement) catch oom();
        }
        if (!(takeIf(c, .@";") or takeIf(c, .newline))) break;
        allowNewline(c);
    }
    return c.expr_data.append(.{ .statements = statements.toOwnedSlice() catch oom() });
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
    return c.expr_data.append(.{ .@"fn" = .{ .params = params, .body = body } });
}

fn parseIf(c: *Compiler) error{ParseError}!Expr {
    try expect(c, .@"if");
    const cond = try parseExprAtom(c);
    const then = try parseExpr(c);
    try expect(c, .@"else");
    const @"else" = try parseExpr(c);
    return c.expr_data.append(.{ .@"if" = .{ .cond = cond, .then = then, .@"else" = @"else" } });
}

fn parseWhile(c: *Compiler) error{ParseError}!Expr {
    try expect(c, .@"while");
    const cond = try parseExprAtom(c);
    const body = try parseExpr(c);
    return c.expr_data.append(.{ .@"while" = .{ .cond = cond, .body = body } });
}

fn parseExprLoose(c: *Compiler) error{ParseError}!Expr {
    var head = try parseExprTight(c);
    var prev_op: ?BinaryOp = null;
    while (true) {
        if (!peekSpace(c)) break;
        switch (peek(c)) {
            .@"==", .@"~=", .@"<", .@">", .@"<=", .@">=", .@"+", .@"-", .@"/", .@"*" => {
                const token = take(c);
                const op = switch (token) {
                    .@"==" => BinaryOp.equal,
                    .@"~=" => BinaryOp.equivalent,
                    .@"<" => BinaryOp.less_than,
                    .@"<=" => BinaryOp.less_than_or_equal,
                    .@">" => BinaryOp.more_than,
                    .@">=" => BinaryOp.more_than_or_equal,
                    .@"+" => BinaryOp.add,
                    .@"-" => BinaryOp.subtract,
                    .@"*" => BinaryOp.multiply,
                    .@"/" => BinaryOp.divide,
                    else => unreachable,
                };
                if (prev_op != null and prev_op != op) {
                    return fail(c, "Ambiguous precedence: {} vs {}", .{ prev_op.?, op });
                }
                prev_op = op;

                try expectSpaceOrNewline(c);
                allowNewline(c);
                const right = try parseExprTight(c);
                const zero = c.expr_data.append(.{ .i32 = 0 });
                const one = c.expr_data.append(.{ .i32 = 1 });
                head = c.expr_data.append(.{ .call = .{
                    .head = c.expr_data.append(.{ .binary_op = op }),
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
    var head = try parseExprPath(c);
    while (true) {
        if (peekSpace(c)) break;
        switch (peek(c)) {
            .@"/" => head = try parseGet(c, head),
            .@"(" => head = try parseCall(c, head),
            .@"[" => head = try parseMake(c, head),
            .@"." => head = try parseCallDot(c, head),
            else => break,
        }
    }
    return head;
}

fn parseGet(c: *Compiler, object: Expr) error{ParseError}!Expr {
    try expect(c, .@"/");
    try expectNoSpace(c);
    const key = try parseExprAtom(c);
    return c.expr_data.append(.{ .get = .{ .object = object, .key = key } });
}

fn parseCall(c: *Compiler, head: Expr) error{ParseError}!Expr {
    try expect(c, .@"(");
    const args = try parseArgs(c, .@")", 0);
    try expect(c, .@")");
    return c.expr_data.append(.{ .call = .{ .head = head, .args = args } });
}

fn parseMake(c: *Compiler, head: Expr) error{ParseError}!Expr {
    try expect(c, .@"[");
    const args = try parseArgs(c, .@"]", 0);
    try expect(c, .@"]");
    return c.expr_data.append(.{ .make = .{ .head = head, .args = args } });
}

fn parseCallDot(c: *Compiler, arg: Expr) error{ParseError}!Expr {
    try expect(c, .@".");
    allowNewline(c);
    const head = try parseName(c);
    try expectNoSpace(c);
    try expect(c, .@"(");
    const args = try parseArgs(c, .@")", 1);
    try expect(c, .@")");

    var keys = ArrayList(Expr).init(c.allocator);
    keys.append(c.expr_data.append(.{ .i32 = 0 })) catch oom();
    keys.appendSlice(args.keys) catch oom();

    var values = ArrayList(Expr).init(c.allocator);
    values.append(arg) catch oom();
    values.appendSlice(args.values) catch oom();

    return c.expr_data.append(.{ .call = .{
        .head = head,
        .args = .{
            .keys = keys.toOwnedSlice() catch oom(),
            .values = values.toOwnedSlice() catch oom(),
        },
    } });
}

fn parseExprPath(c: *Compiler) error{ParseError}!Expr {
    if (takeIf(c, .@"@")) {
        var head = try parseExprAtom(c);
        while (true) {
            if (peekSpace(c)) break;
            switch (peek(c)) {
                .@"/" => head = try parseGet(c, head),
                else => break,
            }
        }
        return c.expr_data.append(.{ .mut = head });
    } else {
        return parseExprAtom(c);
    }
}

fn parseExprAtom(c: *Compiler) error{ParseError}!Expr {
    switch (peek(c)) {
        .name => return parseName(c),
        .@"%" => return parseIntrinsic(c),
        .number => return parseNumber(c),
        .string => return parseString(c),
        .@"[" => return parseObject(c),
        .@"{" => return parseGroup(c),
        else => {
            const token = take(c);
            return fail(c, "Expected expr-atom, found {}", .{token});
        },
    }
}

fn parseName(c: *Compiler) error{ParseError}!Expr {
    try expect(c, .name);
    const name = lastTokenText(c);
    inline for (@typeInfo(Builtin).Enum.fields) |field| {
        if (std.mem.eql(u8, name, field.name)) {
            return c.expr_data.append(.{ .builtin = @as(Builtin, @enumFromInt(field.value)) });
        }
    }
    return c.expr_data.append(.{ .name = name });
}

fn parseIntrinsic(c: *Compiler) error{ParseError}!Expr {
    try expect(c, .@"%");
    try expectNoSpace(c);
    try expect(c, .name);
    const name = lastTokenText(c);
    inline for (@typeInfo(Intrinsic).Enum.fields) |field| {
        if (std.mem.eql(u8, name, field.name)) {
            return c.expr_data.append(.{ .intrinsic = @as(Intrinsic, @enumFromInt(field.value)) });
        }
    }
    return fail(c, "Invalid intrinsic: {s}", .{name});
}

fn parseNumber(c: *Compiler) error{ParseError}!Expr {
    try expect(c, .number);
    const text = lastTokenText(c);
    if (std.mem.indexOfScalar(u8, text, '.') == null) {
        const num = std.fmt.parseInt(i32, text, 10) catch |err|
            return fail(c, "Can't parse i32 because {}: {s}", .{ err, text });
        return c.expr_data.append(.{ .i32 = num });
    } else {
        const num = std.fmt.parseFloat(f32, text) catch |err|
            return fail(c, "Can't parse f32 because {}: {s}", .{ err, text });
        return c.expr_data.append(.{ .f32 = num });
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
                else => return fail(c, "Invalid string escape: {}", .{char}),
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
    return c.expr_data.append(.{ .string = chars.toOwnedSlice() catch oom() });
}

fn parseObject(c: *Compiler) error{ParseError}!Expr {
    try expect(c, .@"[");
    const fields = try parseArgs(c, .@"]", 0);
    try expect(c, .@"]");
    return c.expr_data.append(.{ .object = fields });
}

fn parseGroup(c: *Compiler) error{ParseError}!Expr {
    try expect(c, .@"{");
    const statements = try parseStatements(c, .@"}");
    try expect(c, .@"}");
    return statements;
}

fn parseArgs(c: *Compiler, end: TokenData, start_ix: i32) error{ParseError}!ObjectExprData {
    var keys = ArrayList(Expr).init(c.allocator);
    var values = ArrayList(Expr).init(c.allocator);

    allowNewline(c);

    // positional args
    var ix: i32 = start_ix;
    while (peek(c) != end and peek(c) != .@"/") {
        const key = c.expr_data.append(.{ .i32 = ix });
        ix += 1;
        const value = try parseExpr(c);
        keys.append(key) catch oom();
        values.append(value) catch oom();
        if (!takeIf(c, .@",")) break;
        allowNewline(c);
    }

    // keyed args
    while (peek(c) != end) {
        try expect(c, .@"/");
        var key = try parseExprAtom(c);
        var value: ?Expr = null;
        if (peek(c) == .@"," or peek(c) == end) {
            value = key;
            switch (c.expr_data.get(key)) {
                .name => {},
                .mut => |mut| {
                    switch (c.expr_data.get(mut)) {
                        .name => key = mut,
                        else => return fail(c, "Short keys must be either /name or /@name", .{}),
                    }
                },
                else => return fail(c, "Short keys must be either /name or /@name", .{}),
            }
        } else {
            value = try parseExpr(c);
        }
        keys.append(key) catch oom();
        values.append(value.?) catch oom();
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
        return fail(c, "Expected {}, found {}", .{ expected, found });
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
        return fail(c, "Expected space, found {}", .{token});
    }
}

fn expectSpaceOrNewline(c: *Compiler) !void {
    if (!peekSpace(c) and !peekNewline(c)) {
        const token = take(c);
        return fail(c, "Expected space or newline, found {}", .{token});
    }
}

fn expectNoSpace(c: *Compiler) !void {
    if (peekSpace(c)) {
        _ = take(c);
        return fail(c, "Unexpected space", .{});
    }
}

fn fail(c: *Compiler, comptime message: []const u8, args: anytype) error{ParseError} {
    const source_ix = c.token_to_source.get(.{ .id = c.token_next.id - 1 })[0];
    c.error_message = std.fmt.allocPrint(
        c.allocator,
        "At {}. " ++
            message ++
            "\n{s}",
        .{source_ix} ++
            args ++
            .{c.source[source_ix..@min(source_ix + 100, c.source.len)]},
    ) catch oom();
    return error.ParseError;
}
