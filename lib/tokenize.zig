const std = @import("std");
const panic = std.debug.panic;
const assert = std.debug.assert;
const Allocator = std.mem.Allocator;

const zest = @import("./zest.zig");
const oom = zest.oom;
const Compiler = zest.Compiler;
const TokenData = zest.TokenData;

pub fn tokenize(c: *Compiler) !void {
    const source = c.source;
    var i: usize = 0;
    while (i < source.len) {
        const start = i;
        const char = source[i];
        i += 1;
        const token: TokenData = switch (char) {
            '@' => .@"@",
            '(' => .@"(",
            ')' => .@")",
            '[' => .@"[",
            ']' => .@"]",
            '}' => .@"}",
            '{' => .@"{",
            ',' => .@",",
            '.' => .@".",
            ':' => .@":",
            ';' => .@";",
            '%' => .@"%",
            '=' => token: {
                if (i < source.len and source[i] == '=') {
                    i += 1;
                    break :token .@"==";
                } else {
                    break :token .@"=";
                }
            },
            '!' => token: {
                if (i < source.len and source[i] == '=') {
                    i += 1;
                    break :token .@"!=";
                } else {
                    return fail(c, start);
                }
            },
            '~' => token: {
                if (i < source.len and source[i] == '=') {
                    i += 1;
                    break :token .@"~=";
                } else {
                    return fail(c, start);
                }
            },
            '<' => token: {
                if (i < source.len and source[i] == '=') {
                    i += 1;
                    break :token .@"<=";
                } else if (i < source.len and source[i] == '<') {
                    i += 1;
                    break :token .@"<<";
                }
                {
                    break :token .@"<";
                }
            },
            '>' => token: {
                if (i < source.len and source[i] == '=') {
                    i += 1;
                    break :token .@">=";
                } else {
                    break :token .@">";
                }
            },
            '+' => .@"+",
            '-' => .@"-",
            '/' => token: {
                if (i < source.len and source[i] == '/') {
                    while (i < source.len and source[i] != '\n') : (i += 1) {}
                    break :token .comment;
                } else {
                    break :token .@"/";
                }
            },
            '*' => .@"*",
            'a'...'z' => token: {
                i -= 1;
                while (i < source.len) {
                    switch (source[i]) {
                        'a'...'z', '0'...'9', '-' => i += 1,
                        else => break,
                    }
                }
                const name = source[start..i];
                const keywords = [_]TokenData{
                    .@"if",
                    .@"else",
                    .@"while",
                    .mut,
                };
                inline for (keywords) |keyword| {
                    if (std.mem.eql(u8, name, @tagName(keyword)))
                        break :token keyword;
                }
                break :token .name;
            },
            '\'' => token: {
                var escaped = false;
                while (i < source.len) : (i += 1) {
                    switch (source[i]) {
                        '\n' => return fail(c, start),
                        '\'' => {
                            if (!escaped) {
                                i += 1;
                                break :token .string;
                            } else {
                                escaped = false;
                            }
                        },
                        '\\' => escaped = true,
                        else => escaped = false,
                    }
                }
                return fail(c, start);
            },
            '0'...'9' => token: {
                while (i < source.len) {
                    switch (source[i]) {
                        '0'...'9' => i += 1,
                        else => break,
                    }
                }
                break :token .number;
            },
            ' ' => token: {
                while (i < source.len and source[i] == ' ') {
                    i += 1;
                }
                break :token .space;
            },
            '\n' => .newline,
            else => return fail(c, start),
        };
        _ = c.token_data.append(token);
        _ = c.token_to_source.append(.{ start, i });
    }

    _ = c.token_data.append(.eof);
    _ = c.token_to_source.append(.{ i, i });
}

fn fail(c: *Compiler, pos: usize) error{TokenizeError} {
    c.error_data = .{ .tokenize = .{ .pos = pos } };
    return error.TokenizeError;
}

pub const TokenizeErrorData = struct {
    pos: usize,
};
