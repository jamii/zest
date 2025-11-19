const std = @import("std");
const panic = std.debug.panic;
const assert = std.debug.assert;
const Allocator = std.mem.Allocator;

const zest = @import("./zest.zig");
const oom = zest.oom;
const Compiler = zest.Compiler;
const sir = zest.sir;
const TokenData = sir.TokenData;

pub fn tokenize(c: *Compiler, source: sir.Source) !void {
    const s = c.sir_source_data.getPtr(source);
    const text = s.text;
    var i: usize = 0;
    while (i < text.len) {
        const start = i;
        const char = text[i];
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
            '.' => token: {
                if (i < text.len and text[i] == '.') {
                    i += 1;
                    break :token .@"..";
                } else {
                    break :token .@".";
                }
            },
            ':' => .@":",
            ';' => .@";",
            '%' => .@"%",
            '=' => token: {
                if (i < text.len and text[i] == '=') {
                    i += 1;
                    break :token .@"==";
                } else {
                    break :token .@"=";
                }
            },
            '!' => token: {
                if (i < text.len and text[i] == '=') {
                    i += 1;
                    break :token .@"!=";
                } else {
                    return fail(c, source, start);
                }
            },
            '~' => token: {
                if (i < text.len and text[i] == '=') {
                    i += 1;
                    break :token .@"~=";
                } else {
                    return fail(c, source, start);
                }
            },
            '<' => token: {
                if (i < text.len and text[i] == '=') {
                    i += 1;
                    break :token .@"<=";
                } else if (i < text.len and text[i] == '<') {
                    i += 1;
                    break :token .@"<<";
                }
                {
                    break :token .@"<";
                }
            },
            '>' => token: {
                if (i < text.len and text[i] == '=') {
                    i += 1;
                    break :token .@">=";
                } else {
                    break :token .@">";
                }
            },
            '+' => .@"+",
            '-' => .@"-",
            '/' => token: {
                if (i < text.len and text[i] == '/') {
                    while (i < text.len and text[i] != '\n') : (i += 1) {}
                    break :token .comment;
                } else {
                    break :token .@"/";
                }
            },
            '*' => .@"*",
            'a'...'z' => token: {
                i -= 1;
                while (i < text.len) {
                    switch (text[i]) {
                        'a'...'z', '0'...'9', '-' => i += 1,
                        else => break,
                    }
                }
                const name = text[start..i];
                const keywords = [_]TokenData{
                    .@"if",
                    .@"else",
                    .@"while",
                    .mut,
                    .namespace,
                };
                inline for (keywords) |keyword| {
                    if (std.mem.eql(u8, name, @tagName(keyword)))
                        break :token keyword;
                }
                break :token .name;
            },
            '\'' => token: {
                var escaped = false;
                while (i < text.len) : (i += 1) {
                    switch (text[i]) {
                        '\n' => return fail(c, source, start),
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
                return fail(c, source, start);
            },
            '0'...'9' => token: {
                while (i < text.len) {
                    switch (text[i]) {
                        '0'...'9' => i += 1,
                        else => break,
                    }
                }
                break :token .number;
            },
            ' ' => token: {
                while (i < text.len and text[i] == ' ') {
                    i += 1;
                }
                break :token .space;
            },
            '\n' => .newline,
            else => return fail(c, source, start),
        };
        _ = s.token_data.append(token);
        _ = s.token_to_text.append(.{ start, i });
    }

    _ = s.token_data.append(.eof);
    _ = s.token_to_text.append(.{ i, i });
}

fn fail(c: *Compiler, source: sir.Source, pos: usize) error{TokenizeError} {
    c.error_data = .{ .tokenize = .{ .source = source, .pos = pos } };
    return error.TokenizeError;
}

pub const TokenizeErrorData = struct {
    source: sir.Source,
    pos: usize,
};

pub fn isName(string: []const u8) bool {
    if (string.len == 0) return false;
    switch (string[0]) {
        'a'...'z' => {},
        else => return false,
    }
    for (1..string.len) |i| {
        switch (string[i]) {
            'a'...'z', '0'...'9', '-' => {},
            else => return false,
        }
    }
    return true;
}
