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
            '@' => TokenData.@"@",
            '(' => TokenData.@"(",
            ')' => TokenData.@")",
            '[' => TokenData.@"[",
            ']' => TokenData.@"]",
            '}' => TokenData.@"}",
            '{' => TokenData.@"{",
            ',' => TokenData.@",",
            '.' => TokenData.@".",
            ';' => TokenData.@";",
            '=' => token: {
                if (i < source.len and source[i] == '=') {
                    i += 1;
                    break :token TokenData.@"==";
                } else {
                    break :token TokenData.@"=";
                }
            },
            '~' => token: {
                if (i < source.len and source[i] == '=') {
                    i += 1;
                    break :token TokenData.@"~=";
                } else {
                    return fail(c, start);
                }
            },
            '<' => token: {
                if (i < source.len and source[i] == '=') {
                    i += 1;
                    break :token TokenData.@"<=";
                } else {
                    break :token TokenData.@"<";
                }
            },
            '>' => token: {
                if (i < source.len and source[i] == '=') {
                    i += 1;
                    break :token TokenData.@">=";
                } else {
                    break :token TokenData.@">";
                }
            },
            '+' => TokenData.@"+",
            '-' => TokenData.@"-",
            '/' => token: {
                if (i < source.len and source[i] == '/') {
                    while (i < source.len and source[i] != '\n') : (i += 1) {}
                    break :token TokenData.comment;
                } else {
                    break :token TokenData.@"/";
                }
            },
            '*' => TokenData.@"*",
            '%' => TokenData.@"%",
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
                };
                break :token match(name, &keywords) orelse TokenData.name;
            },
            '\'' => token: {
                var escaped = false;
                while (i < source.len) : (i += 1) {
                    switch (source[i]) {
                        '\n' => return fail(c, start),
                        '\'' => {
                            if (!escaped) {
                                i += 1;
                                break :token TokenData.string;
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
                const before_decimal = i;
                if (i < source.len and source[i] == '.') {
                    i += 1;
                    while (i < source.len) {
                        switch (source[i]) {
                            '0'...'9' => i += 1,
                            else => break,
                        }
                    }
                    // Tokenize `42. ` as an `number . space` rather than `number space`
                    if (i - before_decimal == 1) i = before_decimal;
                }
                break :token TokenData.number;
            },
            ' ' => token: {
                while (i < source.len and source[i] == ' ') {
                    i += 1;
                }
                break :token TokenData.space;
            },
            '\n' => TokenData.newline,
            else => return fail(c, start),
        };
        _ = c.token_data.append(token);
        _ = c.token_to_source.append(.{ start, i });
    }

    _ = c.token_data.append(.eof);
    _ = c.token_to_source.append(.{ i, i });
}

fn match(name: []const u8, comptime token_data: []const TokenData) ?TokenData {
    inline for (token_data) |token| {
        if (std.mem.eql(u8, name, @tagName(token)))
            return token;
    }
    return null;
}

fn fail(c: *Compiler, pos: usize) error{TokenizeError} {
    c.error_message = std.fmt.allocPrint(c.allocator, "Tokenizer error at {}: {s}", .{
        pos,
        c.source[pos..@min(pos + 100, c.source.len)],
    }) catch oom();
    return error.TokenizeError;
}
