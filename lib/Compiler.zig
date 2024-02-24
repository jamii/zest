const std = @import("std");
const panic = std.debug.panic;
const assert = std.debug.assert;
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;

const util = @import("./util.zig");
const oom = util.oom;
const Parser = @import("./Parser.zig");
const Analyzer = @import("./Analyzer.zig");
const ExprId = Parser.ExprId;

const Self = @This();
allocator: Allocator,
parser: Parser,
analyzer: Analyzer,
scope: Scope,
functions: ArrayList(Function),
wasm: ArrayList(u8),
error_message: ?[]const u8,

const Scope = ArrayList(Binding);

const Binding = struct {
    name: []const u8,
    expr_id: ExprId,
};

const Function = struct {
    name: []const u8,
    locals_count: u32,
    body: []const Instruction,
};

const Instruction = union(enum) {
    i64: i64,
};

pub fn init(allocator: Allocator, parser: Parser, analyzer: Analyzer) Self {
    return .{
        .allocator = allocator,
        .parser = parser,
        .analyzer = analyzer,
        .scope = Scope.init(allocator),
        .functions = ArrayList(Function).init(allocator),
        .wasm = ArrayList(u8).init(allocator),
        .error_message = null,
    };
}

pub fn compile(self: *Self) error{CompileError}!void {
    self.functions.append(.{
        .name = "main",
        .locals_count = 0,
        .body = try self.compileBody(self.parser.exprs.items.len - 1),
    }) catch oom();
}

fn compileBody(self: *Self, expr_id: ExprId) error{CompileError}![]Instruction {
    var instructions = ArrayList(Instruction).init(self.allocator);
    try self.compileExpr(expr_id, &instructions);
    return instructions.toOwnedSlice() catch oom();
}

fn compileExpr(self: *Self, expr_id: ExprId, instructions: *ArrayList(Instruction)) error{CompileError}!void {
    const expr = self.parser.exprs.items[expr_id];
    switch (expr) {
        .i64 => |num| {
            instructions.append(.{ .i64 = num }) catch oom();
        },
        .statements => |statements| {
            for (statements) |statement| {
                try self.compileExpr(statement, instructions);
            }
        },
        else => return self.fail("TODO Can't compile {}", .{expr}),
    }
}

pub fn emitAll(self: *Self) []u8 {
    // Magic
    self.emitBytes(&.{ 0x00, 0x61, 0x73, 0x6D });

    // Version
    self.emitBytes(&.{ 0x01, 0x00, 0x00, 0x00 });

    {
        // Function types
        const section = self.emitSectionStart(1);
        defer self.emitSectionEnd(section);

        self.emitLenOf(self.functions.items);
        for (self.functions.items) |function| {
            _ = function;
            // Fn type.
            self.emitByte(0x60);
            // Number of param types.
            self.emitLebU32(0);
            // Number of result types.
            self.emitLebU32(0);
        }
    }

    {
        // Functions
        const section = self.emitSectionStart(3);
        defer self.emitSectionEnd(section);

        self.emitLenOf(self.functions.items);
        for (self.functions.items) |function| {
            _ = function;
            // Type 0.
            self.emitLebU32(0);
        }
    }

    {
        // Exports
        const section = self.emitSectionStart(7);
        defer self.emitSectionEnd(section);

        // Number of exports.
        self.emitLebU32(1);
        // Name
        self.emitName("main");
        // Fn export.
        self.emitByte(0x00);
        // Fn 0.
        self.emitLebU32(0);
    }

    {
        // Code
        const section = self.emitSectionStart(10);
        defer self.emitSectionEnd(section);

        self.emitLenOf(self.functions.items);
        for (self.functions.items) |function| {
            const start = self.emitByteCountLater();
            defer self.emitByteCount(start);

            self.emitLebU32(function.locals_count);

            //for (function.body) |instruction| {
            //    self.emitInstruction(instruction);
            //}

            // Emit end.
            self.emitByte(0x0B);
        }
    }

    return self.wasm.items;
}

fn emitByte(self: *Self, b: u8) void {
    self.wasm.append(b) catch oom();
}

fn emitBytes(self: *Self, bs: []const u8) void {
    self.wasm.appendSlice(bs) catch oom();
}

fn emitLebU32(self: *Self, i: u32) void {
    // https://webassembly.github.io/spec/core/binary/values.html#integers
    var n = i;
    while (true) {
        const chunk = @as(u8, @truncate(n & 0b0111_1111));
        n = n >> 7;
        const encoded_chunk = if (n == 0) chunk else chunk | 0b1000_0000;
        self.wasm.append(encoded_chunk) catch oom();
        if (n == 0) break;
    }
}

fn emitLebU64(self: *Self, i: u64) void {
    // https://webassembly.github.io/spec/core/binary/values.html#integers
    var n = i;
    while (true) {
        const chunk = @as(u8, @truncate(n & 0b0111_1111));
        n = n >> 7;
        const encoded_chunk = if (n == 0) chunk else chunk | 0b1000_0000;
        self.wasm.append(encoded_chunk) catch oom();
        if (n == 0) break;
    }
}

fn emitLebI64(self: *Self, i: i64) void {
    self.emitLebU64(@bitCast(i));
}

fn emitName(self: *Self, string: []const u8) void {
    self.emitLebU32(@intCast(string.len));
    self.emitBytes(string);
}

const bytesForU32Max = 5; // ceil(32/7)

// Write zeroes here and fix it up later.
fn emitByteCountLater(self: *Self) usize {
    const ix = self.wasm.items.len;
    self.wasm.appendNTimes(0, bytesForU32Max) catch oom();
    return ix;
}

// Fix up some zeroes that we wrote earlier.
fn emitByteCount(self: *Self, ix: usize) void {
    const byte_count = self.wasm.items.len - ix - bytesForU32Max;

    // Same as emitLebU32, but overwriting existing zeroes.
    var n = byte_count;
    var offset: usize = 0;
    while (n >= (1 << 7)) {
        self.wasm.items[ix + offset] = @as(u8, @truncate(n)) | (1 << 7);
        n = n >> 7;
        offset += 1;
    }
    self.wasm.items[ix + offset] = @truncate(n);

    // Fix up trailing zeroes so the whole thing decodes as one number.
    while (offset < bytesForU32Max - 1) {
        self.wasm.items[ix + offset] |= 1 << 7;
        offset += 1;
    }
}

fn emitSectionStart(self: *Self, section: u8) usize {
    self.emitByte(section);
    return self.emitByteCountLater();
}

fn emitSectionEnd(self: *Self, ix: usize) void {
    self.emitByteCount(ix);
}

fn emitLenOf(self: *Self, slice: anytype) void {
    self.emitLebU32(@intCast(slice.len));
}

fn emitInstruction(self: *Self, instruction: Instruction) void {
    switch (instruction) {
        .i64 => |num| {
            self.emitByte(0x42);
            self.emitLebI64(num);
        },
    }
}

fn fail(self: *Self, comptime message: []const u8, args: anytype) error{CompileError} {
    self.error_message = std.fmt.allocPrint(self.allocator, message, args) catch oom();
    return error.CompileError;
}
