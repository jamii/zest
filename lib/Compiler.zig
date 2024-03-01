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
const Place = Analyzer.Place;
const Repr = @import("./Semantalyzer.zig").Repr;

const Self = @This();
allocator: Allocator,
parser: Parser,
analyzer: Analyzer,
scope: Scope,
wasm: ArrayList(u8),
error_message: ?[]const u8,

const Scope = ArrayList(Binding);

const Binding = struct {
    name: []const u8,
    expr_id: ExprId,
};

const ValType = enum {
    i32,
    i64,
};

const wasm_page_size = 64 << 10;

pub fn init(allocator: Allocator, parser: Parser, analyzer: Analyzer) Self {
    return .{
        .allocator = allocator,
        .parser = parser,
        .analyzer = analyzer,
        .scope = Scope.init(allocator),
        .wasm = ArrayList(u8).init(allocator),
        .error_message = null,
    };
}

pub fn compile(self: *Self) error{CompileError}![]u8 {
    // Magic
    self.emitBytes(&.{ 0x00, 0x61, 0x73, 0x6D });

    // Version
    self.emitBytes(&.{ 0x01, 0x00, 0x00, 0x00 });

    // Function types
    {
        const section = self.emitSectionStart(1);
        defer self.emitSectionEnd(section);

        self.emitLenOf(self.analyzer.functions.items);
        for (self.analyzer.functions.items) |function| {
            // Fn type.
            self.emitByte(0x60);
            // Param types.
            self.emitLebU32(@intCast(1 + function.params.len));
            self.emitValType(.i32);
            for (function.params) |_|
                self.emitValType(.i32);
            // Result types.
            self.emitLebU32(0);
        }
    }

    // Functions
    {
        const section = self.emitSectionStart(3);
        defer self.emitSectionEnd(section);

        self.emitLenOf(self.analyzer.functions.items);
        for (0..self.analyzer.functions.items.len) |function_ix| {
            // Function i has type i
            self.emitLebU32(@intCast(function_ix));
        }
    }

    // Memory
    {
        const section = self.emitSectionStart(5);
        defer self.emitSectionEnd(section);

        // Number of memories.
        self.emitLebU32(1);
        // No maximum.
        self.emitByte(0x00);
        // At minimum enough memory for the stack.
        self.emitLebU32(@intCast(@divExact(self.analyzer.stack_offset_max, wasm_page_size)));
    }

    // Globals
    {
        const section = self.emitSectionStart(6);
        defer self.emitSectionEnd(section);

        // Number of globals
        self.emitLebU32(1);

        // stack_pointer: i32 mut = analyzer.stack_offset_max
        self.emitValType(.i32);
        self.emitByte(0x01);
        self.emitI32Const(@intCast(self.analyzer.stack_offset_max));
        self.emitEnd();
    }

    // Exports
    {
        const section = self.emitSectionStart(7);
        defer self.emitSectionEnd(section);

        // Number of exports.
        self.emitLebU32(2);

        // main = fn 0
        self.emitName("main");
        self.emitByte(0x00);
        self.emitLebU32(0);

        // memory = mem 0
        self.emitName("memory");
        self.emitByte(0x02);
        self.emitLebU32(0);
    }

    // Code
    {
        const section = self.emitSectionStart(10);
        defer self.emitSectionEnd(section);

        self.emitLenOf(self.analyzer.functions.items);
        for (self.analyzer.functions.items) |function| {
            const start = self.emitByteCountLater();
            defer self.emitByteCount(start);

            self.emitLebU32(function.locals_count);

            // Push frame
            self.emitGlobalGet(0);
            self.emitU32Const(@intCast(function.frame_size));
            self.emitSub(.i32);
            self.emitGlobalSet(0);

            try self.compileExpr(function.body);

            // TODO temporary hack so we can read the result from test.js
            const src = self.analyzer.places[function.body].?;
            const dest = Place{ .base = .shadow, .offset = @as(u32, @intCast(function.frame_size)) - 8, .length = 8 };
            self.emitCopy(dest, src);

            // Pop frame
            self.emitGlobalGet(0);
            self.emitU32Const(@intCast(function.frame_size));
            self.emitAdd(.i32);
            self.emitGlobalSet(0);

            self.emitEnd();
        }
    }

    return self.wasm.items;
}

fn compileExpr(self: *Self, expr_id: ExprId) error{CompileError}!void {
    const expr = self.parser.exprs.items[expr_id];
    const repr = self.analyzer.reprs[expr_id].?;
    const dest = self.analyzer.places[expr_id].?;
    switch (expr) {
        .i64 => |num| {
            self.emitPlaceBase(dest);
            self.emitI64Const(num);
            self.emitStore(.i64, dest);
        },
        .object => |object| {
            if (repr != .@"struct") return self.fail("TODO Can't compile {}", .{expr});
            for (object.values) |value| try self.compileExpr(value);
        },
        .name => {
            const src = self.analyzer.places[self.analyzer.name_lets[expr_id].?].?;
            self.emitCopy(dest, src);
        },
        .let => |let| {
            try self.compileExpr(let.value);
        },
        .set => |set| {
            try self.compileExpr(set.value);
            self.emitCopy(self.analyzer.places[set.path].?, self.analyzer.places[set.value].?);
        },
        .call => |call| {
            const head = self.parser.exprs.items[call.head];
            if (head != .builtin) return self.fail("TODO Can't compile {}", .{expr});
            switch (head.builtin) {
                .equal,
                .less_than,
                .less_than_or_equal,
                .more_than,
                .more_than_or_equal,
                .add,
                .subtract,
                => {
                    const val_type: ValType = switch (repr) {
                        .i64 => .i64,
                        else => return self.fail("TODO Can't compile {}", .{head.builtin}),
                    };
                    try self.compileExpr(call.args.values[0]);
                    try self.compileExpr(call.args.values[1]);
                    self.emitPlaceBase(dest);
                    self.emitLoad(val_type, self.analyzer.places[call.args.values[0]].?);
                    self.emitLoad(val_type, self.analyzer.places[call.args.values[1]].?);
                    switch (head.builtin) {
                        .equal => self.emitEqual(val_type),
                        .less_than => self.emitLessThan(val_type),
                        .less_than_or_equal => self.emitLessThanOrEqual(val_type),
                        .more_than => self.emitMoreThan(val_type),
                        .more_than_or_equal => self.emitMoreThanOrEqual(val_type),
                        .add => self.emitAdd(val_type),
                        .subtract => self.emitSubtract(val_type),
                        else => unreachable,
                    }
                    switch (head.builtin) {
                        .equal, .less_than, .less_than_or_equal, .more_than, .more_than_or_equal => {
                            self.emitU32ToI64();
                        },
                        .add, .subtract => {},
                        else => unreachable,
                    }
                    self.emitStore(val_type, dest);
                },
                else => return self.fail("TODO Can't compile {}", .{head.builtin}),
            }
        },
        .get => |get| {
            try self.compileExpr(get.object);
            const object_repr = self.analyzer.reprs[get.object].?;
            const key = self.analyzer.constants[get.key].?;
            const src = object_repr.@"struct".placeOf(self.analyzer.places[get.object].?, key).?;
            self.emitCopy(dest, src);
        },
        .statements => |statements| {
            for (statements) |statement| {
                try self.compileExpr(statement);
            }
        },
        else => return self.fail("TODO Can't compile {}", .{expr}),
    }
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
        const is_last_chunk = n == 0;
        const chunk = @as(u8, @truncate(n & 0b0111_1111));
        n = n >> 7;
        const encoded_chunk = if (is_last_chunk) chunk else chunk | 0b1000_0000;
        self.wasm.append(encoded_chunk) catch oom();
        if (is_last_chunk) break;
    }
}

fn emitLebI32(self: *Self, i: i32) void {
    self.emitLebU32(@bitCast(i));
}

fn emitLebU64(self: *Self, i: u64) void {
    // https://webassembly.github.io/spec/core/binary/values.html#integers
    var n = i;
    while (true) {
        const is_last_chunk = n == 0;
        const chunk = @as(u8, @truncate(n & 0b0111_1111));
        n = n >> 7;
        const encoded_chunk = if (is_last_chunk) chunk else chunk | 0b1000_0000;
        self.wasm.append(encoded_chunk) catch oom();
        if (is_last_chunk) break;
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

fn emitValType(self: *Self, val_type: ValType) void {
    switch (val_type) {
        .i32 => self.emitByte(0x7F),
        .i64 => self.emitByte(0x7E),
    }
}

fn emitI32Const(self: *Self, num: i32) void {
    self.emitByte(0x41);
    self.emitLebI32(num);
}

fn emitU32Const(self: *Self, num: u32) void {
    self.emitI32Const(@bitCast(num));
}

fn emitI64Const(self: *Self, num: i64) void {
    self.emitByte(0x42);
    self.emitLebI64(num);
}

fn emitGlobalGet(self: *Self, global: u32) void {
    self.emitByte(0x23);
    self.emitLebU32(global);
}

fn emitGlobalSet(self: *Self, global: u32) void {
    self.emitByte(0x24);
    self.emitLebU32(global);
}

fn emitLocalGet(self: *Self, local: u32) void {
    self.emitByte(0x20);
    self.emitLebU32(local);
}

fn emitLocalSet(self: *Self, local: u32) void {
    self.emitByte(0x21);
    self.emitLebU32(local);
}

fn emitEqual(self: *Self, val_type: ValType) void {
    switch (val_type) {
        .i32 => self.emitByte(0x46),
        .i64 => self.emitByte(0x51),
    }
}

fn emitLessThan(self: *Self, val_type: ValType) void {
    switch (val_type) {
        .i32 => self.emitByte(0x48),
        .i64 => self.emitByte(0x53),
    }
}

fn emitLessThanOrEqual(self: *Self, val_type: ValType) void {
    switch (val_type) {
        .i32 => self.emitByte(0x4C),
        .i64 => self.emitByte(0x57),
    }
}

fn emitMoreThan(self: *Self, val_type: ValType) void {
    switch (val_type) {
        .i32 => self.emitByte(0x4A),
        .i64 => self.emitByte(0x55),
    }
}

fn emitMoreThanOrEqual(self: *Self, val_type: ValType) void {
    switch (val_type) {
        .i32 => self.emitByte(0x4E),
        .i64 => self.emitByte(0x59),
    }
}

fn emitAdd(self: *Self, val_type: ValType) void {
    switch (val_type) {
        .i32 => self.emitByte(0x6A),
        .i64 => self.emitByte(0x7C),
    }
}

fn emitSub(self: *Self, val_type: ValType) void {
    switch (val_type) {
        .i32 => self.emitByte(0x6B),
        .i64 => self.emitByte(0x7D),
    }
}

fn emitSubtract(self: *Self, val_type: ValType) void {
    switch (val_type) {
        .i32 => self.emitByte(0x6B),
        .i64 => self.emitByte(0x7D),
    }
}

fn emitU32ToI64(self: *Self) void {
    self.emitByte(0xAD);
}

fn emitLoad(self: *Self, val_type: ValType, place: Place) void {
    self.emitPlaceBase(place);
    const alignment = 0; // TODO aligment
    switch (val_type) {
        .i32 => self.emitByte(0x28),
        .i64 => self.emitByte(0x29),
    }
    self.emitLebU32(alignment);
    self.emitLebU32(place.offset);
}

fn emitPlaceBase(self: *Self, place: Place) void {
    switch (place.base) {
        .result => self.emitLocalGet(0),
        .shadow => self.emitGlobalGet(0),
    }
}

fn emitPlace(self: *Self, place: Place) void {
    self.emitPlaceBase(place);
    self.emitU32Const(place.offset);
    self.emitAdd(.i32);
}

// Expects (base, value) on stack.
fn emitStore(self: *Self, val_type: ValType, place: Place) void {
    const alignment = 0; // TODO aligment
    switch (val_type) {
        .i32 => self.emitByte(0x36),
        .i64 => self.emitByte(0x37),
    }
    self.emitLebU32(alignment);
    self.emitLebU32(place.offset);
}

fn emitCopy(self: *Self, dest: Place, src: Place) void {
    assert(src.length == dest.length);
    if (src.equal(dest)) return;
    self.emitPlace(dest);
    self.emitPlace(src);
    self.emitU32Const(src.length);
    // memory.copy(mem 0 => mem 0)
    self.emitByte(0xFC);
    self.emitLebU32(10);
    self.emitByte(0x00);
    self.emitByte(0x00);
}

fn emitEnd(self: *Self) void {
    self.emitByte(0x0B);
}

fn fail(self: *Self, comptime message: []const u8, args: anytype) error{CompileError} {
    self.error_message = std.fmt.allocPrint(self.allocator, message, args) catch oom();
    return error.CompileError;
}
