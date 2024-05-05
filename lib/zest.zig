const std = @import("std");
const panic = std.debug.panic;
const assert = std.debug.assert;
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const wasm = std.wasm;

pub const deepEqual = @import("deep.zig").deepEqual;
pub const deepHash = @import("deep.zig").deepHash;

pub const Repr = @import("./repr.zig").Repr;
pub const ReprStruct = @import("./repr.zig").ReprStruct;
pub const ReprUnion = @import("./repr.zig").ReprUnion;
pub const ReprFun = @import("./repr.zig").ReprFun;

pub const Value = @import("./value.zig").Value;
pub const ValueStruct = @import("./value.zig").ValueStruct;
pub const ValueUnion = @import("./value.zig").ValueUnion;
pub const ValueFun = @import("./value.zig").ValueFun;

pub const sir = @import("./sir.zig");
pub const dir = @import("./dir.zig");
pub const tir = @import("./tir.zig");
pub const wir = @import("./wir.zig");

pub const tokenize = @import("./tokenize.zig").tokenize;
pub const parse = @import("./parse.zig").parse;
pub const desugar = @import("./desugar.zig").desugar;
pub const evalMain = @import("./eval.zig").evalMain;
pub const inferMain = @import("./infer.zig").inferMain;
pub const lower = @import("./lower.zig").lower;
pub const generate = @import("./generate.zig").generate;

pub fn oom() noreturn {
    panic("OOM", .{});
}

pub fn fieldType(comptime T: type, comptime field_enum: std.meta.FieldEnum(T)) type {
    return std.meta.fieldInfo(T, field_enum).type;
}

pub fn List(comptime K: type, comptime V: type) type {
    return struct {
        data: ArrayList(V),

        const Self = @This();

        pub fn init(allocator: Allocator) Self {
            return .{ .data = ArrayList(V).init(allocator) };
        }

        pub fn append(self: *Self, value: V) K {
            const id = self.data.items.len;
            self.data.append(value) catch oom();
            return .{ .id = id };
        }

        pub fn appendSlice(self: *Self, value: []const V) void {
            self.data.appendSlice(value) catch oom();
        }

        pub fn appendNTimes(self: *Self, value: V, n: usize) void {
            self.data.appendNTimes(value, n) catch oom();
        }

        pub fn get(self: Self, key: K) V {
            return self.data.items[key.id];
        }

        pub fn getPtr(self: Self, key: K) *V {
            return &self.data.items[key.id];
        }

        pub fn items(self: Self) []V {
            return self.data.items;
        }

        pub fn count(self: Self) usize {
            return self.data.items.len;
        }

        pub fn firstKey(self: Self) ?K {
            return if (self.data.items.len == 0) null else .{ .id = 0 };
        }

        pub fn lastKey(self: Self) ?K {
            return if (self.data.items.len == 0) null else .{ .id = self.data.items.len - 1 };
        }

        pub fn lastValue(self: Self) ?V {
            return if (self.lastKey()) |key| self.get(key) else null;
        }
    };
}

pub fn Map(comptime K: type, comptime V: type) type {
    return std.HashMap(K, V, struct {
        const Self = @This();
        pub fn hash(_: Self, pseudo_key: K) u64 {
            return deepHash(pseudo_key);
        }
        pub fn eql(_: Self, pseudo_key: K, key: K) bool {
            return deepEqual(pseudo_key, key);
        }
    }, std.hash_map.default_max_load_percentage);
}

pub const Token = struct { id: usize };

pub const TokenData = enum {
    number,
    string,
    name,
    @"if",
    @"else",
    @"while",
    @"@",
    @"(",
    @")",
    @"[",
    @"]",
    @"{",
    @"}",
    @",",
    @".",
    @":",
    @";",
    @"=",
    @"==",
    @"~=",
    @"<",
    @"<=",
    @">",
    @">=",
    @"+",
    @"-",
    @"/",
    @"*",
    comment,
    space,
    newline,
    eof,
};

pub const Builtin = enum {
    // binary ops
    equal,
    equivalent,
    less_than,
    less_than_or_equal,
    more_than,
    more_than_or_equal,
    add,
    subtract,
    multiply,
    divide,
};

pub fn FlatLattice(comptime T: type) type {
    return union(enum) {
        zero,
        one: T,
        many: T,
    };
}

pub const Compiler = struct {
    allocator: Allocator,
    source: []const u8,

    // tokenize
    token_data: List(Token, TokenData),
    token_to_source: List(Token, [2]usize),

    // parse
    token_next: Token,
    sir_expr_data: List(sir.Expr, sir.ExprData),

    // desugar
    scope: dir.Scope,
    dir_fun_data: List(dir.Fun, dir.FunData),
    dir_fun_main: ?dir.Fun,

    // eval
    dir_frame_stack: ArrayList(dir.Frame),
    value_stack: ArrayList(Value),
    local_stack: ArrayList(Value),

    // infer
    tir_fun_data: List(tir.Fun, tir.FunData),
    tir_fun_by_key: Map(tir.FunKey, tir.Fun),
    tir_fun_main: ?tir.Fun,
    tir_frame_stack: ArrayList(tir.Frame),
    repr_stack: ArrayList(Repr),

    // lower
    wir_fun_data: List(wir.Fun, wir.FunData),
    wir_fun_main: ?wir.Fun,
    constant_bytes: List(wir.Constant, []const u8),
    fun_type_memo: Map(wir.FunTypeData, wir.FunType),
    fun_type_data: List(wir.FunType, wir.FunTypeData),
    address_stack: ArrayList(?wir.Address), // null if on wasm stack
    local_address: List(wir.Local, wir.Address),

    // generate
    wasm: ArrayList(u8),

    error_data: ?ErrorData,

    pub fn init(allocator: Allocator, source: []const u8) Compiler {
        return .{
            .allocator = allocator,
            .source = source,

            .token_data = fieldType(Compiler, .token_data).init(allocator),
            .token_to_source = fieldType(Compiler, .token_to_source).init(allocator),

            .token_next = .{ .id = 0 },
            .sir_expr_data = fieldType(Compiler, .sir_expr_data).init(allocator),

            .scope = fieldType(Compiler, .scope).init(allocator),
            .dir_fun_data = fieldType(Compiler, .dir_fun_data).init(allocator),
            .dir_fun_main = null,

            .dir_frame_stack = fieldType(Compiler, .dir_frame_stack).init(allocator),
            .value_stack = fieldType(Compiler, .value_stack).init(allocator),
            .local_stack = fieldType(Compiler, .local_stack).init(allocator),

            .tir_fun_data = fieldType(Compiler, .tir_fun_data).init(allocator),
            .tir_fun_by_key = fieldType(Compiler, .tir_fun_by_key).init(allocator),
            .tir_fun_main = null,
            .tir_frame_stack = fieldType(Compiler, .tir_frame_stack).init(allocator),
            .repr_stack = fieldType(Compiler, .repr_stack).init(allocator),

            .wir_fun_data = fieldType(Compiler, .wir_fun_data).init(allocator),
            .wir_fun_main = null,
            .constant_bytes = fieldType(Compiler, .constant_bytes).init(allocator),
            .fun_type_memo = fieldType(Compiler, .fun_type_memo).init(allocator),
            .fun_type_data = fieldType(Compiler, .fun_type_data).init(allocator),
            .address_stack = fieldType(Compiler, .address_stack).init(allocator),
            .local_address = fieldType(Compiler, .local_address).init(allocator),

            .wasm = fieldType(Compiler, .wasm).init(allocator),

            .error_data = null,
        };
    }

    pub fn box(c: *Compiler, value: anytype) *@TypeOf(value) {
        const ptr = c.allocator.create(@TypeOf(value)) catch oom();
        ptr.* = value;
        return ptr;
    }

    pub fn dupeOne(c: *Compiler, value: anytype) []@TypeOf(value) {
        return c.allocator.dupe(@TypeOf(value), &[1]@TypeOf(value){value}) catch oom();
    }
};

pub const TokenizeErrorData = @import("./tokenize.zig").TokenizeErrorData;
pub const ParseErrorData = @import("./parse.zig").ParseErrorData;
pub const DesugarErrorData = @import("./desugar.zig").DesugarErrorData;
pub const EvalErrorData = @import("./eval.zig").EvalErrorData;
pub const InferErrorData = @import("./infer.zig").InferErrorData;
pub const LowerErrorData = @import("./lower.zig").LowerErrorData;
pub const ErrorData = union(enum) {
    tokenize: TokenizeErrorData,
    parse: ParseErrorData,
    desugar: struct {
        expr: sir.Expr,
        data: DesugarErrorData,
    },
    eval: struct {
        fun: dir.Fun,
        expr: dir.Expr,
        data: EvalErrorData,
    },
    infer: struct {
        key: tir.FunKey,
        fun: tir.Fun,
        expr: dir.Expr,
        data: InferErrorData,
    },
    lower: struct {
        data: LowerErrorData,
    },
};

pub fn formatError(c: *Compiler) []const u8 {
    if (c.error_data) |error_data|
        switch (error_data) {
            .desugar => |err| {
                const expr_data = c.sir_expr_data.get(err.expr);
                return switch (err.data) {
                    .invalid_pattern => format(c, "Invalid pattern: {}", .{expr_data}),
                    .name_not_bound => |data| format(c, "Name not bound: {s}", .{data.name}),
                    .name_already_bound => |data| format(c, "Name already bound: {s}", .{data.name}),
                    .invalid_let_path => format(c, "Invalid let path: {}", .{expr_data}),
                    .todo => format(c, "TODO desugar: {}", .{expr_data}),
                };
            },
            .eval => |err| {
                const expr_data = c.dir_fun_data.get(err.fun).expr_data.get(err.expr);
                return switch (err.data) {
                    .key_not_found => |data| format(c, "Key {} not found in {}", .{ data.key, data.object }),
                    .not_an_object => |data| format(c, "Not an object: {}", .{data}),
                    .not_a_fun => |data| format(c, "Not a function: {}", .{data}),
                    .cannot_stage_expr => format(c, "Cannot stage expr", .{}),
                    .cannot_unstage_value => |data| format(c, "Cannot unstage value: {}", .{data}),
                    .todo => format(c, "TODO eval: {}", .{expr_data}),
                };
            },
            .infer => |err| {
                const expr_data = c.dir_fun_data.get(err.key.fun).expr_data.get(err.expr);
                return switch (err.data) {
                    .value_not_staged => |data| format(c, "Value not staged: {}", .{data}),
                    .type_error => |data| format(c, "Expected {}, found {}", .{ data.expected, data.found }),
                    .not_an_object => |data| format(c, "Not an object: {}", .{data}),
                    .key_not_found => |data| format(c, "Key {} not found in {}", .{ data.key, data.object }),
                    .not_a_fun => |data| format(c, "Not a function: {}", .{data}),
                    .todo => format(c, "TODO infer: {}", .{expr_data}),
                };
            },
            .lower => |err| {
                return switch (err.data) {
                    .todo => format(c, "TODO lower", .{}),
                };
            },
            else => return format(c, "{}", .{c.error_data.?}),
        }
    else
        return "ok";
}

pub fn format(c: *Compiler, comptime message: []const u8, args: anytype) []const u8 {
    return std.fmt.allocPrint(c.allocator, message, args) catch oom();
}

pub fn compileLax(c: *Compiler) error{ TokenizeError, ParseError, DesugarError }!void {
    try tokenize(c);
    assert(c.token_data.count() == c.token_to_source.count());

    try parse(c);
    assert(c.token_next.id == c.token_data.count());

    try desugar(c);
    assert(c.dir_fun_main != null);
}

pub fn compileStrict(c: *Compiler) error{ EvalError, InferError, LowerError, GenerateError }!void {
    assert(c.dir_fun_main != null);

    try inferMain(c);
    assert(c.tir_fun_main != null);

    //try lower(c);
    //assert(c.wir_fun_main != null);

    //try generate(c);
    //assert(c.wasm.items.len != 0);
}
