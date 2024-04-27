const std = @import("std");
const panic = std.debug.panic;
const assert = std.debug.assert;
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const wasm = std.wasm;

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

pub const deepEqual = @import("deep.zig").deepEqual;
pub const deepHash = @import("deep.zig").deepHash;

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

// Syntax IR
pub const SirExpr = struct { id: usize };

pub const SirExprData = union(enum) {
    // TODO Replace i32/f32 with bigInt/bigDec
    i32: i32,
    f32: f32,
    string: []const u8,
    object: SirObject,
    name: []const u8,
    builtin: Builtin,
    mut: SirExpr,
    let_or_set: struct {
        path: SirExpr,
        value: SirExpr,
    },
    get: struct {
        object: SirExpr,
        key: SirExpr,
    },
    @"if": struct {
        cond: SirExpr,
        then: SirExpr,
        @"else": SirExpr,
    },
    @"while": struct {
        cond: SirExpr,
        body: SirExpr,
    },
    fun: struct {
        params: SirObject,
        body: SirExpr,
    },
    make: struct {
        head: SirExpr,
        args: SirObject,
    },
    call: struct {
        head: SirExpr,
        args: SirObject,
    },
    block: []SirExpr,
};

pub const SirObject = struct {
    keys: []SirExpr,
    values: []SirExpr,
};

// Dynamic IR

pub const DirLocal = struct { id: usize };

pub const DirLocalData = struct {};

pub const DirExpr = struct { id: usize };

pub const DirExprData = union(enum) {
    i32: i32,
    f32: f32,
    string: []const u8,
    struct_init: usize,
    fun_init: struct {
        fun: DirFun,
    },
    arg,
    closure,
    local_get: DirLocal,
    local_set: DirLocal,
    object_get,
    call,
    drop,
    block_begin: struct {
        expr_count: usize,
    },
    block_end: struct {
        expr_count: usize,
    },
    @"return",
    stage, // Stage the following block.
    unstage, // Unstage the following expr.
};

// Push in order.
// Pop in reverse order.
pub fn DirExprInput(comptime T: type) type {
    return union(std.meta.Tag(DirExprData)) {
        i32,
        f32,
        string,
        struct_init: struct {
            keys: []Value,
            values: []T,
        },
        fun_init: struct {
            closure: T,
        },
        arg,
        closure,
        local_get,
        local_set: struct {
            value: T,
        },
        object_get: struct {
            object: T,
            key: Value,
        },
        call: struct {
            fun: T,
            args: T,
        },
        drop: struct {
            value: T,
        },
        block_begin,
        block_end,
        @"return": struct {
            value: T,
        },
        stage,
        unstage,
    };
}

pub fn DirExprOutput(comptime T: type) type {
    return union(std.meta.Tag(DirExprData)) {
        i32: T,
        f32: T,
        string: T,
        struct_init: T,
        fun_init: T,
        arg: T,
        closure: T,
        local_get: T,
        local_set,
        object_get: T,
        call: T,
        drop,
        block_begin,
        block_end,
        @"return",
        stage: T,
        unstage: T,
    };
}

pub const DirFun = struct { id: usize };

pub const DirFunData = struct {
    closure_keys_index: Map([]const u8, void),
    closure_keys: ArrayList([]const u8),

    local_data: List(DirLocal, DirLocalData),

    expr_data: List(DirExpr, DirExprData),

    pub fn init(allocator: Allocator) DirFunData {
        return .{
            .closure_keys_index = fieldType(DirFunData, .closure_keys_index).init(allocator),
            .closure_keys = fieldType(DirFunData, .closure_keys).init(allocator),

            .local_data = fieldType(DirFunData, .local_data).init(allocator),

            .expr_data = fieldType(DirFunData, .expr_data).init(allocator),
        };
    }
};

pub const Scope = struct {
    closure_until_len: usize,
    staged_until_len: usize,
    bindings: ArrayList(Binding),

    pub fn init(allocator: Allocator) Scope {
        return .{
            .closure_until_len = 0,
            .staged_until_len = 0,
            .bindings = fieldType(Scope, .bindings).init(allocator),
        };
    }

    pub fn push(self: *Scope, binding: Binding) void {
        self.bindings.append(binding) catch oom();
    }

    pub fn save(self: *Scope) usize {
        return self.bindings.items.len;
    }

    pub fn restore(self: *Scope, saved: usize) void {
        self.bindings.shrinkRetainingCapacity(saved);
    }

    pub fn lookup(self: *Scope, name: []const u8) ?BindingInfo {
        var i: usize = self.bindings.items.len;
        while (i > 0) : (i -= 1) {
            const binding = self.bindings.items[i - 1];
            if (std.mem.eql(u8, binding.name, name)) {
                return BindingInfo{
                    .name = binding.name,
                    .value = if (i - 1 < self.closure_until_len)
                        .{ .closure = binding.name }
                    else
                        binding.value,
                    .is_staged = i - 1 < self.staged_until_len,
                };
            }
        }
        return null;
    }
};

pub const Binding = struct {
    name: []const u8,
    value: AbstractValue,
};

pub const BindingInfo = struct {
    name: []const u8,
    value: AbstractValue,
    is_staged: bool,
};

pub const AbstractValue = union(enum) {
    arg,
    closure: []const u8,
    local: DirLocal,
};

pub const DirFrame = struct {
    fun: DirFun,
    arg: Value,
    closure: Value,
    expr: DirExpr = .{ .id = 0 },
};

// Typed IR

pub fn FlatLattice(comptime T: type) type {
    return union(enum) {
        zero,
        one: T,
        many: T,
    };
}

pub const TirLocal = struct { id: usize };

pub const TirLocalData = struct {
    repr: FlatLattice(Repr),
};

pub const TirExpr = struct { id: usize };

pub const TirExprData = union(enum) {
    i32: i32,
    f32: f32,
    string: []const u8,
    struct_init,
    fun_init,
    arg,
    closure,
    local_get: TirLocal,
    local_set: TirLocal,
    object_get: struct {
        key: Value,
    },
    call: TirFun,
    drop,
    block_begin: struct {
        expr_count: usize,
    },
    block_end: struct {
        expr_count: usize,
    },
    @"return",
};

pub const TirFunKey = struct {
    fun: DirFun,
    closure_repr: Repr,
    arg_repr: Repr,
};

pub const TirFun = struct { id: usize };

pub const TirFunData = struct {
    local_data: List(TirLocal, TirLocalData),

    expr_data: List(TirExpr, TirExprData),
    expr_repr: List(TirExpr, ?Repr), // null for exprs that don't return a value

    return_repr: FlatLattice(Repr),

    pub fn init(allocator: Allocator) TirFunData {
        return .{
            .local_data = fieldType(TirFunData, .local_data).init(allocator),

            .expr_data = fieldType(TirFunData, .expr_data).init(allocator),
            .expr_repr = fieldType(TirFunData, .expr_repr).init(allocator),

            .return_repr = .zero,
        };
    }
};

pub const TirFrame = struct {
    key: TirFunKey,
    fun: TirFun,
    expr: DirExpr,
};

pub const Compiler = struct {
    allocator: Allocator,
    source: []const u8,

    // tokenize
    token_data: List(Token, TokenData),
    token_to_source: List(Token, [2]usize),

    // parse
    token_next: Token,
    sir_expr_data: List(SirExpr, SirExprData),

    // lower
    scope: Scope,
    dir_fun_data: List(DirFun, DirFunData),
    dir_fun_main: ?DirFun,

    // eval
    dir_frame_stack: ArrayList(DirFrame),
    value_stack: ArrayList(Value),
    local_stack: ArrayList(Value),

    // infer
    tir_fun_data: List(TirFun, TirFunData),
    tir_fun_by_key: Map(TirFunKey, TirFun),
    tir_fun_main: ?TirFun,
    tir_frame_stack: ArrayList(TirFrame),
    repr_stack: ArrayList(Repr),

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

pub const ErrorData = union(enum) {
    tokenize: TokenizeErrorData,
    parse: ParseErrorData,
    lower: struct {
        expr: SirExpr,
        data: LowerErrorData,
    },
    eval: struct {
        fun: DirFun,
        expr: DirExpr,
        data: EvalErrorData,
    },
    infer: struct {
        key: TirFunKey,
        fun: TirFun,
        expr: DirExpr,
        data: InferErrorData,
    },
};
pub const TokenizeErrorData = @import("./tokenize.zig").TokenizeErrorData;
pub const ParseErrorData = @import("./parse.zig").ParseErrorData;
pub const LowerErrorData = @import("./lower.zig").LowerErrorData;
pub const EvalErrorData = @import("./eval.zig").EvalErrorData;
pub const InferErrorData = @import("./infer.zig").InferErrorData;

pub fn formatError(c: *Compiler) []const u8 {
    if (c.error_data) |error_data|
        switch (error_data) {
            .lower => |err| {
                const expr_data = c.sir_expr_data.get(err.expr);
                return switch (err.data) {
                    .invalid_pattern => format(c, "Invalid pattern: {}", .{expr_data}),
                    .name_not_in_scope => format(c, "Name not in scope: {s}", .{expr_data.name}),
                    .invalid_let_path => format(c, "Invalid let path: {}", .{expr_data}),
                    .todo => format(c, "TODO lower: {}", .{expr_data}),
                };
            },
            .eval => |err| {
                const expr_data = c.dir_fun_data.get(err.fun).expr_data.get(err.expr);
                return switch (err.data) {
                    .key_not_found => |data| format(c, "Key {} not found in {}", .{ data.key, data.object }),
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
            else => return format(c, "{}", .{c.error_data.?}),
        }
    else
        return "ok";
}

pub fn format(c: *Compiler, comptime message: []const u8, args: anytype) []const u8 {
    return std.fmt.allocPrint(c.allocator, message, args) catch oom();
}

pub const Repr = @import("./repr.zig").Repr;
pub const ReprStruct = @import("./repr.zig").ReprStruct;
pub const ReprUnion = @import("./repr.zig").ReprUnion;
pub const ReprFun = @import("./repr.zig").ReprFun;

pub const Value = @import("./value.zig").Value;
pub const ValueStruct = @import("./value.zig").ValueStruct;
pub const ValueUnion = @import("./value.zig").ValueUnion;
pub const ValueFun = @import("./value.zig").ValueFun;

pub const tokenize = @import("./tokenize.zig").tokenize;
pub const parse = @import("./parse.zig").parse;
pub const lower = @import("./lower.zig").lower;
pub const evalMain = @import("./eval.zig").evalMain;
pub const inferMain = @import("./infer.zig").inferMain;
pub const generate = @import("./generate.zig").generate;

pub fn compileLax(c: *Compiler) error{ TokenizeError, ParseError, LowerError }!void {
    try tokenize(c);
    assert(c.token_data.count() == c.token_to_source.count());

    try parse(c);
    assert(c.token_next.id == c.token_data.count());

    try lower(c);
    assert(c.dir_fun_main != null);
}

pub fn compileStrict(c: *Compiler) error{ EvalError, InferError, GenerateError }!void {
    assert(c.dir_fun_main != null);

    try inferMain(c);
    assert(c.tir_fun_main != null);

    try generate(c);
    assert(c.wasm.items.len != 0);
}
