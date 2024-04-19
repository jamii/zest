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

    // syntax
    get,
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
    @"if": struct {
        cond: SirExpr,
        then: SirExpr,
        @"else": SirExpr,
    },
    @"while": struct {
        cond: SirExpr,
        body: SirExpr,
    },
    @"fn": struct {
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
    arg, // Argument to the current function.
    local_get: DirLocal,
    local_set: DirLocal,
    object_get,
    drop,
    @"return",
};

// Push in order.
// Pop in reverse order.
pub const DirExprInput = union(std.meta.Tag(DirExprData)) {
    i32,
    f32,
    string,
    arg,
    local_get,
    local_set: struct {
        value: Value,
    },
    object_get: struct {
        object: Value,
        key: Value,
    },
    drop: struct {
        value: Value,
    },
    @"return": struct {
        value: Value,
    },
};

pub const DirExprOutput = union(std.meta.Tag(DirExprData)) {
    i32: struct {
        value: Value,
    },
    f32: struct {
        value: Value,
    },
    string: struct {
        value: Value,
    },
    arg: struct {
        value: Value,
    },
    local_get: struct {
        value: Value,
    },
    local_set,
    object_get: struct {
        value: Value,
    },
    drop,
    @"return",
};

pub const DirFun = struct { id: usize };

pub const DirFunData = struct {
    local_data: List(DirLocal, DirLocalData),

    expr_data: List(DirExpr, DirExprData),

    pub fn init(allocator: Allocator) DirFunData {
        return .{
            .local_data = fieldType(DirFunData, .local_data).init(allocator),

            .expr_data = fieldType(DirFunData, .expr_data).init(allocator),
        };
    }
};

pub const Scope = struct {
    bindings: ArrayList(Binding),

    pub fn init(allocator: Allocator) Scope {
        return .{
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

    pub fn lookup(self: *Scope, name: []const u8) ?Binding {
        var i: usize = self.bindings.items.len;
        while (i > 0) : (i -= 1) {
            const binding = self.bindings.items[i - 1];
            if (std.mem.eql(u8, binding.name, name)) {
                return binding;
            }
        }
        return null;
    }
};

pub const Binding = struct {
    name: []const u8,
    value: AbstractValue,
};

pub const AbstractValue = union(enum) {
    arg,
    local: DirLocal,
    //function: Function,
    builtin: Builtin,
};

pub const SpecializationArgs = struct {
    //function: Function,
    mode: enum { lax, strict },
    in_repr: Repr,
};

pub const Specialization = struct { id: usize };

pub const SpecializationData = struct {
    //function: Function,

    //local_repr: List(Local, Repr),

    //shadow_repr: List(Shadow, Repr),

    //node_data: List(Node, NodeData),
    //node_first: ?Node,
    //node_last: ?Node,
    //node_next: List(Node, ?Node),
    //node_prev: List(Node, ?Node),

    //in_repr: List(Arg, Repr),
    //out_repr: Repr,
    //node_repr: List(Node, Repr),

    pub fn init(
        //allocator: Allocator,
        //function: Function,
    ) SpecializationData {
        return .{
            //.function = function,

            //.local_repr = fieldType(SpecializationData, .local_repr).init(allocator),

            //.shadow_repr = fieldType(SpecializationData, .shadow_repr).init(allocator),

            //.node_data = fieldType(SpecializationData, .node_data).init(allocator),
            //.node_first = null,
            //.node_last = null,
            //.node_next = fieldType(SpecializationData, .node_next).init(allocator),
            //.node_prev = fieldType(SpecializationData, .node_prev).init(allocator),

            //.in_repr = fieldType(SpecializationData, .in_repr).init(allocator),
            //.out_repr = Repr.emptyUnion(),
            //.node_repr = fieldType(SpecializationData, .node_repr).init(allocator),
        };
    }
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

    args_to_specialization: Map(SpecializationArgs, ?Specialization),
    specialization_data: List(Specialization, SpecializationData),
    specialization_main: ?Specialization,

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

            .args_to_specialization = fieldType(Compiler, .args_to_specialization).init(allocator),
            .specialization_main = null,
            .specialization_data = fieldType(Compiler, .specialization_data).init(allocator),

            .wasm = fieldType(Compiler, .wasm).init(allocator),

            .error_data = null,
        };
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
        expr: DirExpr,
        data: EvalErrorData,
    },
};
pub const TokenizeErrorData = @import("./tokenize.zig").TokenizeErrorData;
pub const ParseErrorData = @import("./parse.zig").ParseErrorData;
pub const LowerErrorData = @import("./lower.zig").LowerErrorData;
pub const EvalErrorData = @import("./eval.zig").EvalErrorData;

pub fn formatError(c: *Compiler) []const u8 {
    return if (c.error_data) |error_data|
        switch (error_data) {
            .lower => |err| switch (err.data) {
                .invalid_pattern => format(c, "Invalid pattern: {}", .{c.sir_expr_data.get(err.expr)}),
                .todo => format(c, "TODO: {}", .{c.sir_expr_data.get(err.expr)}),
            },
            else => format(c, "{}", .{c.error_data.?}),
        }
    else
        "ok";
}

pub fn format(c: *Compiler, comptime message: []const u8, args: anytype) []const u8 {
    return std.fmt.allocPrint(c.allocator, message, args) catch oom();
}

pub const Repr = @import("./repr.zig").Repr;
pub const ReprStruct = @import("./repr.zig").ReprStruct;
pub const ReprUnion = @import("./repr.zig").ReprUnion;

pub const Value = @import("./value.zig").Value;
pub const ValueStruct = @import("./value.zig").ValueStruct;
pub const ValueUnion = @import("./value.zig").ValueUnion;

pub const tokenize = @import("./tokenize.zig").tokenize;
pub const parse = @import("./parse.zig").parse;
pub const lower = @import("./lower.zig").lower;
pub const eval = @import("./eval.zig").eval;
//pub const infer = @import("./infer.zig").infer;
//pub const shadowify = @import("./shadowify.zig").shadowify;
//pub const reinfer = @import("./infer.zig").reinfer;
//pub const stackify = @import("./stackify.zig").stackify;
//pub const generate = @import("./generate.zig").generate;

pub fn compile(c: *Compiler) error{ TokenizeError, ParseError, LowerError, InferError, GenerateError }!void {
    try tokenize(c);
    assert(c.token_data.count() == c.token_to_source.count());

    try parse(c);
    assert(c.token_next.id == c.token_data.count());

    try lower(c);
    assert(c.dir_fun_main != null);

    //try infer(c);
    //assert(c.specialization_main != null);

    //shadowify(c);

    //try reinfer(c);

    //stackify(c);

    //try generate(c);
}
