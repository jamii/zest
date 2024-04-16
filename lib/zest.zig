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

pub const Expr = struct { id: usize };

pub const ExprData = union(enum) {
    // TODO Replace i32/f32 with bigInt/bigDec
    i32: i32,
    f32: f32,
    string: []const u8,
    object: ObjectExprData,
    name: []const u8,
    builtin: Builtin,
    mut: Expr,
    let_or_set: struct {
        path: Expr,
        value: Expr,
    },
    @"if": struct {
        cond: Expr,
        then: Expr,
        @"else": Expr,
    },
    @"while": struct {
        cond: Expr,
        body: Expr,
    },
    @"fn": struct {
        params: ObjectExprData,
        body: Expr,
    },
    make: struct {
        head: Expr,
        args: ObjectExprData,
    },
    call: struct {
        head: Expr,
        args: ObjectExprData,
    },
    block: []Expr,
};

pub const ObjectExprData = struct {
    keys: []Expr,
    values: []Expr,
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

pub const Arg = struct { id: usize };

pub const Local = struct { id: usize };

pub const Shadow = struct { id: usize };

pub const Function = struct { id: usize };

pub const FunctionData = struct {
    local_repr: List(Local, Repr),

    //node_data: List(Node, NodeData),

    pub fn init(allocator: Allocator) FunctionData {
        return .{
            .local_repr = fieldType(FunctionData, .local_repr).init(allocator),

            .node_data = fieldType(FunctionData, .node_data).init(allocator),
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
    //node: Node,
    function: Function,
    builtin: Builtin,
};

pub const SpecializationArgs = struct {
    function: Function,
    mode: enum { lax, strict },
    in_repr: Repr,
};

pub const Specialization = struct { id: usize };

pub const SpecializationData = struct {
    function: Function,

    local_repr: List(Local, Repr),

    shadow_repr: List(Shadow, Repr),

    //node_data: List(Node, NodeData),
    //node_first: ?Node,
    //node_last: ?Node,
    //node_next: List(Node, ?Node),
    //node_prev: List(Node, ?Node),

    //in_repr: List(Arg, Repr),
    //out_repr: Repr,
    //node_repr: List(Node, Repr),

    pub fn init(allocator: Allocator, function: Function) SpecializationData {
        return .{
            .function = function,

            .local_repr = fieldType(SpecializationData, .local_repr).init(allocator),

            .shadow_repr = fieldType(SpecializationData, .shadow_repr).init(allocator),

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

    token_data: List(Token, TokenData),
    token_to_source: List(Token, [2]usize),

    token_next: Token,
    expr_data: List(Expr, ExprData),

    scope: Scope,
    function_data: List(Function, FunctionData),
    function_main: ?Function,

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
            .expr_data = fieldType(Compiler, .expr_data).init(allocator),

            .scope = fieldType(Compiler, .scope).init(allocator),
            .function_main = null,
            .function_data = fieldType(Compiler, .function_data).init(allocator),

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
};
pub const TokenizeErrorData = @import("./tokenize.zig").TokenizeErrorData;
pub const ParseErrorData = @import("./parse.zig").ParseErrorData;

pub const Repr = @import("./repr.zig").Repr;
pub const ReprStruct = @import("./repr.zig").ReprStruct;
pub const ReprUnion = @import("./repr.zig").ReprUnion;

pub const Value = @import("./value.zig").Value;
pub const ValueStruct = @import("./value.zig").ValueStruct;
pub const ValueUnion = @import("./value.zig").ValueUnion;

pub const tokenize = @import("./tokenize.zig").tokenize;
pub const parse = @import("./parse.zig").parse;
//pub const lower = @import("./lower.zig").lower;
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

    //try lower(c);
    //assert(c.function_main != null);

    //try infer(c);
    //assert(c.specialization_main != null);

    //shadowify(c);

    //try reinfer(c);

    //stackify(c);

    //try generate(c);
}
