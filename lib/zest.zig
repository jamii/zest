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
pub const ReprKind = @import("./repr.zig").ReprKind;

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
pub const generate = @import("./generate.zig").generate;

pub fn oom() noreturn {
    panic("OOM", .{});
}

pub fn p(thing: anytype) void {
    std.debug.print("{any}\n", .{thing});
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
    mut,
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
    @"%",
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
    @"and",
    @"or",
    not,
    @"memory-size",
    @"memory-grow",
    load,
    store,

    pub fn argCount(builtin: Builtin) usize {
        return switch (builtin) {
            .@"memory-size" => 0,
            .not, .@"memory-grow" => 1,
            .equal, .equivalent, .less_than, .less_than_or_equal, .more_than, .more_than_or_equal, .add, .subtract, .multiply, .divide, .@"and", .@"or", .load, .store => 2,
        };
    }
};

pub const TreePart = enum {
    leaf,
    branch_begin,
    branch_end,
};

pub fn treePart(expr_data: anytype) TreePart {
    switch (@TypeOf(expr_data)) {
        sir.ExprData, dir.ExprData, tir.ExprData => {},
        else => @compileError("What are " ++ @typeName(@TypeOf(expr_data))),
    }
    switch (expr_data) {
        inline else => |_, tag| {
            if (comptime std.mem.endsWith(u8, @tagName(tag), "_begin")) {
                return .branch_begin;
            } else if (comptime std.mem.endsWith(u8, @tagName(tag), "_end")) {
                return .branch_end;
            } else {
                return .leaf;
            }
        },
    }
}

pub fn FlatLattice(comptime T: type) type {
    return union(enum) {
        zero,
        one: T,
        many: T,
    };
}

pub const Stage = enum {
    source,
    tokens,
    sir,
    dir,
    tir,
    wir,
};

pub const Compiler = struct {
    allocator: Allocator,
    source: []const u8,

    // tokenize
    token_data: List(Token, TokenData),
    token_to_source: List(Token, [2]usize),

    // parse
    token_next: Token,
    sir_expr_data: List(sir.Expr, sir.ExprData),
    sir_expr_data_buffer: ArrayList(sir.ExprData),
    sir_expr_main: ?sir.Expr,

    // desugar
    scope: dir.Scope,
    dir_fun_data: List(dir.Fun, dir.FunData),
    dir_fun_main: ?dir.Fun,
    sir_expr_next: sir.Expr,

    // eval
    dir_frame_stack: ArrayList(dir.Frame),
    value_stack: ArrayList(Value),
    local_stack: ArrayList(Value),
    while_stack: ArrayList(dir.Expr),
    block_value_count_stack: ArrayList(usize),
    heap: ArrayList(u8),

    // infer
    tir_fun_data: List(tir.Fun, tir.FunData),
    tir_fun_by_key: Map(tir.FunKey, tir.Fun),
    tir_fun_main: ?tir.Fun,
    tir_frame_stack: ArrayList(tir.Frame),
    repr_stack: ArrayList(Repr),
    repr_fixup_stack: ArrayList(tir.Expr),

    // generate
    wir_fun_data: List(wir.Fun, wir.FunData),
    wir_fun_by_tir: Map(tir.Fun, wir.Fun),
    fun_type_memo: Map(wir.FunTypeData, wir.FunType),
    fun_type_data: List(wir.FunType, wir.FunTypeData),
    tir_expr_next: tir.Expr,
    local_walue: List(tir.Local, ?wir.Walue),
    inlining: ?struct {
        closure: wir.Walue,
        arg: wir.Walue,
        local_offset: usize,
    },
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
            .sir_expr_data_buffer = fieldType(Compiler, .sir_expr_data_buffer).init(allocator),
            .sir_expr_main = null,

            .scope = fieldType(Compiler, .scope).init(allocator),
            .dir_fun_data = fieldType(Compiler, .dir_fun_data).init(allocator),
            .dir_fun_main = null,
            .sir_expr_next = .{ .id = 0 },

            .dir_frame_stack = fieldType(Compiler, .dir_frame_stack).init(allocator),
            .value_stack = fieldType(Compiler, .value_stack).init(allocator),
            .local_stack = fieldType(Compiler, .local_stack).init(allocator),
            .while_stack = fieldType(Compiler, .while_stack).init(allocator),
            .block_value_count_stack = fieldType(Compiler, .block_value_count_stack).init(allocator),
            .heap = fieldType(Compiler, .heap).init(allocator),

            .tir_fun_data = fieldType(Compiler, .tir_fun_data).init(allocator),
            .tir_fun_by_key = fieldType(Compiler, .tir_fun_by_key).init(allocator),
            .tir_fun_main = null,
            .tir_frame_stack = fieldType(Compiler, .tir_frame_stack).init(allocator),
            .repr_stack = fieldType(Compiler, .repr_stack).init(allocator),
            .repr_fixup_stack = fieldType(Compiler, .repr_fixup_stack).init(allocator),

            .wir_fun_data = fieldType(Compiler, .wir_fun_data).init(allocator),
            .wir_fun_by_tir = fieldType(Compiler, .wir_fun_by_tir).init(allocator),
            .fun_type_memo = fieldType(Compiler, .fun_type_memo).init(allocator),
            .fun_type_data = fieldType(Compiler, .fun_type_data).init(allocator),
            .tir_expr_next = .{ .id = 0 },
            .local_walue = fieldType(Compiler, .local_walue).init(allocator),
            .inlining = null,
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

    pub fn dupe(c: *Compiler, comptime T: type, slice: []const T) []T {
        return c.allocator.dupe(T, slice) catch oom();
    }

    pub fn print(c: *Compiler, stage: Stage, writer: anytype) !void {
        switch (stage) {
            .source => {
                try writer.print("--- SOURCE ---\n", .{});
                try writer.print("{s}\n", .{c.source});
                try writer.print("---\n", .{});
            },
            .tokens => {
                try writer.print("--- TOKENS ---\n", .{});
                for (c.token_data.items(), c.token_to_source.items()) |token_data, source_range| {
                    try writer.print("{} {any}\n", .{ token_data, source_range });
                }
                try writer.print("---\n", .{});
            },
            .sir => {
                try writer.print("--- SIR ---\n", .{});
                try c.printSir(writer, c.sir_expr_main.?, 0);
                try writer.print("---\n", .{});
            },
            .dir => {
                try writer.print("--- DIR ---\n", .{});
                try writer.print("main = f{}\n", .{c.dir_fun_main.?.id});
                for (c.dir_fun_data.items(), 0..) |f, fun_id| {
                    try writer.print("f{} = (closure", .{fun_id});
                    for (0..f.arg_data.count()) |arg_id| {
                        try writer.print(", a{}", .{arg_id});
                    }
                    try writer.print(")\n", .{});
                    var indent: usize = 1;
                    for (0..f.local_data.count()) |local_id| {
                        try writer.writeByteNTimes(' ', indent * 2);
                        try writer.print("local l{}\n", .{local_id});
                    }
                    for (f.expr_data.items()) |expr_data| {
                        if (treePart(expr_data) == .branch_end) indent -= 1;
                        try writer.writeByteNTimes(' ', indent * 2);
                        try writer.print("{s}", .{@tagName(expr_data)});
                        switch (expr_data) {
                            .i32 => |i| try writer.print(" {}", .{i}),
                            .f32 => |i| try writer.print(" {}", .{i}),
                            .string => |s| try writer.print(" {s}", .{s}),
                            .arg => |arg| try writer.print(" a{}", .{arg.id}),
                            .local_get => |local| try writer.print(" l{}", .{local.id}),
                            .local_let_end => |local| try writer.print(" l{}", .{local.id}),
                            .struct_init_end => |count| try writer.print(" count={}", .{count}),
                            .fun_init_end => |fun_init| try writer.print(" f{}", .{fun_init.fun.id}),
                            .assert_object_end => |assert_object| try writer.print(" count={}", .{assert_object.count}),
                            .call_end => |call_end| try writer.print(" arg_count={}", .{call_end.arg_count}),
                            .call_builtin_end => |builtin| try writer.print(" {}", .{builtin}),
                            inline else => |data, tag| if (@TypeOf(data) != void) @compileError("Missing print case " ++ @tagName(tag)),
                        }
                        try writer.print("\n", .{});
                        if (treePart(expr_data) == .branch_begin) indent += 1;
                    }
                }
                try writer.print("---\n", .{});
            },
            .tir => {
                try writer.print("--- TIR ---\n", .{});
                try writer.print("main = f{}\n", .{c.tir_fun_main.?.id});
                for (c.tir_fun_data.items(), 0..) |f, fun_id| {
                    try writer.print("f{} = (closure", .{fun_id});
                    for (0..f.key.arg_reprs.len) |arg_id| {
                        try writer.print(", a{}", .{arg_id});
                    }
                    try writer.print(")\n", .{});
                    var indent: usize = 1;
                    for (f.local_data.items(), 0..) |local_data, local_id| {
                        try writer.writeByteNTimes(' ', indent * 2);
                        try writer.print("local l{} /{}\n", .{ local_id, local_data.repr.one });
                    }
                    for (f.expr_data.items()) |expr_data| {
                        if (treePart(expr_data) == .branch_end) indent -= 1;
                        try writer.writeByteNTimes(' ', indent * 2);
                        try writer.print("{s}", .{@tagName(expr_data)});
                        switch (expr_data) {
                            .i32 => |i| try writer.print(" {}", .{i}),
                            .f32 => |i| try writer.print(" {}", .{i}),
                            .string => |s| try writer.print(" {s}", .{s}),
                            .arg => |arg| try writer.print(" a{}", .{arg.id}),
                            .local_get => |local| try writer.print(" l{}", .{local.id}),
                            .local_let_end => |local| try writer.print(" l{}", .{local.id}),
                            .object_get_end => |object_get| try writer.print(" index={}", .{object_get.index}),
                            .ref_get_end => |ref_get| try writer.print(" offset={}", .{ref_get.offset}),
                            .ref_set_end => {},
                            .call_end => |fun| try writer.print(" f{}", .{fun.id}),
                            .call_builtin_end => |builtin| try writer.print(" {}", .{builtin}),
                            .make_end => |make_end| try writer.print(" to={}", .{make_end.to}),
                            .struct_init_end => |repr_struct| try writer.print(" /{}", .{Repr{ .@"struct" = repr_struct }}),
                            .ref_init_begin, .if_begin, .ref_deref_end => |repr| try writer.print(" /{}", .{repr}),
                            inline else => |data, tag| if (@TypeOf(data) != void) @compileError("Missing print case: " ++ @tagName(tag)),
                        }
                        try writer.print("\n", .{});
                        if (treePart(expr_data) == .branch_begin) indent += 1;
                    }
                }
                try writer.print("---\n", .{});
            },
            else => panic("TODO", .{}),
        }
    }

    fn printSir(c: *Compiler, writer: anytype, start_expr: sir.Expr, start_indent: usize) @TypeOf(writer.print("", .{})) {
        var expr = start_expr;
        var indent = start_indent;
        while (true) {
            const expr_data = c.sir_expr_data.get(expr);
            if (treePart(expr_data) == .branch_end) indent -= 1;
            try writer.writeByteNTimes(' ', indent * 2);
            try writer.print("{s}", .{@tagName(expr_data)});
            if (expr_data == .indirect) {
                try writer.print("\n", .{});
                try c.printSir(writer, expr_data.indirect, indent);
            } else {
                switch (expr_data) {
                    .i32 => |i| try writer.print(" {}", .{i}),
                    .f32 => |i| try writer.print(" {}", .{i}),
                    .string => |s| try writer.print(" {s}", .{s}),
                    .name => |name| try writer.print(" {s} mut={}", .{ name.name, name.mut }),
                    .call_builtin_end => |builtin| try writer.print(" {}", .{builtin}),
                    .indirect => unreachable,
                    inline else => |data, tag| if (@TypeOf(data) != void) @compileError("Missing print case " ++ @tagName(tag)),
                }
                try writer.print("\n", .{});
            }
            if (treePart(expr_data) == .branch_begin) indent += 1;
            if (indent == start_indent) break;
            expr.id += 1;
        }
    }
};

pub const TokenizeErrorData = @import("./tokenize.zig").TokenizeErrorData;
pub const ParseErrorData = @import("./parse.zig").ParseErrorData;
pub const DesugarErrorData = @import("./desugar.zig").DesugarErrorData;
pub const EvalErrorData = @import("./eval.zig").EvalErrorData;
pub const InferErrorData = @import("./infer.zig").InferErrorData;
pub const GenerateErrorData = @import("./generate.zig").GenerateErrorData;
pub const ErrorData = union(enum) {
    tokenize: TokenizeErrorData,
    parse: ParseErrorData,
    desugar: DesugarErrorData,
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
    generate: struct {
        data: GenerateErrorData,
    },
};

pub fn formatError(c: *Compiler) []const u8 {
    if (c.error_data) |error_data|
        switch (error_data) {
            .desugar => |err| {
                const expr_data = c.sir_expr_data.get(c.sir_expr_next);
                return switch (err) {
                    .invalid_pattern => format(c, "Invalid pattern: {}", .{expr_data}),
                    .name_not_bound => |data| format(c, "Name not bound: {s}", .{data.name}),
                    .name_already_bound => |data| format(c, "Name already bound: {s}", .{data.name}),
                    .may_not_mutate_immutable_binding => |data| format(c, "May not mututate immutable binding: {s}", .{data.name}),
                    .invalid_path => format(c, "Invalid path: {}", .{expr_data}),
                    .invalid_let_path => format(c, "Invalid let path: {}", .{expr_data}),
                    .meaningless_mut => format(c, "Meaningless to write `mut` here", .{}),
                    .wrong_builtin_arg_count => |data| format(c, "%{s} expected {} arguments, found {}", .{ @tagName(data.builtin), data.expected, data.found }),
                    .todo => format(c, "TODO desugar: {}", .{expr_data}),
                };
            },
            .eval => |err| {
                const expr_data = c.dir_fun_data.get(err.fun).expr_data.get(err.expr);
                return switch (err.data) {
                    .type_error => |data| format(c, "Expected {}, found {}", .{ data.expected, data.found }),
                    .key_not_found => |data| format(c, "Key {} not found in {}", .{ data.key, data.object }),
                    .wrong_number_of_keys => |data| format(c, "Expected {} keys, found {} keys", .{ data.expected, data.actual }),
                    .expected_object => |data| format(c, "Expected an object, found: {}", .{data}),
                    .expected_is_ref => |data| format(c, "Expected a mutable reference, found: {}", .{data}),
                    .expected_has_no_ref => |data| format(c, "Expected a value containing no mutable references, found: {}", .{data}),
                    .not_a_fun => |data| format(c, "Not a function: {}", .{data}),
                    .not_a_bool => |data| format(c, "Not a 'boolean': {}", .{data}),
                    .cannot_stage_expr => format(c, "Cannot stage expr", .{}),
                    .cannot_unstage_value => |data| format(c, "Cannot unstage value: {}", .{data}),
                    .invalid_call_builtin => |data| format(c, "Cannot call {} with these args: {any}", .{ data.builtin, data.args }),
                    .cannot_make => |data| format(c, "Cannot make {} with these args: {}", .{ data.head, data.args }),
                    .cannot_make_head => |data| format(c, "Cannot make {}", .{data.head}),
                    .todo => format(c, "TODO eval: {}", .{expr_data}),
                };
            },
            .infer => |err| {
                const expr_data = c.dir_fun_data.get(err.key.fun).expr_data.get(err.expr);
                return switch (err.data) {
                    .value_not_staged => |data| format(c, "Value not staged: {}", .{data}),
                    .type_error => |data| format(c, "Expected {}, found {}", .{ data.expected, data.found }),
                    .wrong_number_of_keys => |data| format(c, "Expected {} keys, found {} keys", .{ data.expected, data.actual }),
                    .expected_object => |data| format(c, "Expected an object, found: {}", .{data}),
                    .expected_is_ref => |data| format(c, "Expected a mutable reference, found: {}", .{data}),
                    .expected_has_no_ref => |data| format(c, "Expected a value containing no mutable references, found: {}", .{data}),
                    .key_not_found => |data| format(c, "Key {} not found in {}", .{ data.key, data.object }),
                    .not_a_fun => |data| format(c, "Not a function: {}", .{data}),
                    .not_a_bool => |data| format(c, "Not a 'boolean': {}", .{data}),
                    .invalid_call_builtin => |data| format(c, "Cannot call {} with these args: {any}", .{ data.builtin, data.args }),
                    .cannot_make => |data| format(c, "Cannot make {} with these args: {}", .{ data.head, data.args }),
                    .cannot_make_head => |data| format(c, "Cannot make {}", .{data.head}),
                    .todo => format(c, "TODO infer: {}", .{expr_data}),
                };
            },
            .generate => |err| {
                return switch (err.data) {
                    .todo => format(c, "TODO generate", .{}),
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
    c.print(.source, std.io.getStdErr().writer()) catch unreachable;

    try tokenize(c);
    assert(c.token_data.count() == c.token_to_source.count());
    c.print(.tokens, std.io.getStdErr().writer()) catch unreachable;

    try parse(c);
    assert(c.token_next.id == c.token_data.count());
    c.print(.sir, std.io.getStdErr().writer()) catch unreachable;

    try desugar(c);
    assert(c.dir_fun_main != null);
    c.print(.dir, std.io.getStdErr().writer()) catch unreachable;
}

pub fn compileStrict(c: *Compiler) error{ EvalError, InferError, GenerateError }!void {
    assert(c.dir_fun_main != null);

    try inferMain(c);
    assert(c.tir_fun_main != null);
    c.print(.tir, std.io.getStdErr().writer()) catch unreachable;

    try generate(c);
    assert(c.wasm.items.len != 0);
}
