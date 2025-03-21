const zest = @This();

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
pub const FormatKey = @import("./value.zig").FormatKey;

pub const sir = @import("./sir.zig");
pub const dir = @import("./dir.zig");
pub const tir = @import("./tir.zig");
pub const wir = @import("./wir.zig");

pub const tokenize = @import("./tokenize.zig").tokenize;
pub const parse = @import("./parse.zig").parse;
pub const desugar = @import("./desugar.zig").desugar;
pub const evalMain = @import("./eval.zig").evalMain;
pub const infer = @import("./infer.zig").infer;
pub const generate = @import("./generate.zig").generate;

pub const isName = @import("./tokenize.zig").isName;

pub fn oom() noreturn {
    panic("OOM", .{});
}

pub fn p(thing: anytype) void {
    std.debug.print("{any}\n", .{thing});
}

pub fn fieldType(comptime T: type, comptime field_enum: std.meta.FieldEnum(T)) type {
    @setEvalBranchQuota(3000);
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
    module,
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
    @"!=",
    @"~=",
    @"<",
    @"<=",
    @">",
    @">=",
    @"+",
    @"-",
    @"/",
    @"*",
    @"<<",
    comment,
    space,
    newline,
    eof,
};

pub const Builtin = enum {
    equal,
    @"not-equal",
    equivalent,
    @"less-than",
    @"less-than-or-equal",
    @"more-than",
    @"more-than-or-equal",
    negate,
    add,
    subtract,
    multiply,
    divide,
    remainder,
    @"bit-shift-left",
    clz,
    @"and",
    @"or",
    not,
    @"memory-size",
    @"memory-grow",
    @"memory-fill",
    @"memory-copy",
    @"heap-start",
    load,
    store,
    @"size-of",
    print,
    panic,
    @"union-has-key",
    @"repr-of",
    reflect,
    @"from-only",
    each,

    pub fn argCount(builtin: Builtin) usize {
        return switch (builtin) {
            .@"memory-size", .@"heap-start", .panic => 0,
            .negate, .not, .@"memory-grow", .@"size-of", .print, .clz, .@"repr-of", .reflect, .@"from-only" => 1,
            .equal, .@"not-equal", .equivalent, .@"less-than", .@"less-than-or-equal", .@"more-than", .@"more-than-or-equal", .add, .subtract, .multiply, .divide, .remainder, .@"bit-shift-left", .@"and", .@"or", .load, .store, .@"union-has-key", .each => 2,
            .@"memory-fill", .@"memory-copy" => 3,
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

pub fn convertPostorderToPreorder(
    c: *Compiler,
    comptime Expr: type,
    comptime ExprData: type,
    post: List(Expr, ExprData),
    pre: *List(Expr, ExprData),
) void {
    //{
    //    zest.p(.@"---");
    //    for (post.data.items) |item| zest.p(item);
    //    zest.p(.@"---");
    //}
    const count = post.count();
    if (count == 0) return;
    pre.data.ensureTotalCapacity(count) catch oom();
    pre.data.items.len = count;
    var post_expr = Expr{ .id = count };
    var pre_expr = Expr{ .id = count };
    convertExpr(c, Expr, ExprData, post, pre, &post_expr, &pre_expr);
    assert(post_expr.id == 0);
    assert(pre_expr.id == 0);
}

fn convertExpr(
    c: *Compiler,
    comptime Expr: type,
    comptime ExprData: type,
    post: List(Expr, ExprData),
    pre: *List(Expr, ExprData),
    post_expr: *Expr,
    pre_expr: *Expr,
) void {
    post_expr.id -= 1;
    const expr_data = post.get(post_expr.*);
    //zest.p(.{ expr_data, expr_data.childCount(c) });
    const pre_tree_end = pre_expr.*;
    const post_tree_end = post_expr.*;
    for (0..expr_data.childCount(c)) |_| {
        convertExpr(c, Expr, ExprData, post, pre, post_expr, pre_expr);
    }
    pre_expr.id -= 1;
    pre.getPtr(pre_expr.*).* = expr_data;
    const pre_tree_start = pre_expr.*;
    const post_tree_start = post_expr.*;
    if (Expr == dir.Expr) {
        switch (expr_data) {
            .stage => {
                pre.getPtr(pre_tree_start).stage.mapping = post_tree_start;
                post.getPtr(post_tree_end).stage.mapping = pre_tree_end;
            },
            .unstage => {
                post.getPtr(post_tree_start).unstage_begin.mapping = .{ .id = pre_tree_start.id + 1 };
                pre.getPtr(.{ .id = pre_tree_start.id + 1 }).unstage_begin.mapping = post_tree_end;
            },
            .repr_of => {
                post.getPtr(post_tree_start).repr_of_begin.mapping = .{ .id = pre_tree_start.id + 1 };
                pre.getPtr(.{ .id = pre_tree_start.id + 1 }).repr_of_begin.mapping = post_tree_end;
            },
            else => {},
        }
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
    sir_expr_data_pre: List(sir.Expr, sir.ExprData),
    sir_expr_data_post: List(sir.Expr, sir.ExprData),

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
    memory: ArrayList(u8),
    printed: ArrayList(u8),

    // infer
    tir_fun_data: List(tir.Fun, tir.FunData),
    tir_fun_by_key: Map(tir.FunKey, tir.Fun),
    tir_fun_main: ?tir.Fun,
    tir_fun_data_next: ?*tir.FunData,
    infer_mode: enum { infer, unstage },

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
    constant_memo: Map(Value, u32),
    constant_data: ArrayList(u8),
    wasm: ArrayList(u8),

    // constants
    reflection: ReprUnion,
    string_innards: ReprStruct,

    error_data: ?ErrorData,

    pub fn init(allocator: Allocator, source: []const u8) Compiler {
        var c = Compiler{
            .allocator = allocator,
            .source = source,

            .token_data = fieldType(Compiler, .token_data).init(allocator),
            .token_to_source = fieldType(Compiler, .token_to_source).init(allocator),

            .token_next = .{ .id = 0 },
            .sir_expr_data_pre = fieldType(Compiler, .sir_expr_data_pre).init(allocator),
            .sir_expr_data_post = fieldType(Compiler, .sir_expr_data_post).init(allocator),

            .scope = fieldType(Compiler, .scope).init(allocator),
            .dir_fun_data = fieldType(Compiler, .dir_fun_data).init(allocator),
            .dir_fun_main = null,
            .sir_expr_next = .{ .id = 0 },

            .dir_frame_stack = fieldType(Compiler, .dir_frame_stack).init(allocator),
            .value_stack = fieldType(Compiler, .value_stack).init(allocator),
            .local_stack = fieldType(Compiler, .local_stack).init(allocator),
            .while_stack = fieldType(Compiler, .while_stack).init(allocator),
            .memory = fieldType(Compiler, .memory).init(allocator),
            .printed = fieldType(Compiler, .printed).init(allocator),

            .tir_fun_data = fieldType(Compiler, .tir_fun_data).init(allocator),
            .tir_fun_by_key = fieldType(Compiler, .tir_fun_by_key).init(allocator),
            .tir_fun_main = null,
            .tir_fun_data_next = null,
            .infer_mode = .infer,

            .wir_fun_data = fieldType(Compiler, .wir_fun_data).init(allocator),
            .wir_fun_by_tir = fieldType(Compiler, .wir_fun_by_tir).init(allocator),
            .fun_type_memo = fieldType(Compiler, .fun_type_memo).init(allocator),
            .fun_type_data = fieldType(Compiler, .fun_type_data).init(allocator),
            .tir_expr_next = .{ .id = 0 },
            .local_walue = fieldType(Compiler, .local_walue).init(allocator),
            .inlining = null,
            .constant_memo = fieldType(Compiler, .constant_memo).init(allocator),
            .constant_data = fieldType(Compiler, .constant_data).init(allocator),
            .wasm = fieldType(Compiler, .wasm).init(allocator),

            .reflection = ReprUnion{
                .keys = allocator.dupe(Value, &[_]Value{
                    .{ .string = "u32" },
                    .{ .string = "i64" },
                    .{ .string = "string" },
                    .{ .string = "struct" },
                    .{ .string = "union" },
                    .{ .string = "fun" },
                    .{ .string = "only" },
                    .{ .string = "repr" },
                    .{ .string = "repr-kind" },
                }) catch oom(),
                // TODO Need lists for most useful values.
                .reprs = allocator.dupe(Repr, &[_]Repr{
                    Repr.emptyStruct(),
                    Repr.emptyStruct(),
                    Repr.emptyStruct(),
                    Repr.emptyStruct(),
                    Repr.emptyStruct(),
                    Repr.emptyStruct(),
                    Repr.emptyStruct(),
                    Repr.emptyStruct(),
                    Repr.emptyStruct(),
                }) catch oom(),
            },
            .string_innards = ReprStruct{
                .keys = allocator.dupe(Value, &[_]Value{ .{ .string = "ptr" }, .{ .string = "len" } }) catch oom(),
                .reprs = allocator.dupe(Repr, &[_]Repr{ .u32, .u32 }) catch oom(),
            },

            .error_data = null,
        };
        // Need at least one page of memory for the allocator.
        c.memory.appendNTimes(0, std.wasm.page_size) catch oom();
        return c;
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
                var expr = sir.Expr{ .id = 0 };
                var indent: usize = 0;
                try c.printSir(writer, &expr, &indent);
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
                    var expr = dir.Expr{ .id = 0 };
                    var indent: usize = 1;
                    for (0..f.local_data.count()) |local_id| {
                        try writer.writeByteNTimes(' ', indent * 2);
                        try writer.print("local l{}\n", .{local_id});
                    }
                    try c.printDir(writer, f, &expr, &indent);
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
                    var expr = tir.Expr{ .id = 0 };
                    var indent: usize = 1;
                    for (f.local_data.items(), 0..) |local_data, local_id| {
                        try writer.writeByteNTimes(' ', indent * 2);
                        try writer.print("local l{} /{}\n", .{ local_id, local_data.repr.one });
                    }
                    try c.printTir(writer, f, &expr, &indent);
                }
                try writer.print("---\n", .{});
            },
            else => panic("TODO", .{}),
        }
    }

    fn printSir(c: *Compiler, writer: anytype, expr: *sir.Expr, indent: *usize) @TypeOf(writer.print("", .{})) {
        const expr_data = c.sir_expr_data_pre.get(expr.*);
        expr.id += 1;

        try writer.writeByteNTimes(' ', indent.* * 2);
        try writer.print("{s}", .{@tagName(expr_data)});
        switch (expr_data) {
            .i64 => |i| try writer.print(" {}", .{i}),
            .f64 => |i| try writer.print(" {}", .{i}),
            .string => |s| try writer.print(" {s}", .{s}),
            .name => |name| try writer.print(" {s} mut={}", .{ name.name, name.mut }),
            .call_builtin => |builtin| try writer.print(" {}", .{builtin}),
            .object => |object| try writer.print(" {}", .{object.count}),
            .pos_value => |pos| try writer.print(" {}", .{pos}),
            .block => |block| try writer.print(" {}", .{block.count}),
            inline else => |data, tag| if (@TypeOf(data) != void) @compileError("Missing print case " ++ @tagName(tag)),
        }
        try writer.print("\n", .{});

        indent.* += 1;
        for (0..expr_data.childCount(c)) |_| {
            try c.printSir(writer, expr, indent);
        }
        indent.* -= 1;
    }

    fn printDir(c: *Compiler, writer: anytype, f: dir.FunData, expr: *dir.Expr, indent: *usize) @TypeOf(writer.print("", .{})) {
        const expr_data = f.expr_data_pre.get(expr.*);
        expr.id += 1;

        try writer.writeByteNTimes(' ', indent.* * 2);
        try writer.print("{s}", .{@tagName(expr_data)});
        switch (expr_data) {
            .i64 => |i| try writer.print(" {}", .{i}),
            .f64 => |i| try writer.print(" {}", .{i}),
            .string => |s| try writer.print(" {s}", .{s}),
            .arg => |arg| try writer.print(" a{}", .{arg.id}),
            .local_get => |local| try writer.print(" l{}", .{local.id}),
            .local_let => |local| try writer.print(" l{}", .{local.id}),
            .struct_init => |struct_init| try writer.print(" count={}", .{struct_init.count}),
            .fun_init => |fun_init| try writer.print(" f{}", .{fun_init.fun.id}),
            .assert_object => |assert_object| try writer.print(" count={}", .{assert_object.count}),
            .call => |call| try writer.print(" arg_count={}", .{call.arg_count}),
            .call_builtin => |builtin| try writer.print(" {}", .{builtin}),
            .block => |block| try writer.print(" {}", .{block.count}),
            .stage, .repr_of_begin, .unstage_begin => |data| try writer.print(" expr_id={}", .{data.mapping.id}),
            inline else => |data, tag| if (@TypeOf(data) != void) @compileError("Missing print case " ++ @tagName(tag)),
        }
        try writer.print("\n", .{});

        indent.* += 1;
        for (0..expr_data.childCount(c)) |_| {
            try c.printDir(writer, f, expr, indent);
        }
        indent.* -= 1;
    }

    fn printTir(c: *Compiler, writer: anytype, f: tir.FunData, expr: *tir.Expr, indent: *usize) @TypeOf(writer.print("", .{})) {
        const expr_data = f.expr_data_pre.get(expr.*);
        expr.id += 1;

        try writer.writeByteNTimes(' ', indent.* * 2);
        try writer.print("{s}", .{@tagName(expr_data)});
        switch (expr_data) {
            .i64 => |i| try writer.print(" {}", .{i}),
            .f64 => |i| try writer.print(" {}", .{i}),
            .string => |s| try writer.print(" {s}", .{s}),
            .only => |v| try writer.print(" {}", .{v}),
            .arg => |arg| try writer.print(" a{}", .{arg.id}),
            .local_get => |local| try writer.print(" l{}", .{local.id}),
            .local_let => |local| try writer.print(" l{}", .{local.id}),
            .object_get => |object_get| try writer.print(" index={}", .{object_get.index}),
            .ref_get => |ref_get| try writer.print(" {}", .{ref_get}),
            .ref_set => {},
            .call => |fun| try writer.print(" f{}", .{fun.id}),
            .call_builtin => |builtin| try writer.print(" {}", .{builtin}),
            .struct_init => |repr_struct| try writer.print(" /{}", .{Repr{ .@"struct" = repr_struct }}),
            .union_init => |union_init| try writer.print(" /{} tag={}", .{ Repr{ .@"union" = union_init.repr }, union_init.tag }),
            .ref_init, .@"if", .ref_deref => |repr| try writer.print(" /{}", .{repr}),
            .block => |block| try writer.print(" {}", .{block.count}),
            .each_struct, .each_union => |funs| try writer.print(" {any}", .{funs}),
            inline else => |data, tag| if (@TypeOf(data) != void) @compileError("Missing print case: " ++ @tagName(tag)),
        }
        try writer.print("\n", .{});

        indent.* += 1;
        for (0..expr_data.childCount(c)) |_| {
            try c.printTir(writer, f, expr, indent);
        }
        indent.* -= 1;
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
        expr: dir.Expr,
        data: InferErrorData,
    },
    generate: struct {
        data: GenerateErrorData,
    },
};

const Location = struct {
    source: []const u8,
    line: usize,
    column: usize,
    range: [2]usize,

    pub fn format(self: Location, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = fmt;
        _ = options;
        try writer.print("At {}:{}:\n{s}\n", .{
            self.line,
            self.column,
            self.source[self.range[0]..self.range[1]],
        });
        try writer.writeByteNTimes(' ', self.column);
        try writer.writeByte('^');
    }
};

fn locate(c: *Compiler, pos: usize) Location {
    const line = 1 + std.mem.count(u8, c.source[0..pos], "\n");
    const start = if (std.mem.lastIndexOfScalar(u8, c.source[0..pos], '\n')) |i| i + 1 else 0;
    const end = std.mem.indexOfScalarPos(u8, c.source, pos, '\n') orelse c.source.len;
    return .{
        .source = c.source,
        .line = line,
        .column = pos - start,
        .range = .{ start, end },
    };
}

pub fn formatError(c: *Compiler) []const u8 {
    if (c.error_data) |error_data|
        switch (error_data) {
            .tokenize => |err| {
                const location = locate(c, err.pos);
                return format(c, "Tokenize error\n{}", .{location});
            },
            .parse => |err| {
                const pos = c.token_to_source.get(c.token_next)[0];
                const location = locate(c, pos);
                return switch (err) {
                    .unexpected => |data| format(c, "Parse error: expected {s}, found {}\n{}", .{ data.expected, data.found, location }),
                    .unexpected_space => format(c, "Parse error: unexpected space\n{}", .{location}),
                    .ambiguous_precedence => |data| format(c, "Parse error: ambigious precedence between {} and {}\n{}", .{ data[0], data[1], location }),
                    .invalid_string_escape => |data| format(c, "Parse error: invalid string escape \\{}\n{}", .{ data, location }),
                    .positional_args_after_keyed_args => format(c, "Parse error: all positional args must appear before all keyed args\n{}", .{location}),
                    .parse_i64 => |data| format(c, "Parse error: invalid i64: {}\n{}", .{ data, location }),
                    .parse_f64 => |data| format(c, "Parse error: invalid f64: {}\n{}", .{ data, location }),
                    .not_a_builtin => |data| format(c, "Parse error: invalid builtin: {s}\n{}", .{ data, location }),
                    .wrong_builtin_arg_count => |data| format(c, "Parse error: expected {} arguments, found {} arguments\n{}", .{ data.expected, data.found, location }),
                };
            },
            .desugar => |err| {
                const expr_data = c.sir_expr_data_pre.get(err.expr);
                return switch (err.data) {
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
                const expr_data = c.dir_fun_data.get(err.fun).expr_data_post.get(err.expr);
                return switch (err.data) {
                    .type_error => |data| format(c, "Expected {}, found {}", .{ data.expected, data.found }),
                    .convert_error => |data| format(c, "Can't convert {} to {}", .{ data.found, data.expected }),
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
                    .out_of_bounds => |data| format(c, "Out of bounds {s} of {} at {}", .{ @tagName(data.op), data.repr, data.address }),
                    .division_by_zero => format(c, "Division by zero", .{}),
                    .panic => format(c, "panic", .{}),
                    .union_never_has_key => |data| format(c, "Can never find key {} in {}", .{ data.key, data.object }),
                    .todo => format(c, "TODO eval: {}", .{expr_data}),
                };
            },
            .infer => |err| {
                const expr_data = c.dir_fun_data.get(err.key.fun).expr_data_pre.get(err.expr);
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
                    .union_never_has_key => |data| format(c, "Can never find key {} in {}", .{ data.key, data.object }),
                    .todo => format(c, "TODO infer: {}", .{expr_data}),
                };
            },
            .generate => |err| {
                return switch (err.data) {
                    .todo => format(c, "TODO generate", .{}),
                };
            },
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
    assert(c.token_next.id == c.token_data.count() - 1);
    c.print(.sir, std.io.getStdErr().writer()) catch unreachable;

    try desugar(c);
    assert(c.dir_fun_main != null);
    c.print(.dir, std.io.getStdErr().writer()) catch unreachable;
}

pub fn compileStrict(c: *Compiler) error{ EvalError, InferError, GenerateError }!void {
    assert(c.dir_fun_main != null);

    try infer(c);
    assert(c.tir_fun_main != null);
    c.print(.tir, std.io.getStdErr().writer()) catch unreachable;

    try generate(c);
    assert(c.wasm.items.len != 0);
}
