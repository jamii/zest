//! Typed IR

const std = @import("std");
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;

const zest = @import("./zest.zig");
const fieldType = zest.fieldType;
const List = zest.List;
const Repr = zest.Repr;
const ReprStruct = zest.ReprStruct;
const ReprUnion = zest.ReprUnion;
const ReprFun = zest.ReprFun;
const Value = zest.Value;
const FlatLattice = zest.FlatLattice;
const TreePart = zest.TreePart;
const Compiler = zest.Compiler;
const dir = @import("./dir.zig");

pub const Arg = struct { id: usize };

pub const Local = struct { id: usize };

pub const LocalData = struct {
    repr: FlatLattice(Repr),
    is_tmp: bool,
};

pub const Expr = struct { id: usize };

pub const ExprData = union(enum) {
    i64: i64,
    f64: f64,
    string: []const u8,
    only: *Value,
    closure,
    arg: Arg,
    local_get: Local,

    struct_init: ReprStruct,
    union_init: struct {
        repr: ReprUnion,
        tag: u32,
    },
    local_let: Local,
    object_get: struct {
        index: usize,
    },
    ref_init: Repr,
    ref_get: union(enum) {
        struct_offset: u32,
        union_tag: u32,
    },
    ref_set,
    ref_deref: Repr,
    call: Fun,
    call_builtin: BuiltinTyped,
    each_struct: []const Fun,
    each_union: []const Fun,
    block: struct {
        count: usize,
    },
    @"if": Repr,
    @"while",
    @"return",

    pub fn childCount(expr_data: ExprData, c: *Compiler) usize {
        return switch (expr_data) {
            .i64, .f64, .string, .closure, .arg, .local_get => 0,
            .only, .union_init, .local_let, .object_get, .ref_init, .ref_get, .ref_deref, .@"return" => 1,
            .each_struct, .each_union, .ref_set, .@"while" => 2,
            .@"if" => 3,
            .struct_init => |repr| repr.keys.len,
            .call => |fun| 1 + c.tir_fun_data.get(fun).key.arg_reprs.len,
            .call_builtin => |builtin| builtin.argCount(),
            .block => |block| block.count,
        };
    }
};

pub const BuiltinTyped = union(enum) {
    equal_u32,
    equal_i64,
    not_equal_u32,
    not_equal_i64,
    less_than_u32,
    less_than_i64,
    less_than_or_equal_u32,
    less_than_or_equal_i64,
    more_than_u32,
    more_than_i64,
    more_than_or_equal_u32,
    more_than_or_equal_i64,
    negate_i64,
    add_u32,
    add_i64,
    subtract_u32,
    subtract_i64,
    multiply_u32,
    multiply_i64,
    remainder_u32,
    remainder_i64,
    bit_shift_left_u32,
    clz_u32,
    memory_size,
    memory_grow,
    memory_fill,
    memory_copy,
    heap_start,
    size_of: u32,
    load: Repr,
    store,
    print_u32,
    print_i64,
    print_string,
    panic,
    union_has_key: u32,
    i64_to_u32,
    from_only,

    pub fn argCount(builtin: BuiltinTyped) usize {
        return switch (builtin) {
            .memory_size, .heap_start, .panic, .size_of => 0,
            .negate_i64, .clz_u32, .memory_grow, .print_u32, .print_i64, .print_string, .load, .union_has_key, .i64_to_u32, .from_only => 1,
            .equal_u32, .equal_i64, .not_equal_u32, .not_equal_i64, .less_than_u32, .less_than_i64, .less_than_or_equal_u32, .less_than_or_equal_i64, .more_than_u32, .more_than_i64, .more_than_or_equal_u32, .more_than_or_equal_i64, .add_u32, .add_i64, .subtract_u32, .subtract_i64, .multiply_u32, .multiply_i64, .remainder_u32, .remainder_i64, .bit_shift_left_u32, .store => 2,
            .memory_fill, .memory_copy => 3,
        };
    }

    pub fn hasSideEffects(builtin: BuiltinTyped) bool {
        return switch (builtin) {
            .equal_u32, .equal_i64, .not_equal_u32, .not_equal_i64, .less_than_u32, .less_than_i64, .less_than_or_equal_u32, .less_than_or_equal_i64, .more_than_u32, .more_than_i64, .more_than_or_equal_u32, .more_than_or_equal_i64, .negate_i64, .add_u32, .add_i64, .subtract_u32, .subtract_i64, .multiply_u32, .multiply_i64, .remainder_u32, .remainder_i64, .bit_shift_left_u32, .clz_u32, .memory_size, .heap_start, .size_of, .union_has_key, .i64_to_u32, .from_only => false,
            .memory_grow, .memory_fill, .memory_copy, .load, .store, .print_u32, .print_i64, .print_string, .panic => true,
        };
    }
};

pub const FunKey = struct {
    fun: dir.Fun,
    closure_repr: Repr,
    arg_reprs: []Repr,
};

pub const Fun = struct { id: usize };

pub const FunData = struct {
    key: FunKey,
    local_data: List(Local, LocalData),
    expr_data_post: List(Expr, ExprData),
    expr_data_pre: List(Expr, ExprData),
    return_repr: FlatLattice(Repr),
    dir_expr_next: dir.Expr,

    pub fn init(allocator: Allocator, key: FunKey) FunData {
        return .{
            .key = key,
            .local_data = fieldType(FunData, .local_data).init(allocator),
            .expr_data_post = fieldType(FunData, .expr_data_post).init(allocator),
            .expr_data_pre = fieldType(FunData, .expr_data_pre).init(allocator),
            .return_repr = .zero,
            .dir_expr_next = .{ .id = 0 },
        };
    }
};
