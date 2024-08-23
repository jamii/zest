//! Typed IR

const std = @import("std");
const Allocator = std.mem.Allocator;

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
    closure,
    arg: Arg,
    local_get: Local,

    nop_begin,
    nop_end,
    struct_init_begin,
    struct_init_end: ReprStruct,
    local_let_begin,
    local_let_end: Local,
    object_get_begin,
    object_get_end: struct {
        index: usize,
    },
    ref_init_begin: Repr,
    ref_init_end,
    ref_get_begin,
    ref_get_end: union(enum) {
        struct_offset: u32,
        union_tag: u32,
    },
    ref_set_begin,
    ref_set_end,
    ref_deref_begin,
    ref_deref_end: Repr,
    call_begin,
    call_end: Fun,
    call_builtin_begin: BuiltinTyped,
    call_builtin_end,
    make_begin,
    // TODO Can remove this ugly union by allowing infer to edit make_begin into something else.
    make_end: union(enum) {
        nop,
        i64_to_u32,
        union_init: struct {
            repr: ReprUnion,
            tag: u32,
        },
        to_only: *Value,
        from_only: *Value,
    },
    block_begin,
    block_last,
    block_end,
    return_begin,
    return_end,

    if_begin: Repr,
    if_then,
    if_else,
    if_end,
    while_begin,
    while_body,
    while_end,
};

pub const BuiltinTyped = union(enum) {
    dummy, // should never be generated
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
    print_string,
    panic,
    union_has_key: u32,

    pub fn hasSideEffects(builtin: BuiltinTyped) bool {
        return switch (builtin) {
            .dummy, .equal_u32, .equal_i64, .not_equal_u32, .not_equal_i64, .less_than_u32, .less_than_i64, .less_than_or_equal_u32, .less_than_or_equal_i64, .more_than_u32, .more_than_i64, .more_than_or_equal_u32, .more_than_or_equal_i64, .add_u32, .add_i64, .subtract_u32, .subtract_i64, .multiply_u32, .multiply_i64, .remainder_u32, .remainder_i64, .bit_shift_left_u32, .clz_u32, .memory_size, .heap_start, .size_of, .union_has_key => false,
            .memory_grow, .memory_fill, .memory_copy, .load, .store, .print_string, .panic => true,
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
    expr_data: List(Expr, ExprData),
    return_repr: FlatLattice(Repr),

    pub fn init(allocator: Allocator, key: FunKey) FunData {
        return .{
            .key = key,

            .local_data = fieldType(FunData, .local_data).init(allocator),

            .expr_data = fieldType(FunData, .expr_data).init(allocator),

            .return_repr = .zero,
        };
    }
};

pub const Frame = struct {
    key: FunKey,
    fun: Fun,
    expr: dir.Expr,
    ends_remaining: usize,
    mode: enum { infer, unstage },
};
