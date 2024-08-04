//! Typed IR

const std = @import("std");
const Allocator = std.mem.Allocator;

const zest = @import("./zest.zig");
const fieldType = zest.fieldType;
const List = zest.List;
const Repr = zest.Repr;
const ReprStruct = zest.ReprStruct;
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
    i32: i32,
    f32: f32,
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
    ref_get_end: struct {
        offset: u32,
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
    make_end: struct {
        to: Repr,
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
    equal_i32,
    less_than_i32,
    less_than_or_equal_i32,
    more_than_i32,
    more_than_or_equal_i32,
    add_i32,
    subtract_i32,
    multiply_i32,
    memory_size,
    memory_grow,
    heap_start,
    size_of: i32,
    load: Repr,
    store: Repr,

    pub fn hasSideEffects(builtin: BuiltinTyped) bool {
        return switch (builtin) {
            .dummy, .equal_i32, .less_than_i32, .less_than_or_equal_i32, .more_than_i32, .more_than_or_equal_i32, .add_i32, .subtract_i32, .multiply_i32, .memory_size, .heap_start, .size_of => false,
            .memory_grow, .load, .store => true,
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
};
