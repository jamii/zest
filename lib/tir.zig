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
    arg,
    local_get: Local,

    nop_begin,
    nop_end,
    struct_init_begin,
    struct_init_end: ReprStruct,
    fun_init_begin,
    fun_init_end: ReprFun,
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
    call_builtin_begin,
    call_builtin_end: BuiltinTyped,
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

    pub fn treePart(expr_data: ExprData) TreePart {
        switch (expr_data) {
            inline else => |_, tag| {
                if (std.mem.endsWith(u8, @tagName(tag), "_begin")) {
                    return .branch_begin;
                } else if (std.mem.endsWith(u8, @tagName(tag), "_end")) {
                    return .branch_end;
                } else {
                    return .leaf;
                }
            },
        }
    }
};

pub const BuiltinTyped = enum {
    equal_i32,
    less_than_i32,
    less_than_or_equal_i32,
    more_than_i32,
    more_than_or_equal_i32,
    add_i32,
    subtract_i32,
    multiply_i32,
};

pub const FunKey = struct {
    fun: dir.Fun,
    closure_repr: Repr,
    arg_repr: Repr,
};

pub const Fun = struct { id: usize };

pub const FunData = struct {
    key: FunKey,

    local_data: List(Local, LocalData),

    expr_data: List(Expr, ExprData),
    expr_repr: List(Expr, ?Repr), // null for exprs that don't return a value

    return_repr: FlatLattice(Repr),

    pub fn init(allocator: Allocator, key: FunKey) FunData {
        return .{
            .key = key,

            .local_data = fieldType(FunData, .local_data).init(allocator),

            .expr_data = fieldType(FunData, .expr_data).init(allocator),
            .expr_repr = fieldType(FunData, .expr_repr).init(allocator),

            .return_repr = .zero,
        };
    }
};

pub const Frame = struct {
    key: FunKey,
    fun: Fun,
    expr: dir.Expr,
};
