//! Typed IR

const std = @import("std");
const Allocator = std.mem.Allocator;

const zest = @import("./zest.zig");
const fieldType = zest.fieldType;
const List = zest.List;
const Repr = zest.Repr;
const Value = zest.Value;
const FlatLattice = zest.FlatLattice;
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
    ref_set_middle,

    begin,
    nop,

    struct_init,
    fun_init,
    local_let: Local,
    object_get: struct {
        index: usize,
        offset: u32,
    },
    ref_init,
    ref_get: struct {
        index: usize,
        offset: u32,
    },
    ref_set,
    ref_deref,
    call: Fun,
    drop,
    block,
    @"return",

    pub fn isEnd(expr_data: ExprData) bool {
        return switch (expr_data) {
            .i32, .f32, .string, .arg, .closure, .local_get, .ref_set_middle, .begin => false,
            .nop, .struct_init, .fun_init, .local_let, .object_get, .ref_init, .ref_get, .ref_set, .ref_deref, .call, .drop, .block, .@"return" => true,
        };
    }
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
