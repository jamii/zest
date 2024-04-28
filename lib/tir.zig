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
};

pub const Expr = struct { id: usize };

pub const ExprData = union(enum) {
    i32: i32,
    f32: f32,
    string: []const u8,
    struct_init,
    fun_init,
    arg,
    closure,
    local_get: Local,
    local_set: Local,
    object_get: struct {
        key: Value,
    },
    call: Fun,
    drop,
    block_begin: struct {
        expr_count: usize,
    },
    block_end: struct {
        expr_count: usize,
    },
    @"return",
};

pub const FunKey = struct {
    fun: dir.Fun,
    closure_repr: Repr,
    arg_repr: Repr,
};

pub const Fun = struct { id: usize };

pub const FunData = struct {
    local_data: List(Local, LocalData),

    expr_data: List(Expr, ExprData),
    expr_repr: List(Expr, ?Repr), // null for exprs that don't return a value

    return_repr: FlatLattice(Repr),

    pub fn init(allocator: Allocator) FunData {
        return .{
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
