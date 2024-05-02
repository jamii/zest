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
    local_let: Local,
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

pub fn ExprInput(comptime T: type) type {
    return union(std.meta.Tag(ExprData)) {
        i32,
        f32,
        string,
        struct_init: []T,
        fun_init: T,
        arg,
        closure,
        local_get,
        local_let: T,
        object_get: T,
        call: [2]T,
        drop: T,
        block_begin,
        block_end,
        @"return": T,
    };
}

pub fn ExprOutput(comptime T: type) type {
    return union(std.meta.Tag(ExprData)) {
        i32: T,
        f32: T,
        string: T,
        struct_init: T,
        fun_init: T,
        arg: T,
        closure: T,
        local_get: T,
        local_let,
        object_get: T,
        call: T,
        drop,
        block_begin,
        block_end,
        @"return",
    };
}

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

    expr_address: List(Expr, ?Address), // null for exprs that don't return a value
    shadow_data: List(Shadow, void),

    pub fn init(allocator: Allocator, key: FunKey) FunData {
        return .{
            .key = key,

            .local_data = fieldType(FunData, .local_data).init(allocator),

            .expr_data = fieldType(FunData, .expr_data).init(allocator),
            .expr_repr = fieldType(FunData, .expr_repr).init(allocator),

            .return_repr = .zero,

            .expr_address = fieldType(FunData, .expr_address).init(allocator),
            .shadow_data = fieldType(FunData, .shadow_data).init(allocator),
        };
    }
};

pub const Shadow = struct { id: usize };

pub const Address = struct {
    base: union(enum) {
        @"return",
        local: Local,
        shadow: Shadow,
    },
    offset: usize = 0,
};

pub const Frame = struct {
    key: FunKey,
    fun: Fun,
    expr: dir.Expr,
};
