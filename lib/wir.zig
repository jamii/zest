//! Wasm IR

const std = @import("std");
const wasm = std.wasm;
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;

const zest = @import("./zest.zig");
const fieldType = zest.fieldType;
const List = zest.List;
const Value = zest.Value;
const tir = zest.tir;

pub const Arg = struct { id: usize };

pub const Local = struct { id: usize };

pub const LocalData = struct {
    type: wasm.Valtype,
};

pub const Expr = struct { id: usize };

pub const ExprData = union(enum) {
    i32: i32,
    f32: f32,
    ptr_to_constant: Constant,
    arg: Arg,
    local_get: Local,
    local_set: Local,
    load: struct {
        type: wasm.Valtype,
        address: Address,
    },
    store: struct {
        type: wasm.Valtype,
        address: Address,
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

pub const Address = struct {
    base: union(enum) {
        @"return",
        arg: Arg,
        shadow,
    },
    offset: u32,
};

pub const Constant = struct { id: usize };

pub const FunType = struct { id: usize };

pub const FunTypeData = struct {
    arg_types: []wasm.Valtype,
    return_types: []wasm.Valtype,
};

pub const Fun = struct { id: usize };

pub const ShadowAddress = struct {
    shadow: tir.Shadow,
    offset: usize,
};

pub const Block = struct {
    block_begin: tir.Expr,
    shadow_offset_next: usize,
    shadow_address_index: usize,
};

pub const FunData = struct {
    tir_fun: tir.Fun,

    fun_type: FunType,

    local_data: List(Local, LocalData),
    local_from_tir: List(tir.Local, ?Local), // null if moved to shadow stack

    expr_data: List(Expr, ExprData),

    block_stack: ArrayList(Block),

    shadow_address_stack: ArrayList(ShadowAddress),
    shadow_offset_next: usize,
    shadow_offset_max: usize,

    pub fn init(
        allocator: Allocator,
        tir_fun: tir.Fun,
        fun_type: FunType,
    ) FunData {
        return .{
            .tir_fun = tir_fun,

            .fun_type = fun_type,

            .local_data = fieldType(FunData, .local_data).init(allocator),
            .local_from_tir = fieldType(FunData, .local_from_tir).init(allocator),

            .expr_data = fieldType(FunData, .expr_data).init(allocator),

            .shadow_address_stack = fieldType(FunData, .shadow_address_stack).init(allocator),
            .block_stack = fieldType(FunData, .block_stack).init(allocator),
            .shadow_offset_max = 0,
            .shadow_offset_next = 0,
        };
    }
};
