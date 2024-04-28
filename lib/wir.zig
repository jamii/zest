//! Wasm IR

const std = @import("std");
const wasm = std.wasm;
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;

const zest = @import("./zest.zig");
const fieldType = zest.fieldType;
const List = zest.List;
const tir = zest.tir;

pub const Local = struct { id: usize };

pub const LocalData = struct {
    type: wasm.Valtype,
};

pub const Expr = struct { id: usize };

pub const ExprData = union(enum) {
    i32_const: i32,
    f32_const: f32,
    ptr_to_const: Constant,
    local_get: Local,
    local_set: Local,
    load: struct { offset: u32 },
    store: struct { offset: u32 },
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

pub const Constant = struct { id: usize };

pub const Fun = struct { id: usize };

pub const FunData = struct {
    tir_fun: tir.Fun,

    arg_types: ArrayList(wasm.Valtype),
    return_types: ArrayList(wasm.Valtype),

    local_data: List(Local, LocalData),

    expr_data: List(Expr, ExprData),

    pub fn init(
        allocator: Allocator,
        tir_fun: tir.Fun,
    ) FunData {
        return .{
            .tir_fun = tir_fun,

            .arg_types = fieldType(FunData, .arg_types).init(allocator),
            .return_types = fieldType(FunData, .return_types).init(allocator),

            .local_data = fieldType(FunData, .local_data).init(allocator),

            .expr_data = fieldType(FunData, .expr_data).init(allocator),
        };
    }
};
