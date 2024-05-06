//! Wasm IR

const std = @import("std");
const wasm = std.wasm;
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;

const zest = @import("./zest.zig");
const fieldType = zest.fieldType;
const List = zest.List;
const Value = zest.Value;
const Repr = zest.Repr;
const tir = zest.tir;

pub const AddressDirect = union(enum) {
    closure,
    arg,
    @"return",
    local: Local,
    shadow,
    stack,
    nowhere,
};

pub const AddressIndirect = struct {
    offset: u32,
    repr: Repr,
};

pub const Address = struct {
    direct: AddressDirect,
    indirect: ?AddressIndirect = null,
};

pub const FunType = struct { id: usize };

pub const FunTypeData = struct {
    arg_types: []wasm.Valtype,
    return_types: []wasm.Valtype,
};

pub const Block = struct {
    offset_next: usize,
};

pub const Local = struct { id: usize };

pub const LocalData = struct {
    type: wasm.Valtype,
};

pub const FunData = struct {
    fun_type: FunType,
    return_arg_count: u32,

    local_data: List(Local, LocalData),

    wasm: ArrayList(u8),

    shadow_offset_next: usize,
    shadow_offset_max: usize,

    pub fn init(
        allocator: Allocator,
        fun_type: FunType,
        return_arg_count: u32,
    ) FunData {
        return .{
            .fun_type = fun_type,
            .return_arg_count = return_arg_count,

            .local_data = fieldType(FunData, .local_data).init(allocator),

            .wasm = fieldType(FunData, .wasm).init(allocator),

            .shadow_offset_max = 0,
            .shadow_offset_next = 0,
        };
    }
};
