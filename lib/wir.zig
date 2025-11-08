//! Wasm IR

const std = @import("std");
const wasm = std.wasm;
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;

const zest = @import("./zest.zig");
const deepEqual = zest.deepEqual;
const List = zest.List;
const Repr = zest.Repr;
const ReprStruct = zest.ReprStruct;
const ReprUnion = zest.ReprUnion;
const ReprFun = zest.ReprFun;
const ReprNamespace = zest.ReprNamespace;
const Value = zest.Value;
const tir = zest.tir;

pub const Walue = union(enum) {
    closure,
    arg: tir.Arg,
    @"return",
    local: Local,
    shadow,
    stack: Repr, // must be primitive
    u32: u32,
    i64: i64,
    string: []const u8,
    @"struct": struct {
        repr: ReprStruct,
        values: []Walue, // may not contain .stack
    },
    @"union": struct {
        repr: ReprUnion,
        // tag and value are null iff repr.keys.len == 0
        tag: ?u32,
        value: ?*Walue, // may not contain .stack
    },
    fun: struct {
        repr: ReprFun,
        closure: *Walue, // may not contain .stack
    },
    only: struct {
        value: *Value,
    },
    namespace: struct {
        repr: ReprNamespace,
    },
    value_at: struct {
        ptr: *Walue,
        repr: Repr,
    },
    add: struct {
        walue: *Walue,
        offset: u32,
    },

    pub fn equal(self: Walue, other: Walue) bool {
        // TODO May have to think about this
        return deepEqual(self, other);
    }

    pub fn emptyStruct() Walue {
        return .{ .@"struct" = .{ .repr = Repr.emptyStruct().@"struct", .values = &.{} } };
    }

    pub fn emptyUnion() Walue {
        return .{ .@"union" = .{ .repr = Repr.emptyUnion().@"union", .tag = null, .value = null } };
    }
};

pub const Destination = union(enum) {
    nowhere,
    anywhere,
    stack,
    value_at: *Walue,
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
    repr: Repr,
};

pub const Fun = struct { id: usize };

pub const FunData = struct {
    tir_fun: tir.Fun,
    fun_type: FunType,

    local_data: List(Local, LocalData),
    local_shadow: ?Local,
    local_shufflers: std.enums.EnumArray(wasm.Valtype, ?Local),

    wasm: ArrayList(u8),

    shadow_offset_next: usize,
    shadow_offset_max: usize,

    is_leaf: bool,

    pub fn init(
        allocator: Allocator,
        tir_fun: tir.Fun,
        fun_type: FunType,
    ) FunData {
        return .{
            .tir_fun = tir_fun,
            .fun_type = fun_type,

            .local_data = .init(allocator),
            .local_shadow = null,
            .local_shufflers = .initFill(null),

            .wasm = .init(allocator),

            .shadow_offset_max = 0,
            .shadow_offset_next = 0,

            .is_leaf = true,
        };
    }
};
