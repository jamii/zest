//! Dynamic IR

const std = @import("std");
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;

const zest = @import("zest.zig");
const oom = zest.oom;
const fieldType = zest.fieldType;
const List = zest.List;
const Map = zest.Map;
const Value = zest.Value;

pub const Local = struct { id: usize };

pub const LocalData = struct {
    is_mutable: bool,
};

pub const Expr = struct { id: usize };

pub const ExprData = union(enum) {
    i32: i32,
    f32: f32,
    string: []const u8,
    arg,
    closure,
    local_get: Local,
    ref_set_middle,

    begin,
    stage,
    unstage,

    struct_init: usize,
    fun_init: struct {
        fun: Fun,
    },
    local_let: Local,
    assert_object: struct {
        count: usize,
    },
    assert_is_ref,
    assert_has_no_ref,
    object_get,
    ref_init,
    ref_get,
    ref_set,
    ref_deref,
    call,
    drop,
    block,
    @"return",

    pub fn isEnd(expr_data: ExprData) bool {
        return switch (expr_data) {
            .i32, .f32, .string, .arg, .closure, .local_get, .ref_set_middle, .begin, .stage, .unstage => false,
            .struct_init, .fun_init, .local_let, .assert_object, .assert_is_ref, .assert_has_no_ref, .object_get, .ref_init, .ref_get, .ref_set, .ref_deref, .call, .drop, .block, .@"return" => true,
        };
    }
};

// Push in order.
// Pop in reverse order.
pub fn ExprInput(comptime T: type) type {
    return union(std.meta.Tag(ExprData)) {
        i32,
        f32,
        string,
        arg,
        closure,
        local_get,
        ref_set_middle,

        begin,
        stage,
        unstage,

        struct_init: struct {
            keys: []Value,
            values: []T,
        },
        fun_init: struct {
            closure: T,
        },
        local_let: struct {
            value: T,
        },
        assert_object: struct {
            value: T,
        },
        assert_is_ref: struct {
            value: T,
        },
        assert_has_no_ref: struct {
            value: T,
        },
        object_get: struct {
            object: T,
            key: Value,
        },
        ref_init: struct {
            value: T,
        },
        ref_get: struct {
            ref: T,
            key: Value,
        },
        ref_set: struct {
            ref: T,
            value: T,
        },
        ref_deref: struct {
            ref: T,
        },
        call: struct {
            fun: T,
            args: T,
        },
        drop: struct {
            value: T,
        },
        block: struct {
            value: T,
        },
        @"return": struct {
            value: T,
        },
    };
}

pub fn ExprOutput(comptime T: type) type {
    return union(std.meta.Tag(ExprData)) {
        i32: T,
        f32: T,
        string: T,
        arg: T,
        closure: T,
        local_get: T,
        ref_set_middle,

        begin,
        stage: T,
        unstage: T,

        struct_init: T,
        fun_init: T,
        local_let,
        assert_object: T,
        assert_is_ref: T,
        assert_has_no_ref: T,
        object_get: T,
        ref_init: T,
        ref_get: T,
        ref_set,
        ref_deref: T,
        call: T,
        drop,
        block: T,
        @"return",
    };
}

pub const Fun = struct { id: usize };

pub const FunData = struct {
    closure_keys_index: Map([]const u8, void),
    closure_keys: ArrayList([]const u8),

    pattern_local_count: usize,
    local_data: List(Local, LocalData),

    pattern_expr_count: usize,
    expr_data: List(Expr, ExprData),

    pub fn init(allocator: Allocator) FunData {
        return .{
            .closure_keys_index = fieldType(FunData, .closure_keys_index).init(allocator),
            .closure_keys = fieldType(FunData, .closure_keys).init(allocator),

            .pattern_local_count = 0,
            .local_data = fieldType(FunData, .local_data).init(allocator),

            .pattern_expr_count = 0,
            .expr_data = fieldType(FunData, .expr_data).init(allocator),
        };
    }
};

pub const Scope = struct {
    closure_until_len: usize,
    staged_until_len: ?usize,
    bindings: ArrayList(Binding),

    pub fn init(allocator: Allocator) Scope {
        return .{
            .closure_until_len = 0,
            .staged_until_len = null,
            .bindings = fieldType(Scope, .bindings).init(allocator),
        };
    }

    pub fn push(self: *Scope, binding: Binding) void {
        self.bindings.append(binding) catch oom();
    }

    pub fn save(self: *Scope) usize {
        return self.bindings.items.len;
    }

    pub fn restore(self: *Scope, saved: usize) void {
        self.bindings.shrinkRetainingCapacity(saved);
    }

    pub fn lookup(self: *Scope, name: []const u8) ?BindingInfo {
        var i: usize = self.bindings.items.len;
        while (i > 0) : (i -= 1) {
            const binding = self.bindings.items[i - 1];
            if (std.mem.eql(u8, binding.name, name)) {
                return BindingInfo{
                    .name = binding.name,
                    .value = if (i - 1 < self.closure_until_len)
                        .{ .closure = binding.name }
                    else
                        binding.value,
                    .is_staged = i - 1 < (self.staged_until_len orelse 0),
                };
            }
        }
        // TODO builtins
        //inline for (@typeInfo(Builtin).Enum.fields) |field| {
        //    if (std.mem.eql(u8, name, field.name)) {
        //        return c.sir_expr_data.append(.{ .builtin = @as(Builtin, @enumFromInt(field.value)) });
        //    }
        //}
        return null;
    }
};

pub const Binding = struct {
    name: []const u8,
    value: Walue,
};

pub const BindingInfo = struct {
    name: []const u8,
    value: Walue,
    is_staged: bool,
};

pub const Walue = union(enum) {
    arg,
    closure: []const u8,
    local: Local,
};

pub const Frame = struct {
    fun: Fun,
    arg: Value,
    closure: Value,
    expr: Expr = .{ .id = 0 },
};
