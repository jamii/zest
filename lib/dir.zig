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
const Builtin = zest.Builtin;
const TreePart = zest.TreePart;

pub const Local = struct { id: usize };

pub const LocalData = struct {
    is_mutable: bool,
    is_tmp: bool,
};

pub const Expr = struct { id: usize };

pub const ExprData = union(enum) {
    i32: i32,
    f32: f32,
    string: []const u8,
    arg,
    closure,
    local_get: Local,

    nop_begin,
    nop_end,
    struct_init_begin,
    struct_init_end: usize,
    fun_init_begin,
    fun_init_end: struct {
        fun: Fun,
    },
    local_let_begin,
    local_let_end: Local,
    assert_object_begin,
    assert_object_end: struct {
        count: usize,
    },
    assert_is_ref_begin,
    assert_is_ref_end,
    assert_has_no_ref_visible_begin,
    assert_has_no_ref_visible_end,
    assert_has_no_ref_begin,
    assert_has_no_ref_end,
    object_get_begin,
    object_get_end,
    ref_init_begin,
    ref_init_end,
    ref_get_begin,
    ref_get_end,
    ref_set_begin,
    ref_set_end,
    ref_deref_begin,
    ref_deref_end,
    call_begin,
    call_end,
    call_builtin_begin,
    call_builtin_end: Builtin,
    block_begin,
    block_last,
    block_end,
    return_begin,
    return_end,

    if_begin,
    if_then,
    if_else,
    if_end,
    while_begin,
    while_body,
    while_end,

    stage_begin,
    stage_end,
    unstage_begin,
    unstage_end,

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
                    .mut = binding.mut,
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
    mut: bool,
};

pub const BindingInfo = struct {
    name: []const u8,
    value: Walue,
    mut: bool,
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
