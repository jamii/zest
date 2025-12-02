//! Dynamic IR

const std = @import("std");
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;

const zest = @import("zest.zig");
const oom = zest.oom;
const List = zest.List;
const Map = zest.Map;
const Value = zest.Value;
const Builtin = zest.Builtin;
const TreePart = zest.TreePart;
const Compiler = zest.Compiler;

pub const Arg = struct { id: usize };

pub const ArgData = struct {};

pub const Local = struct { id: usize };

pub const LocalData = struct {
    is_tmp: bool,
    is_mutable: bool,
};

pub const Expr = struct { id: usize };

pub const ExprData = union(enum) {
    i64: i64,
    f64: f64,
    string: []const u8,
    repr_u32,
    repr_i64,
    repr_string,
    repr_any,
    repr_repr,
    repr_kind_struct,
    repr_kind_union,
    repr_kind_list,
    repr_kind_only,
    repr_kind_namespace,
    arg: Arg,
    closure,
    // TODO Once namespaces can close over values, functions will have to close over namespaces, and .namespace should refer to the namespace of the current fun rather than hardcoding it
    namespace: Namespace,
    local_get: Local,

    struct_init: struct {
        count: usize,
    },
    fun_init: struct {
        fun: Fun,
    },
    local_let: Local,
    assert_object: struct {
        count: usize,
    },
    assert_is_ref,
    assert_has_no_ref_visible,
    assert_has_no_ref,
    object_get,
    namespace_get,
    ref_init,
    ref_get,
    ref_set,
    ref_deref,
    call: struct {
        arg_count: usize,
    },
    call_builtin: Builtin,
    make,
    block: struct {
        count: usize,
    },
    @"if",
    @"while": struct {
        // Points to expr before while condition in expr_data_post.
        begin: Expr,
    },
    @"return",
    stage: Mapping,
    unstage,
    repr_of,

    // Placeholders to help eval remember what it is doing.
    if_then,
    if_else,
    while_begin,
    while_body,
    stage_begin,
    unstage_begin: Mapping,
    repr_of_begin: Mapping,

    pub fn childCount(expr_data: ExprData, c: *Compiler) usize {
        _ = c;
        return switch (expr_data) {
            .i64, .f64, .string, .repr_u32, .repr_i64, .repr_string, .repr_any, .repr_repr, .repr_kind_struct, .repr_kind_union, .repr_kind_list, .repr_kind_only, .repr_kind_namespace, .arg, .closure, .namespace, .local_get, .if_then, .if_else, .while_begin, .while_body, .stage_begin, .unstage_begin, .repr_of_begin => 0,
            .fun_init, .local_let, .assert_object, .assert_is_ref, .assert_has_no_ref_visible, .assert_has_no_ref, .ref_init, .ref_deref, .@"return" => 1,
            .object_get, .namespace_get, .ref_get, .ref_set, .make, .stage, .unstage, .repr_of => 2,
            .@"while" => 4,
            .@"if" => 5,
            .struct_init => |struct_init| 2 * struct_init.count,
            .call => |call| 1 + call.arg_count,
            .call_builtin => |builtin| builtin.argCount(),
            .block => |block| block.count,
        };
    }
};

// Maps between expr_data_pre and expr_data_post
pub const Mapping = struct {
    mapping: Expr = .{ .id = 0 },
};

pub const Fun = struct { id: usize };

pub const FunData = struct {
    @"inline": bool,
    closure_keys_index: Map([]const u8, void),
    closure_keys: ArrayList([]const u8),
    arg_data: List(Arg, ArgData),
    local_data: List(Local, LocalData),
    expr_data_post: List(Expr, ExprData),
    expr_data_pre: List(Expr, ExprData),

    pub fn init(allocator: Allocator) FunData {
        return .{
            .@"inline" = false,
            .closure_keys_index = .init(allocator),
            .closure_keys = .init(allocator),
            .arg_data = .init(allocator),
            .local_data = .init(allocator),
            .expr_data_post = .init(allocator),
            .expr_data_pre = .init(allocator),
        };
    }
};

pub const Namespace = struct { id: usize };

pub const NamespaceData = struct {
    definition_by_name: Map([]const u8, Definition),
    definition_data: List(Definition, DefinitionData),

    pub fn init(allocator: Allocator) NamespaceData {
        return .{
            .definition_by_name = .init(allocator),
            .definition_data = .init(allocator),
        };
    }
};

pub const Definition = struct { id: usize };

pub const DefinitionData = struct {
    fun: Fun,
    value: union(enum) {
        unevaluated,
        evaluating,
        evaluated: Value,
    },
};

pub const predefined_bindings: [10]Binding = .{
    .{ .name = "u32", .value = .{ .constant = .repr_u32 }, .mut = false },
    .{ .name = "i64", .value = .{ .constant = .repr_i64 }, .mut = false },
    .{ .name = "string", .value = .{ .constant = .repr_string }, .mut = false },
    .{ .name = "struct", .value = .{ .constant = .repr_kind_struct }, .mut = false },
    .{ .name = "union", .value = .{ .constant = .repr_kind_union }, .mut = false },
    .{ .name = "list", .value = .{ .constant = .repr_kind_list }, .mut = false },
    .{ .name = "only", .value = .{ .constant = .repr_kind_only }, .mut = false },
    .{ .name = "any", .value = .{ .constant = .repr_any }, .mut = false },
    .{ .name = "namespace", .value = .{ .constant = .repr_kind_namespace }, .mut = false },
    .{ .name = "repr", .value = .{ .constant = .repr_repr }, .mut = false },
};

pub const Scope = struct {
    closure_until_len: usize,
    staged_until_len: ?usize,
    bindings: ArrayList(Binding),

    pub fn init(allocator: Allocator) Scope {
        var scope = Scope{
            .closure_until_len = 0,
            .staged_until_len = null,
            .bindings = .init(allocator),
        };
        scope.bindings.appendSlice(&predefined_bindings) catch oom();
        return scope;
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
                    .value = if (!binding.value.isStatic() and i - 1 < self.closure_until_len)
                        .{ .closure = binding.name }
                    else
                        binding.value,
                    .mut = binding.mut,
                    .is_staged = !binding.value.isStatic() and i - 1 < (self.staged_until_len orelse 0),
                };
            }
        }
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
    arg: Arg,
    closure: []const u8,
    local: Local,
    constant: ExprData,
    definition: struct {
        namespace: Namespace,
        name: []const u8,
    },

    pub fn isStatic(walue: Walue) bool {
        return switch (walue) {
            .constant, .definition => true,
            .arg, .closure, .local => false,
        };
    }
};

pub const Frame = struct {
    fun: Fun,
    args: []Value,
    closure: Value,
    expr: Expr = .{ .id = 0 },
    memo: ?struct {
        namespace: Namespace,
        definition: Definition,
    } = null,
};
