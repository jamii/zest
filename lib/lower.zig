const std = @import("std");
const panic = std.debug.panic;
const assert = std.debug.assert;
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;

const zest = @import("./zest.zig");
const oom = zest.oom;
const deepEqual = zest.deepEqual;
const Compiler = zest.Compiler;
const Expr = zest.Expr;
const ExprData = zest.ExprData;
const ObjectExprData = zest.ObjectExprData;
const Function = zest.Function;
const FunctionData = zest.FunctionData;
const Node = zest.Node;
const NodeData = zest.NodeData;
const Value = zest.Value;
const AbstractValue = zest.AbstractValue;

pub fn lower(c: *Compiler) error{LowerError}!void {
    c.function_main = try lowerFunction(c, .{ .keys = &.{}, .values = &.{} }, c.expr_data.lastKey().?);
}

fn lowerFunction(c: *Compiler, args: ObjectExprData, body: Expr) error{LowerError}!Function {
    var f = FunctionData.init(c.allocator);
    const arg = f.node_data.append(.{ .arg_get = .{ .arg = .{ .id = 0 } } });
    try lowerObjectPattern(c, &f, arg, args);
    const result_node = try lowerExpr(c, &f, body);
    _ = f.node_data.append(.{ .@"return" = result_node });
    return c.function_data.append(f);
}

fn lowerObjectPattern(c: *Compiler, f: *FunctionData, object: Node, pattern: ObjectExprData) error{LowerError}!void {
    for (pattern.keys, pattern.values) |key_expr, value_expr| {
        const key = try evalKey(c, f, key_expr);
        const value_node = f.node_data.append(.{ .get = .{ .object = object, .key = key } });
        try lowerPattern(c, f, value_node, value_expr);
    }
}

fn lowerPattern(c: *Compiler, f: *FunctionData, input: Node, pattern: Expr) error{LowerError}!void {
    _ = f;
    const expr_data = c.expr_data.get(pattern);
    switch (expr_data) {
        .name => |name| {
            c.scope.push(.{
                .name = name,
                .value = .{ .node = input },
            });
        },
        //.object => |object| {
        //    TODO assert input is a struct?
        //    try lowerObjectPattern(c, f, input, object);
        //}
        else => return fail(c, pattern, "Invalid pattern", .{}),
    }
}

fn lowerExpr(c: *Compiler, f: *FunctionData, expr: Expr) error{LowerError}!Node {
    const expr_data = c.expr_data.get(expr);
    switch (expr_data) {
        .i32 => |i| {
            return f.node_data.append(.{ .value = .{ .i32 = i } });
        },
        .object => |object| {
            return f.node_data.append(try lowerObject(c, f, object));
        },
        .name => |name| {
            const binding = c.scope.lookup(name) orelse
                return fail(c, expr, "Not defined: {s}", .{name});
            switch (binding.value) {
                .node => |node| return node,
                .function, .intrinsic => return fail(c, expr, "You may not use a function here", .{}),
            }
        },
        .intrinsic => {
            return fail(c, expr, "Intrinsics may only be called", .{});
        },
        .builtin => |builtin| {
            switch (builtin) {
                .i32 => return f.node_data.append(.{ .value = .{ .repr = .i32 } }),
            }
        },
        .let => |let| {
            const value = try lowerExprOrFn(c, f, let.value);
            if (let.mut) return fail(c, expr, "TODO", .{});
            c.scope.push(.{
                .name = let.name,
                .value = value,
            });
            return f.node_data.append(.{ .value = Value.emptyStruct() });
        },
        .@"fn" => return fail(c, expr, "You may not create a function here", .{}),
        .call => |call| {
            const head = try lowerExprOrFn(c, f, call.head);
            const args = try lowerObject(c, f, call.args);
            switch (head) {
                .node => return fail(c, expr, "Cannot call {}", .{head}),
                .function => |function| {
                    const arg = f.node_data.append(args);
                    return f.node_data.append(.{ .call = .{
                        .function = function,
                        .specialization = null,
                        .args = c.dupeOne(arg),
                    } });
                },
                .intrinsic => |intrinsic| {
                    switch (intrinsic) {
                        .@"i32-add" => {
                            if (!deepEqual(@as([]const Value, &.{ .{ .i32 = 0 }, .{ .i32 = 1 } }), args.struct_init.keys))
                                return fail(c, expr, "Invalid call to intrinsic", .{});
                            return f.node_data.append(.{ .add = .{
                                args.struct_init.values[0],
                                args.struct_init.values[1],
                            } });
                        },
                        .@"i32-store" => {
                            try matchKeys(c, expr, args.struct_init.keys, .{ 0, "to" });
                            return f.node_data.append(.{ .store = .{
                                .value = args.struct_init.values[0],
                                .to = args.struct_init.values[1],
                            } });
                        },
                        .@"i32-load" => {
                            try matchKeys(c, expr, args.struct_init.keys, .{0});
                            return f.node_data.append(.{ .load = .{
                                .from = args.struct_init.values[0],
                                .repr = .i32,
                            } });
                        },
                        .@"memory-copy" => {
                            try matchKeys(c, expr, args.struct_init.keys, .{ "from", "to", "byte-count" });
                            const byte_count = try evalExpr(c, f, call.args.values[2]);
                            if (byte_count != .i32)
                                return fail(c, expr, "Invalid call to intrinsic", .{});

                            return f.node_data.append(.{ .copy = .{
                                .from = args.struct_init.values[0],
                                .to = args.struct_init.values[1],
                                .byte_count = @intCast(byte_count.i32),
                            } });
                        },
                    }
                },
            }
        },
        .get => |get| {
            const object = try lowerExpr(c, f, get.object);
            const key = try evalKey(c, f, get.key);
            return f.node_data.append(.{ .get = .{ .object = object, .key = key } });
        },
        .statements => |statements| {
            const scope_saved = c.scope.save();
            defer c.scope.restore(scope_saved);

            if (statements.len == 0) {
                return f.node_data.append(.{ .value = Value.emptyStruct() });
            } else {
                var node: ?Node = null;
                for (statements) |statement| {
                    node = try lowerExpr(c, f, statement);
                }
                return node.?;
            }
        },
        else => return fail(c, expr, "TODO", .{}),
    }
}

fn matchKeys(c: *Compiler, expr: Expr, actual_keys: []const Value, expected_keys: anytype) error{LowerError}!void {
    if (actual_keys.len != expected_keys.len)
        return fail(c, expr, "Expected {} args, found {} args", .{ expected_keys.len, actual_keys.len });
    inline for (expected_keys, 0..) |expected_key, i| {
        const actual_key = actual_keys[i];
        switch (@TypeOf(expected_key)) {
            comptime_int => {
                if (actual_key != .i32 or
                    actual_key.i32 != @as(i32, expected_key))
                    return fail(c, expr, "Expected key {}, found key {}", .{ expected_key, actual_key });
            },
            else => {
                if (actual_key != .string or
                    !std.mem.eql(u8, actual_key.string, expected_key))
                    return fail(c, expr, "Expected key '{s}', found key {}", .{ expected_key, actual_key });
            },
        }
    }
}

fn lowerObject(c: *Compiler, f: *FunctionData, object: ObjectExprData) error{LowerError}!NodeData {
    const keys = c.allocator.alloc(Value, object.keys.len) catch oom();
    for (keys, object.keys) |*key_node, key| key_node.* = try evalKey(c, f, key);

    const values = c.allocator.alloc(Node, object.values.len) catch oom();
    for (values, object.values) |*value_node, value| value_node.* = try lowerExpr(c, f, value);

    // TODO sort keys and values
    return NodeData{ .struct_init = .{ .keys = keys, .values = values } };
}

fn lowerExprOrFn(c: *Compiler, f: *FunctionData, expr: Expr) error{LowerError}!AbstractValue {
    const expr_data = c.expr_data.get(expr);
    switch (expr_data) {
        .name => |name| {
            const binding = c.scope.lookup(name) orelse
                return fail(c, expr, "Not defined: {s}", .{name});
            return binding.value;
        },
        .intrinsic => |intrinsic| {
            return .{ .intrinsic = intrinsic };
        },
        .@"fn" => |@"fn"| {
            return .{ .function = try lowerFunction(c, @"fn".params, @"fn".body) };
        },
        else => {
            return .{ .node = try lowerExpr(c, f, expr) };
        },
    }
}

fn evalKey(c: *Compiler, f: *FunctionData, expr: Expr) error{LowerError}!Value {
    const expr_data = c.expr_data.get(expr);
    switch (expr_data) {
        .name => |name| return .{ .string = name },
        else => return evalExpr(c, f, expr),
    }
}

fn evalExpr(c: *Compiler, f: *FunctionData, expr: Expr) error{LowerError}!Value {
    _ = f;
    const expr_data = c.expr_data.get(expr);
    switch (expr_data) {
        .i32 => |i| return .{ .i32 = i },
        .string => |string| return .{ .string = string },
        else => return fail(c, expr, "Can't const-eval", .{}),
    }
}

fn fail(c: *Compiler, expr: Expr, comptime message: []const u8, args: anytype) error{LowerError} {
    const expr_data = c.expr_data.get(expr);
    c.error_message = std.fmt.allocPrint(
        c.allocator,
        "At {}={}. " ++
            message,
        .{ expr, expr_data } ++
            args,
    ) catch oom();
    return error.LowerError;
}
