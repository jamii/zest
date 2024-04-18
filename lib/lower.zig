const std = @import("std");
const panic = std.debug.panic;
const assert = std.debug.assert;
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;

const zest = @import("./zest.zig");
const oom = zest.oom;
const deepEqual = zest.deepEqual;
const Compiler = zest.Compiler;
const SirExpr = zest.SirExpr;
const SirExprData = zest.SirExprData;
const SirObject = zest.SirObject;
const DirExpr = zest.DirExpr;
const DirExprData = zest.DirExprData;
const DirFun = zest.DirFun;
const DirFunData = zest.DirFunData;
const Value = zest.Value;
const AbstractValue = zest.AbstractValue;

pub fn lower(c: *Compiler) error{LowerError}!void {
    c.dir_fun_main = try lowerFun(c, .{ .keys = &.{}, .values = &.{} }, c.sir_expr_data.lastKey().?);
}

fn lowerFun(c: *Compiler, params: SirObject, body: SirExpr) error{LowerError}!DirFun {
    var f = DirFunData.init(c.allocator);
    const args = f.expr_data.append(.arg);
    try lowerObjectPattern(c, &f, args, params);
    const result_dir = try lowerExpr(c, &f, body);
    _ = f.expr_data.append(.{ .@"return" = result_dir });
    return c.dir_fun_data.append(f);
}

fn lowerObjectPattern(c: *Compiler, f: *DirFunData, object: DirExpr, pattern: SirObject) error{LowerError}!void {
    for (pattern.keys, pattern.values) |key_expr, value_expr| {
        const key = try lowerKey(c, f, key_expr);
        const value_dir = f.expr_data.append(.{ .get = .{ .object = object, .key = key } });
        try lowerPattern(c, f, value_dir, value_expr);
    }
}

fn lowerKey(c: *Compiler, f: *DirFunData, expr: SirExpr) error{LowerError}!DirExpr {
    const expr_data = c.sir_expr_data.get(expr);
    switch (expr_data) {
        .name => |name| return f.expr_data.append(.{ .string = name }),
        else => return lowerExpr(c, f, expr),
    }
}

fn lowerPattern(c: *Compiler, f: *DirFunData, input: DirExpr, pattern: SirExpr) error{LowerError}!void {
    _ = f;
    const expr_data = c.sir_expr_data.get(pattern);
    switch (expr_data) {
        .name => |name| {
            c.scope.push(.{
                .name = name,
                .value = .{ .expr = input },
            });
        },
        //.object => |object| {
        //    TODO assert input is a struct?
        //    try lowerObjectPattern(c, f, input, object);
        //}
        else => return fail(c, pattern, .invalid_pattern),
    }
}

fn lowerExpr(c: *Compiler, f: *DirFunData, expr: SirExpr) error{LowerError}!DirExpr {
    _ = f;
    const expr_data = c.sir_expr_data.get(expr);
    switch (expr_data) {
        //.i32 => |i| {
        //    return f.expr_data.append(.{ .value = .{ .i32 = i } });
        //},
        //.object => |object| {
        //    return f.expr_data.append(try lowerObject(c, f, object));
        //},
        //.name => |name| {
        //    const binding = c.scope.lookup(name) orelse
        //        return fail(c, expr, "Not defined: {s}", .{name});
        //    switch (binding.value) {
        //        .dir => |dir| return dir,
        //        .function, .intrinsic, .builtin => return fail(c, expr, "You may not use a function here", .{}),
        //    }
        //},
        //.intrinsic => {
        //    return fail(c, expr, "Intrinsics may only be called", .{});
        //},
        //.builtin => |builtin| {
        //    switch (builtin) {
        //        .i32 => return f.expr_data.append(.{ .value = .{ .repr = .i32 } }),
        //        .@"get-repr-data" => return fail(c, expr, "Builtins may only be called", .{}),
        //    }
        //},
        //.let => |let| {
        //    const value = try lowerExprOrFn(c, f, let.value);
        //    if (let.mut) return fail(c, expr, "TODO", .{});
        //    c.scope.push(.{
        //        .name = let.name,
        //        .value = value,
        //    });
        //    return f.expr_data.append(.{ .value = Value.emptyStruct() });
        //},
        //.@"fn" => return fail(c, expr, "You may not create a function here", .{}),
        //.call => |call| {
        //    const head = try lowerExprOrFn(c, f, call.head);
        //    const args = try lowerObject(c, f, call.args);
        //    switch (head) {
        //        .dir => return fail(c, expr, "Cannot call {}", .{head}),
        //        .function => |function| {
        //            const arg = f.expr_data.append(args);
        //            return f.expr_data.append(.{ .call = .{
        //                .function = function,
        //                .specialization = null,
        //                .args = c.dupeOne(arg),
        //            } });
        //        },
        //        .intrinsic => |intrinsic| {
        //            switch (intrinsic) {
        //                .@"i32-add" => {
        //                    if (!deepEqual(@as([]const Value, &.{ .{ .i32 = 0 }, .{ .i32 = 1 } }), args.struct_init.keys))
        //                        return fail(c, expr, "Invalid call to intrinsic", .{});
        //                    return f.expr_data.append(.{ .add = .{
        //                        args.struct_init.values[0],
        //                        args.struct_init.values[1],
        //                    } });
        //                },
        //                // TODO for store/load/copy, might be worth adding an assert that the address is >stack-top
        //                .@"i32-store" => {
        //                    try matchKeys(c, expr, args.struct_init.keys, .{ 0, "to" });
        //                    return f.expr_data.append(.{ .store = .{
        //                        .value = args.struct_init.values[0],
        //                        .to = args.struct_init.values[1],
        //                    } });
        //                },
        //                .@"i32-load" => {
        //                    try matchKeys(c, expr, args.struct_init.keys, .{0});
        //                    return f.expr_data.append(.{ .load = .{
        //                        .from = args.struct_init.values[0],
        //                        .repr = .i32,
        //                    } });
        //                },
        //                .@"memory-copy" => {
        //                    try matchKeys(c, expr, args.struct_init.keys, .{ "from", "to", "byte-count" });
        //                    return f.expr_data.append(.{ .copy = .{
        //                        .from = args.struct_init.values[0],
        //                        .to = args.struct_init.values[1],
        //                        .byte_count = args.struct_init.values[2],
        //                    } });
        //                },
        //                .@"stack-top" => {
        //                    try matchKeys(c, expr, args.struct_init.keys, .{});
        //                    return f.expr_data.append(.stack_top);
        //                },
        //            }
        //        },
        //        .builtin => |builtin| {
        //            switch (builtin) {
        //                .i32 => return fail(c, expr, "Cannot call {}", .{head}),
        //                .@"get-repr-data" => {
        //                    try matchKeys(c, expr, args.struct_init.keys, .{0});
        //                    return f.expr_data.append(.{ .get_repr_data = args.struct_init.values[0] });
        //                },
        //            }
        //        },
        //    }
        //},
        //.get => |get| {
        //    const object = try lowerExpr(c, f, get.object);
        //    const key = try evalKey(c, f, get.key);
        //    return f.expr_data.append(.{ .get = .{ .object = object, .key = key } });
        //},
        //.statements => |statements| {
        //    const scope_saved = c.scope.save();
        //    defer c.scope.restore(scope_saved);

        //    if (statements.len == 0) {
        //        return f.expr_data.append(.{ .value = Value.emptyStruct() });
        //    } else {
        //        var dir: ?Dir = null;
        //        for (statements) |statement| {
        //            dir = try lowerExpr(c, f, statement);
        //        }
        //        return dir.?;
        //    }
        //},
        else => return fail(c, expr, .todo),
    }
}

//fn matchKeys(c: *Compiler, expr: Expr, actual_keys: []const Value, expected_keys: anytype) error{LowerError}!void {
//    if (actual_keys.len != expected_keys.len)
//        return fail(c, expr, "Expected {} args, found {} args", .{ expected_keys.len, actual_keys.len });
//    inline for (expected_keys, 0..) |expected_key, i| {
//        const actual_key = actual_keys[i];
//        switch (@TypeOf(expected_key)) {
//            comptime_int => {
//                if (actual_key != .i32 or
//                    actual_key.i32 != @as(i32, expected_key))
//                    return fail(c, expr, "Expected key {}, found key {}", .{ expected_key, actual_key });
//            },
//            else => {
//                if (actual_key != .string or
//                    !std.mem.eql(u8, actual_key.string, expected_key))
//                    return fail(c, expr, "Expected key '{s}', found key {}", .{ expected_key, actual_key });
//            },
//        }
//    }
//}

//fn lowerObject(c: *Compiler, f: *DirFunData, object: SirObject) error{LowerError}!DirData {
//    const keys = c.allocator.alloc(Value, object.keys.len) catch oom();
//    for (keys, object.keys) |*key_dir, key| key_dir.* = try evalKey(c, f, key);

//    const values = c.allocator.alloc(Dir, object.values.len) catch oom();
//    for (values, object.values) |*value_dir, value| value_dir.* = try lowerExpr(c, f, value);

//    // TODO sort keys and values
//    return DirData{ .struct_init = .{ .keys = keys, .values = values } };
//}

//fn lowerExprOrFn(c: *Compiler, f: *DirFunData, expr: Expr) error{LowerError}!AbstractValue {
//    const expr_data = c.expr_data.get(expr);
//    switch (expr_data) {
//        .name => |name| {
//            const binding = c.scope.lookup(name) orelse
//                return fail(c, expr, "Not defined: {s}", .{name});
//            return binding.value;
//        },
//        .intrinsic => |intrinsic| {
//            return .{ .intrinsic = intrinsic };
//        },
//        .builtin => |builtin| {
//            return .{ .builtin = builtin };
//        },
//        .@"fn" => |@"fn"| {
//            return .{ .function = try lowerFun(c, @"fn".params, @"fn".body) };
//        },
//        else => {
//            return .{ .dir = try lowerExpr(c, f, expr) };
//        },
//    }
//}

//fn evalExpr(c: *Compiler, f: *DirFunData, expr: Expr) error{LowerError}!Value {
//    _ = f;
//    const expr_data = c.expr_data.get(expr);
//    switch (expr_data) {
//        .i32 => |i| return .{ .i32 = i },
//        .string => |string| return .{ .string = string },
//        else => return fail(c, expr, "Can't const-eval", .{}),
//    }
//}

fn fail(c: *Compiler, expr: SirExpr, data: LowerErrorData) error{LowerError} {
    c.error_data = .{ .lower = .{ .expr = expr, .data = data } };
    return error.LowerError;
}

pub const LowerErrorData = union(enum) {
    invalid_pattern,
    todo,
};
