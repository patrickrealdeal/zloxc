const std = @import("std");
const ast = @import("ast.zig");
const Node = ast.Node;
const Type = @import("type.zig").Type;
const TokenType = @import("token.zig").TokenType;

pub const TypeChecker = struct {
    allocator: std.mem.Allocator,
    symbols: std.StringHashMap(Symbol),
    errors: std.ArrayList(TypeError),
    current_function_return_type: ?Type,
    allocated_keys: std.ArrayList([]const u8), // Track allocated keys for cleanup
    scopes: std.ArrayList(std.StringHashMap(Symbol)), // Stack of scopes for nested functions

    const Symbol = struct {
        type_info: Type,
        is_mutable: bool,
    };

    pub fn init(allocator: std.mem.Allocator) TypeChecker {
        return .{
            .allocator = allocator,
            .symbols = std.StringHashMap(Symbol).init(allocator),
            .errors = .empty,
            .current_function_return_type = null,
            .allocated_keys = .empty,
            .scopes = .empty,
        };
    }

    pub fn deinit(self: *TypeChecker) void {
        // Free all allocated key strings
        for (self.allocated_keys.items) |key| {
            self.allocator.free(key);
        }
        self.allocated_keys.deinit(self.allocator);

        // Clean up any remaining scopes
        for (self.scopes.items) |*scope| {
            scope.deinit();
        }
        self.scopes.deinit(self.allocator);

        self.symbols.deinit();
        self.errors.deinit(self.allocator);
    }

    /// Reset the type checker for a fresh compilation (used when running files)
    pub fn reset(self: *TypeChecker) void {
        // Free all allocated key strings
        for (self.allocated_keys.items) |key| {
            self.allocator.free(key);
        }
        self.allocated_keys.clearRetainingCapacity();

        // Clear any nested scopes
        for (self.scopes.items) |*scope| {
            scope.deinit();
        }
        self.scopes.clearRetainingCapacity();

        // Clear the symbol table
        self.symbols.clearRetainingCapacity();

        // Clear errors
        self.errors.clearRetainingCapacity();

        // Reset function return type tracking
        self.current_function_return_type = null;
    }

    /// Register a native function with the type checker
    /// This should be called for each native function during VM initialization
    pub fn registerNative(self: *TypeChecker, name: []const u8, param_types: []const Type, return_type: Type) !void {
        // Allocate memory for the function type
        const params = try self.allocator.alloc(Type, param_types.len);
        for (param_types, 0..) |param_type, i| {
            params[i] = param_type;
        }

        const ret_type_ptr = try self.allocator.create(Type);
        ret_type_ptr.* = return_type;

        const func_type = Type{
            .function = .{
                .params = params,
                .return_type = ret_type_ptr,
            },
        };

        // Register the function as an immutable symbol
        try self.putSymbol(name, .{
            .type_info = func_type,
            .is_mutable = false,
        });
    }

    /// Store a symbol with a duplicated key string (for persistence across compilations)
    fn putSymbol(self: *TypeChecker, name: []const u8, symbol: Symbol) !void {
        // Check if this key already exists
        if (self.symbols.contains(name)) {
            // Key already exists, just update the value
            try self.symbols.put(name, symbol);
        } else {
            // New key - duplicate it for persistence
            const key_copy = try self.allocator.dupe(u8, name);
            try self.allocated_keys.append(self.allocator, key_copy);
            try self.symbols.put(key_copy, symbol);
        }
    }

    /// Look up a symbol in the current scope and all parent scopes
    fn lookupSymbol(self: *TypeChecker, name: []const u8) ?Symbol {
        // First check current scope
        if (self.symbols.get(name)) |symbol| {
            return symbol;
        }

        // Then check parent scopes (from most recent to oldest)
        var i: usize = self.scopes.items.len;
        while (i > 0) {
            i -= 1;
            if (self.scopes.items[i].get(name)) |symbol| {
                return symbol;
            }
        }

        return null;
    }

    /// Push a new scope (for entering function bodies)
    fn pushScope(self: *TypeChecker) !void {
        // Save current symbol table as a parent scope
        try self.scopes.append(self.allocator, self.symbols);
        // Create new empty scope
        self.symbols = std.StringHashMap(Symbol).init(self.allocator);
    }

    /// Pop a scope (for exiting function bodies)
    fn popScope(self: *TypeChecker) void {
        // Clean up current scope
        self.symbols.deinit();
        // Restore parent scope (pop returns the last item)
        if (self.scopes.pop()) |parent_scope| {
            self.symbols = parent_scope;
        } else {
            // This shouldn't happen, but create an empty scope as fallback
            self.symbols = std.StringHashMap(Symbol).init(self.allocator);
        }
    }

    /// Main entry point - check an entire program
    pub fn checkProgram(self: *TypeChecker, nodes: []*Node) !bool {
        // Clear previous errors but keep symbol table for REPL persistence
        self.errors.clearRetainingCapacity();

        for (nodes) |node| {
            _ = try self.checkNode(node);
        }

        return self.errors.items.len == 0;
    }

    /// Check a single node and return its type
    fn checkNode(self: *TypeChecker, node: *Node) !Type {
        return switch (node.*) {
            .literal => |val| self.inferLiteralType(val),
            .number_literal => |num_lit| {
                // If the literal contains a decimal point, it should be typed as float
                // even if the value is a whole number (e.g., 0.0, 1.0, 2.0)
                const has_decimal = std.mem.indexOfScalar(u8, num_lit.lexeme, '.') != null;
                if (has_decimal) {
                    return Type.float;
                }
                return self.inferLiteralType(num_lit.value);
            },

            .binary => |*bin| {
                const left_type = try self.checkNode(bin.left);
                const right_type = try self.checkNode(bin.right);

                const result_type = try self.checkBinaryOp(bin.op, left_type, right_type);
                bin.result_type = result_type;
                return result_type;
            },

            .unary => |un| {
                const operand_type = try self.checkNode(un.right);
                return try self.checkUnaryOp(un.op, operand_type);
            },

            .var_declaration => |*decl| {
                var var_type: Type = Type.unknown;

                // Check initializer if present
                if (decl.initializer) |initt| {
                    var_type = try self.checkNode(initt);
                }

                // Handle type annotation
                if (decl.type_annotation) |annot| {
                    if (annot.type_kind == .inferred) {
                        // Type inference
                        if (decl.initializer == null) {
                            try self.addError("Cannot infer type without initializer", .{});
                            var_type = Type.unknown;
                        }
                        // else: use the inferred type from initializer
                    } else {
                        // Explicit type - check compatibility
                        if (decl.initializer != null and !annot.type_kind.isAssignableFrom(var_type)) {
                            const var_type_str = try var_type.toString(self.allocator);
                            defer self.allocator.free(var_type_str);
                            const annot_type_str = try annot.type_kind.toString(self.allocator);
                            defer self.allocator.free(annot_type_str);
                            try self.addError("Type mismatch for variable '{s}': cannot assign {s} to {s}", .{ decl.name, var_type_str, annot_type_str });
                        }
                        var_type = annot.type_kind;
                    }

                    // Store in symbol table (with duplicated key for persistence)
                    try self.putSymbol(decl.name, .{
                        .type_info = var_type,
                        .is_mutable = annot.is_mutable,
                    });
                } else {
                    // No type annotation - this should not happen after parser changes
                    // but adding error for safety
                    try self.addError("Variable '{s}' must have type annotation (use ':=' for inference or ': type' for explicit type)", .{decl.name});
                    // Store with unknown type for error recovery
                    try self.putSymbol(decl.name, .{
                        .type_info = Type.unknown,
                        .is_mutable = true,
                    });
                }

                decl.inferred_type = var_type;
                return Type.nil;
            },

            .var_ref => |var_ref| {
                const symbol = self.lookupSymbol(var_ref.name) orelse {
                    try self.addError("Undefined variable '{s}'", .{var_ref.name});
                    return Type.unknown;
                };
                return symbol.type_info;
            },

            .assignment => |assign| {
                const value_type = try self.checkNode(assign.value);

                const symbol = self.lookupSymbol(assign.target) orelse {
                    try self.addErrorWithLoc(
                        "undefined variable '{s}'",
                        .{assign.target},
                        assign.loc,
                        assign.target,
                    );
                    return Type.unknown;
                };

                // Check mutability with enhanced error
                if (!symbol.is_mutable) {
                    try self.addErrorWithLoc(
                        "cannot assign to immutable variable '{s}'",
                        .{assign.target},
                        assign.loc,
                        assign.target,
                    );
                }

                // Check type compatibility with enhanced error
                if (!symbol.type_info.isAssignableFrom(value_type)) {
                    const value_type_str = try value_type.toString(self.allocator);
                    defer self.allocator.free(value_type_str);
                    const symbol_type_str = try symbol.type_info.toString(self.allocator);
                    defer self.allocator.free(symbol_type_str);

                    const msg = try std.fmt.allocPrint(
                        self.allocator,
                        "type mismatch: expected '{s}', found '{s}'",
                        .{ symbol_type_str, value_type_str },
                    );
                    defer self.allocator.free(msg);

                    try self.addErrorWithLoc(
                        "{s}",
                        .{msg},
                        assign.loc,
                        assign.target,
                    );
                }

                return value_type;
            },

            .function_declaration => |func_decl| {
                // Create function type
                var param_types = try self.allocator.alloc(Type, func_decl.params.len);
                for (func_decl.params, 0..) |param, i| {
                    param_types[i] = param.param_type;
                }

                const return_type_ptr = try self.allocator.create(Type);
                return_type_ptr.* = func_decl.return_type;

                const func_type = Type{
                    .function = .{
                        .params = param_types,
                        .return_type = return_type_ptr,
                    },
                };

                // Store function in current scope before entering function body
                // This allows the function to be called and enables recursion
                try self.putSymbol(func_decl.name, .{
                    .type_info = func_type,
                    .is_mutable = false, // Functions are immutable
                });

                // Save current function context
                const prev_return_type = self.current_function_return_type;
                self.current_function_return_type = func_decl.return_type;

                // Push a new scope for the function body
                try self.pushScope();
                defer self.popScope();

                // Add parameters to function's local scope
                for (func_decl.params) |param| {
                    try self.putSymbol(param.name, .{
                        .type_info = param.param_type,
                        .is_mutable = true, // Parameters are mutable by default
                    });
                }

                // Check function body
                _ = try self.checkNode(func_decl.body);

                // Restore context
                self.current_function_return_type = prev_return_type;

                return func_type;
            },

            .call => |call_expr| {
                // Special handling for built-in typeOf function
                if (call_expr.callee.* == .var_ref) {
                    const callee_name = call_expr.callee.var_ref.name;
                    if (std.mem.eql(u8, callee_name, "typeOf")) {
                        // typeOf takes exactly one argument of any type and returns string
                        if (call_expr.arguments.len != 1) {
                            try self.addError("typeOf() expects exactly 1 argument, got {}", .{call_expr.arguments.len});
                        } else {
                            // Type check the argument (but accept any type)
                            _ = try self.checkNode(call_expr.arguments[0]);
                        }
                        return Type.string;
                    }
                }

                const callee_type = try self.checkNode(call_expr.callee);

                if (callee_type != .function) {
                    const callee_type_str = try callee_type.toString(self.allocator);
                    defer self.allocator.free(callee_type_str);
                    try self.addError("Cannot call non-function type {s}", .{callee_type_str});
                    return Type.unknown;
                }

                const func_type = callee_type.function;

                // Check argument count
                if (call_expr.arguments.len != func_type.params.len) {
                    try self.addError("Expected {} arguments but got {}", .{ func_type.params.len, call_expr.arguments.len });
                    return func_type.return_type.*;
                }

                // Check argument types
                for (call_expr.arguments, func_type.params) |arg, expected_type| {
                    const arg_type = try self.checkNode(arg);
                    if (!expected_type.isAssignableFrom(arg_type)) {
                        const expected_str = try expected_type.toString(self.allocator);
                        defer self.allocator.free(expected_str);
                        const arg_str = try arg_type.toString(self.allocator);
                        defer self.allocator.free(arg_str);
                        try self.addError("Argument type mismatch: expected {s}, got {s}", .{ expected_str, arg_str });
                    }
                }

                return func_type.return_type.*;
            },

            .ret_statement => |ret| {
                const return_type = if (ret.value) |val|
                    try self.checkNode(val)
                else
                    Type.nil;

                if (self.current_function_return_type) |expected| {
                    if (!expected.isAssignableFrom(return_type)) {
                        const expected_str = try expected.toString(self.allocator);
                        defer self.allocator.free(expected_str);
                        const return_str = try return_type.toString(self.allocator);
                        defer self.allocator.free(return_str);
                        try self.addError("Return type mismatch: expected {s}, got {s}", .{ expected_str, return_str });
                    }
                }

                return Type.nil;
            },

            .block => |blk| {
                // Push a new scope for the block
                try self.pushScope();
                defer self.popScope();

                for (blk.statements) |stmt| {
                    _ = try self.checkNode(stmt);
                }
                return Type.nil;
            },

            .if_statement => |if_stmt| {
                const cond_type = try self.checkNode(if_stmt.condition);
                if (cond_type != .bool and cond_type != .unknown) {
                    const cond_str = try cond_type.toString(self.allocator);
                    defer self.allocator.free(cond_str);
                    try self.addError("If condition must be bool, got {s}", .{cond_str});
                }

                _ = try self.checkNode(if_stmt.then_branch);
                if (if_stmt.else_branch) |else_br| {
                    _ = try self.checkNode(else_br);
                }

                return Type.nil;
            },

            .while_statement => |while_stmt| {
                const cond_type = try self.checkNode(while_stmt.condition);
                if (cond_type != .bool and cond_type != .unknown) {
                    const cond_str = try cond_type.toString(self.allocator);
                    defer self.allocator.free(cond_str);
                    try self.addError("While condition must be bool, got {s}", .{cond_str});
                }

                _ = try self.checkNode(while_stmt.body);
                return Type.nil;
            },

            .for_statement => |for_stmt| {
                if (for_stmt.initializer) |initt| {
                    _ = try self.checkNode(initt);
                }

                if (for_stmt.condition) |cond| {
                    const cond_type = try self.checkNode(cond);
                    if (cond_type != .bool and cond_type != .unknown) {
                        const cond_str = try cond_type.toString(self.allocator);
                        defer self.allocator.free(cond_str);
                        try self.addError("For condition must be bool, got {s}", .{cond_str});
                    }
                }

                if (for_stmt.increment) |incr| {
                    _ = try self.checkNode(incr);
                }

                _ = try self.checkNode(for_stmt.body);
                return Type.nil;
            },

            .print_statement, .expression_statement => |expr| {
                _ = try self.checkNode(expr);
                return Type.nil;
            },

            else => Type.unknown,
        };
    }

    fn inferLiteralType(self: *TypeChecker, val: @import("value.zig").Value) Type {
        _ = self;
        if (val.isNumber()) {
            const num = val.asNumber();
            // Check if it's a whole number (integer)
            // Note: We use minInt(i32) - 1 for the lower bound and maxInt(i32) + 1 for upper bound
            // to handle edge cases like -2147483648 which is parsed as unary minus applied to 2147483648
            if (@floor(num) == num and num >= (@as(f64, std.math.minInt(i32)) - 1) and num <= (@as(f64, std.math.maxInt(i32)) + 1)) {
                return Type.int;
            }
            return Type.float;
        } else if (val.isBool()) {
            return Type.bool;
        } else if (val.isNil()) {
            return Type.nil;
        } else if (val.isObj()) {
            const obj = val.asObj();
            if (obj.obj_t == .string) {
                return Type.string;
            }
        }
        return Type.unknown;
    }

    fn checkBinaryOp(self: *TypeChecker, op: TokenType, left: Type, right: Type) !Type {
        return switch (op) {
            .plus => blk: {
                if ((left == .int or left == .float) and (right == .int or right == .float)) {
                    break :blk if (left == .float or right == .float) Type.float else Type.int;
                } else if (left == .string and right == .string) {
                    break :blk Type.string;
                } else {
                    const left_str = try left.toString(self.allocator);
                    defer self.allocator.free(left_str);
                    const right_str = try right.toString(self.allocator);
                    defer self.allocator.free(right_str);
                    try self.addError("Invalid operands for '+': {s} and {s}", .{ left_str, right_str });
                    break :blk Type.unknown;
                }
            },
            .minus, .star, .slash => blk: {
                if ((left == .int or left == .float) and (right == .int or right == .float)) {
                    break :blk if (left == .float or right == .float) Type.float else Type.int;
                } else {
                    try self.addError("Arithmetic requires numeric operands", .{});
                    break :blk Type.unknown;
                }
            },
            .less, .less_equal, .greater, .greater_equal => blk: {
                if ((left == .int or left == .float) and (right == .int or right == .float)) {
                    break :blk Type.bool;
                } else {
                    try self.addError("Comparison requires numeric operands", .{});
                    break :blk Type.bool;
                }
            },
            .equal_equal, .bang_equal => Type.bool,
            else => Type.unknown,
        };
    }

    fn checkUnaryOp(self: *TypeChecker, op: TokenType, operand: Type) !Type {
        return switch (op) {
            .minus => blk: {
                if (operand == .int or operand == .float) {
                    break :blk operand;
                } else {
                    try self.addError("Unary '-' requires numeric operand", .{});
                    break :blk Type.unknown;
                }
            },
            .bang => Type.bool,
            else => Type.unknown,
        };
    }

    fn addError(self: *TypeChecker, comptime fmt: []const u8, args: anytype) !void {
        const msg = try std.fmt.allocPrint(self.allocator, fmt, args);
        try self.errors.append(self.allocator, .{ .message = msg });
        std.debug.print("TYPE ERROR: {s}\n", .{msg});
    }

    fn addErrorWithLoc(
        self: *TypeChecker,
        comptime fmt: []const u8,
        args: anytype,
        loc: ast.SourceLoc,
        variable_name: ?[]const u8,
    ) !void {
        const msg = try std.fmt.allocPrint(self.allocator, fmt, args);
        try self.errors.append(self.allocator, .{
            .message = msg,
            .line = loc.line,
            .column = loc.column,
            .length = loc.length,
            .variable_name = variable_name,
        });

        // Pretty print the error immediately
        try self.printPrettyError(self.errors.items[self.errors.items.len - 1]);
    }

    fn printPrettyError(self: *TypeChecker, err: TypeError) !void {
        _ = self;
        var stderr = std.fs.File.stderr();
        var writer_buf = stderr.writer(&.{});
        var writer = &writer_buf.interface;

        // Print error header with color
        try writer.print("\x1b[1;31merror:\x1b[0m {s}\n", .{err.message});

        if (err.line) |line| {
            // Print location
            try writer.print("  \x1b[1;36m-->\x1b[0m line {d}\n", .{line});

            // If we have the variable name, show it with carets
            if (err.variable_name) |var_name| {
                try writer.print("   \x1b[1;34m|\x1b[0m\n", .{});
                try writer.print(" \x1b[1;34m{d} |\x1b[0m ", .{line});
                try writer.print("{s}\n", .{var_name});
                try writer.print("   \x1b[1;34m|\x1b[0m ", .{});

                // Print carets
                for (0..var_name.len) |_| {
                    try writer.writeAll("\x1b[1;31m^\x1b[0m");
                }
                try writer.writeAll("\n");
            }
        }

        try writer.writeAll("\n");
    }
};

pub const TypeError = struct {
    message: []const u8,
    line: ?usize = null,
    column: ?usize = null,
    length: ?usize = null,
    source_line: ?[]const u8 = null,
    variable_name: ?[]const u8 = null,
};

