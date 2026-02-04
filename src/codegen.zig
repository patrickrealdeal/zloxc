const std = @import("std");
const Node = @import("ast.zig").Node;
const Compiler = @import("compiler.zig").Compiler;
const OpCode = @import("chunk.zig").OpCode;
const Chunk = @import("chunk.zig");
const Value = @import("value.zig").Value;
const Obj = @import("object.zig");
const VM = @import("vm.zig");
const debug = @import("debug.zig");
const TokenType = @import("token.zig").TokenType;

compiler: *Compiler,
vm: *VM,
allocator: std.mem.Allocator,

const Codegen = @This();

pub fn init(compiler: *Compiler, vm: *VM) Codegen {
    return .{
        .compiler = compiler,
        .vm = vm,
        .allocator = compiler.allocator,
    };
}

/// Main entry point for code generation - traverses AST and emits bytecode
pub fn emitFromAst(self: *Codegen, node: *Node) !void {
    switch (node.*) {
        .literal => |val| {
            try self.emitConstant(val);
        },

        .binary => |bin| {
            try self.emitFromAst(bin.left);
            try self.emitFromAst(bin.right);
            switch (bin.op) {
                .plus => self.emitByte(@intFromEnum(OpCode.add)),
                .minus => self.emitByte(@intFromEnum(OpCode.sub)),
                .star => self.emitByte(@intFromEnum(OpCode.mul)),
                .slash => self.emitByte(@intFromEnum(OpCode.div)),
                .bang_equal => self.emitBytes(@intFromEnum(OpCode.equal), @intFromEnum(OpCode.not)),
                .equal_equal => self.emitByte(@intFromEnum(OpCode.equal)),
                .greater => self.emitByte(@intFromEnum(OpCode.greater)),
                .greater_equal => self.emitBytes(@intFromEnum(OpCode.less), @intFromEnum(OpCode.not)),
                .less => self.emitByte(@intFromEnum(OpCode.less)),
                .less_equal => self.emitBytes(@intFromEnum(OpCode.greater), @intFromEnum(OpCode.not)),
                else => unreachable,
            }
        },

        .logical => |logical| {
            try self.emitFromAst(logical.left);
            if (logical.op == .@"and") {
                const end_jump = self.emitJump(@intFromEnum(OpCode.jump_if_false));
                self.emitByte(@intFromEnum(OpCode.pop));
                try self.emitFromAst(logical.right);
                self.patchJump(end_jump);
            } else {
                const else_jump = self.emitJump(@intFromEnum(OpCode.jump_if_false));
                const end_jump = self.emitJump(@intFromEnum(OpCode.jump));
                self.patchJump(else_jump);
                self.emitByte(@intFromEnum(OpCode.pop));
                try self.emitFromAst(logical.right);
                self.patchJump(end_jump);
            }
        },

        .unary => |un| {
            try self.emitFromAst(un.right);
            switch (un.op) {
                .minus => self.emitByte(@intFromEnum(OpCode.negate)),
                .bang => self.emitByte(@intFromEnum(OpCode.not)),
                else => unreachable,
            }
        },

        .var_declaration => |decl| {
            std.debug.print("DEBUG: Declaring var '{s}' at scope depth {}\n", .{ decl.name, decl.scope_depth_at_declaration });

            // Emit initializer or nil
            if (decl.initializer) |initt| {
                try self.emitFromAst(initt);
            } else {
                self.emitByte(@intFromEnum(OpCode.nil));
            }

            // Use scope depth stored in AST node to determine global vs local
            if (decl.scope_depth_at_declaration == 0) {
                // Global variable
                const global = try self.makeStringConstant(decl.name);
                self.emitBytes(@intFromEnum(OpCode.define_global), global);
            } else {
                // Local variable - ADD IT FIRST, THEN mark initialized
                try self.addLocal(decl.name); // ADD THIS LINE
                self.markInitialized();
            }
        },

        .var_ref => |var_ref| {
            try self.emitVariableGet(var_ref.name);
        },

        .assignment => |assign| {
            try self.emitFromAst(assign.value);
            try self.emitVariableSet(assign.target);
        },

        .block => |blk| {
            self.beginScope();
            for (blk.statements) |stmt| {
                try self.emitFromAst(stmt);
                if (stmt.* == .ret_statement) break;
            }
            self.endScope();
        },

        .if_statement => |if_stmt| {
            try self.emitFromAst(if_stmt.condition);
            const then_jump = self.emitJump(@intFromEnum(OpCode.jump_if_false));
            self.emitByte(@intFromEnum(OpCode.pop));
            try self.emitFromAst(if_stmt.then_branch);

            const else_jump = self.emitJump(@intFromEnum(OpCode.jump));
            self.patchJump(then_jump);
            self.emitByte(@intFromEnum(OpCode.pop));

            if (if_stmt.else_branch) |else_br| {
                try self.emitFromAst(else_br);
            }
            self.patchJump(else_jump);
        },

        .for_statement => |for_stmt| {
            //self.beginScope();

            // Emit initializer
            if (for_stmt.initializer) |initt| {
                try self.emitFromAst(initt);
            }

            const loop_start = self.currentChunk().code.items.len;

            // Emit condition (or assume true)
            var exit_jump: ?usize = null;
            if (for_stmt.condition) |cond| {
                try self.emitFromAst(cond);
                exit_jump = self.emitJump(@intFromEnum(OpCode.jump_if_false));
                self.emitByte(@intFromEnum(OpCode.pop));
            }

            // Jump over increment to body
            var body_jump: ?usize = null;
            if (for_stmt.increment != null) {
                body_jump = self.emitJump(@intFromEnum(OpCode.jump));
            }

            // Emit increment
            const increment_start = self.currentChunk().code.items.len;
            if (for_stmt.increment) |incr| {
                try self.emitFromAst(incr);
                self.emitByte(@intFromEnum(OpCode.pop));
            }

            // Loop back to condition
            self.emitLoop(loop_start);

            // Patch jump to body
            if (body_jump) |jump| {
                self.patchJump(jump);
            }

            // Emit body
            try self.emitFromAst(for_stmt.body);

            // Loop back to increment (or condition if no increment)
            if (for_stmt.increment != null) {
                self.emitLoop(increment_start);
            } else {
                self.emitLoop(loop_start);
            }

            // Patch exit jump
            if (exit_jump) |jump| {
                self.patchJump(jump);
                self.emitByte(@intFromEnum(OpCode.pop));
            }

            //self.endScope();
        },

        .while_statement => |while_stmt| {
            const loop_start = self.currentChunk().code.items.len;

            // Emit condition
            try self.emitFromAst(while_stmt.condition);

            // Exit if false
            const exit_jump = self.emitJump(@intFromEnum(OpCode.jump_if_false));
            self.emitByte(@intFromEnum(OpCode.pop));

            // Emit body
            try self.emitFromAst(while_stmt.body);

            // Loop back
            self.emitLoop(loop_start);

            self.patchJump(exit_jump);
            self.emitByte(@intFromEnum(OpCode.pop));
        },

        .function_declaration => |func_decl| {
            // Save current compiler
            const enclosing_compiler = self.compiler;

            // Create new compiler for this function
            var func_compiler = try Compiler.init(self.vm, .function, self.compiler);
            defer func_compiler.deinit();
            self.compiler = &func_compiler;

            self.compiler.func.name = try Obj.String.copy(self.vm, func_decl.name);
            self.compiler.func.arity = func_decl.arity;

            // Begin function scope
            self.beginScope();

            // Add parameters as locals
            for (func_decl.params) |param| {
                try self.addLocal(param);
                self.markInitialized();
            }

            // Emit function body
            switch (func_decl.body.*) {
                .block => |body_block| {
                    for (body_block.statements) |stmt| {
                        try self.emitFromAst(stmt);
                    }
                },
                else => return error.Undefined,
            }

            self.endScope();

            // End function compilation
            const func = self.endCompilerInternal();

            // Restore enclosing compiler
            self.compiler = enclosing_compiler;

            // Emit closure instruction with upvalues
            const func_constant = try self.makeConstant(Value.fromObj(&func.obj));
            self.emitBytes(@intFromEnum(OpCode.closure), func_constant);

            // Emit upvalue information
            for (func_compiler.upvalues.items) |upvalue| {
                self.emitByte(if (upvalue.source == .local) 1 else 0);
                self.emitByte(upvalue.index);
            }

            // Use scope depth stored in AST to determine global vs local
            if (func_decl.scope_depth_at_declaration == 0) {
                const global = try self.makeStringConstant(func_decl.name);
                self.emitBytes(@intFromEnum(OpCode.define_global), global);
            } else {
                // Local function - mark initialized
                try self.addLocal(func_decl.name);
                self.markInitialized();
            }
        },

        .call => |call_expr| {
            // Emit the callee
            try self.emitFromAst(call_expr.callee);

            // Emit all arguments
            for (call_expr.arguments) |arg| {
                try self.emitFromAst(arg);
            }

            // Emit call instruction with argument count
            self.emitBytes(@intFromEnum(OpCode.call), @intCast(call_expr.arguments.len));
        },

        .class_declaration => |class_decl| {
            // Declare variable (Scope check)
            if (class_decl.scope_depth_at_declaration > 0) {
                // Local scope - add to locals
                try self.addLocal(class_decl.name);
                self.markInitialized();
            }

            // Create string constant for class name
            const name_idx = try self.makeStringConstant(class_decl.name);

            // Emit OpCode
            self.emitBytes(@intCast(@intFromEnum(OpCode.class)), @intCast(name_idx));

            // Define variable (Global scope logic)
            if (class_decl.scope_depth_at_declaration == 0) {
                self.emitBytes(@intCast(@intFromEnum(OpCode.define_global)), @intCast(name_idx));
            }
        },

        .print_statement => |value| {
            try self.emitFromAst(value);
            self.emitByte(@intFromEnum(OpCode.print));
        },

        .expression_statement => |expr| {
            try self.emitFromAst(expr);
            self.emitByte(@intFromEnum(OpCode.pop));
        },

        .ret_statement => |ret| {
            if (ret.value) |val| {
                try self.emitFromAst(val);
                self.emitByte(@intFromEnum(OpCode.ret));
            } else {
                self.emitReturn();
            }
        },
    }
}

/// Completes compilation and returns the compiled function
pub fn endCompilation(self: *Codegen) *Obj.Function {
    self.emitReturn();
    const func = self.compiler.func;

    if (comptime debug.print_code) {
        Chunk.disassemble(&func.chunk, if (func.name) |name| name.bytes else "<script>");
        std.debug.print("-----------------\n", .{});
    }

    return func;
}

// ============================================================================
// Helper methods for bytecode emission
// ============================================================================

fn emitByte(self: *Codegen, byte: u8) void {
    self.currentChunk().write(byte, 0) catch |e| {
        std.log.err("Error {any} trying to emit byte\n", .{e});
        std.process.exit(1);
    };
}

fn emitBytes(self: *Codegen, byte1: u8, byte2: u8) void {
    self.emitByte(byte1);
    self.emitByte(byte2);
}

fn emitReturn(self: *Codegen) void {
    self.emitByte(@intFromEnum(OpCode.nil));
    self.emitByte(@intFromEnum(OpCode.ret));
}

fn emitConstant(self: *Codegen, value: Value) !void {
    self.emitBytes(@intFromEnum(OpCode.constant), try self.makeConstant(value));
}

fn makeConstant(self: *Codegen, value: Value) !u8 {
    self.vm.push(value);
    const constant = self.currentChunk().addConstant(value) catch {
        std.log.err("Error adding constant\n", .{});
        return error.CompilerError;
    };
    _ = self.vm.pop();

    if (constant > std.math.maxInt(u8)) {
        std.log.err("Too many constants in a chunk\n", .{});
        return error.CompilerError;
    }
    return @intCast(constant);
}

fn makeStringConstant(self: *Codegen, name: []const u8) !u8 {
    const str = try Obj.String.copy(self.vm, name);
    return try self.makeConstant(Value.fromObj(&str.obj));
}

fn emitJump(self: *Codegen, byte: u8) usize {
    self.emitByte(byte);
    // 16-bit placeholder operand to calculate the jump
    self.emitBytes(0xff, 0xff);
    return self.currentChunk().code.items.len - 2;
}

fn patchJump(self: *Codegen, offset: usize) void {
    // -2 to adjust for the bytecode for the jump offset itself
    const jump = self.currentChunk().code.items.len - offset - 2;

    if (jump > std.math.maxInt(u16)) {
        std.log.err("Too much code to jump over.\n", .{});
    }

    self.currentChunk().code.items[offset] = @as(u8, @truncate((jump >> 8))) & 0xff;
    self.currentChunk().code.items[offset + 1] = @truncate(jump & 0xff);
}

fn emitLoop(self: *Codegen, loop_start: usize) void {
    self.emitByte(@intFromEnum(OpCode.loop));

    const offset = self.currentChunk().code.items.len - loop_start + 2;
    if (offset > std.math.maxInt(u16)) {
        std.log.err("Loop body too large.\n", .{});
    }

    self.emitByte(@truncate((offset >> 8) & 0xff));
    self.emitByte(@truncate(offset & 0xff));
}

fn currentChunk(self: *Codegen) *Chunk {
    return &self.compiler.func.chunk;
}

// ============================================================================
// Variable resolution and emission
// ============================================================================

fn emitVariableGet(self: *Codegen, name: []const u8) !void {
    std.debug.print("DEBUG: Getting var '{s}' at scope depth {}\n", .{ name, self.compiler.scope_depth });
    var get_op: OpCode = undefined;
    var arg: u8 = undefined;

    if (self.resolveLocal(name)) |local| {
        arg = local;
        get_op = .get_local;
    } else if (try self.resolveUpvalue(name)) |upvalue| {
        arg = upvalue;
        get_op = .get_upvalue;
    } else {
        arg = try self.makeStringConstant(name);
        get_op = .get_global;
    }

    self.emitBytes(@intCast(@intFromEnum(get_op)), arg);
}

fn emitVariableSet(self: *Codegen, name: []const u8) !void {
    var set_op: OpCode = undefined;
    var arg: u8 = undefined;

    if (self.resolveLocal(name)) |local| {
        arg = local;
        set_op = .set_local;
    } else if (try self.resolveUpvalue(name)) |upvalue| {
        arg = upvalue;
        set_op = .set_upvalue;
    } else {
        arg = try self.makeStringConstant(name);
        set_op = .set_global;
    }

    self.emitBytes(@intCast(@intFromEnum(set_op)), arg);
}

fn resolveLocal(self: *Codegen, name: []const u8) ?u8 {
    var i: usize = self.compiler.local_count;
    while (i > 0) {
        i -= 1;
        const local = self.compiler.locals[i];
        if (std.mem.eql(u8, name, local.name)) {
            if (local.depth == null) {
                std.log.err("Can't read local variable in its own initializer.\n", .{});
            }
            return @intCast(i);
        }
    }
    return null;
}

fn resolveLocalInCompiler(self: *Codegen, compiler: *Compiler, name: []const u8) ?u8 {
    _ = self;
    var i: usize = compiler.local_count;
    while (i > 0) {
        i -= 1;
        const local = compiler.locals[i];
        if (std.mem.eql(u8, name, local.name)) {
            if (local.depth == null) {
                std.log.err("Can't read local variable in its own initializer.\n", .{});
            }
            return @intCast(i);
        }
    }
    return null;
}

fn resolveUpvalue(self: *Codegen, name: []const u8) !?u8 {
    if (self.compiler.enclosing) |enclosing| {
        // Look up in the parent compiler's locals
        if (self.resolveLocalInCompiler(enclosing, name)) |local| {
            enclosing.locals[local].is_captured = true;
            return try self.compiler.addUpvalue(local, .local);
        }

        // Recursively check parent's upvalues
        var parent_codegen = Codegen{
            .compiler = enclosing,
            .vm = self.vm,
            .allocator = self.allocator,
        };
        if (try parent_codegen.resolveUpvalue(name)) |upvalue| {
            return try self.compiler.addUpvalue(upvalue, .enclosing);
        }
    }
    return null;
}

fn addLocal(self: *Codegen, name: []const u8) !void {
    if (self.compiler.local_count == std.math.maxInt(u8)) {
        return error.TooManyLocalVariables;
    }
    self.compiler.locals[self.compiler.local_count] = .{ .name = name, .depth = null };
    self.compiler.local_count += 1;
}

// ============================================================================
// Scope management
// ============================================================================

fn beginScope(self: *Codegen) void {
    self.compiler.scope_depth += 1;
}

fn endScope(self: *Codegen) void {
    self.compiler.scope_depth -= 1;

    while (self.compiler.local_count > 1 and
        self.compiler.locals[self.compiler.local_count - 1].depth.? > self.compiler.scope_depth)
    {
        if (self.compiler.locals[self.compiler.local_count - 1].is_captured) {
            self.emitByte(@intFromEnum(OpCode.close_upvalue));
        } else {
            self.emitByte(@intFromEnum(OpCode.pop));
        }
        self.compiler.local_count -= 1;
    }
}

fn markInitialized(self: *Codegen) void {
    if (self.compiler.scope_depth == 0) return;
    self.compiler.locals[self.compiler.local_count - 1].depth = self.compiler.scope_depth;
}

/// Internal function to end compiler without emitting return (used for nested functions)
fn endCompilerInternal(self: *Codegen) *Obj.Function {
    self.emitReturn();
    const func = self.compiler.func;

    if (self.compiler.enclosing) |compiler| {
        self.compiler = compiler;
    }

    if (comptime debug.print_code) {
        Chunk.disassemble(&func.chunk, if (func.name) |name| name.bytes else "<script>");
        std.debug.print("-----------------\n", .{});
    }

    return func;
}
