const std = @import("std");
const Token = @import("token.zig");
const Node = @import("ast.zig").Node;
const Parser = @import("compiler.zig").Parser;
const OpCode = @import("chunk.zig").OpCode;
const Value = @import("value.zig").Value;
const Compiler = @import("compiler.zig").Compiler;
const Obj = @import("object.zig");

parser: *Parser,

const Codegen = @This();

pub fn init(parser: *Parser) Codegen {
    return .{ .parser = parser };
}

pub fn emitFromAst(self: *Codegen, node: *Node) !void {
    switch (node.*) {
        .literal => |val| {
            try self.parser.emitConstant(val);
        },
        .binary => |bin| {
            try self.emitFromAst(bin.left);
            try self.emitFromAst(bin.right);
            switch (bin.op) {
                .plus => self.parser.emitByte(@intFromEnum(OpCode.add)),
                .minus => self.parser.emitByte(@intFromEnum(OpCode.sub)),
                .star => self.parser.emitByte(@intFromEnum(OpCode.mul)),
                .slash => self.parser.emitByte(@intFromEnum(OpCode.div)),
                .bang_equal => self.parser.emitBytes(@intFromEnum(OpCode.equal), @intFromEnum(OpCode.not)),
                .equal_equal => self.parser.emitByte(@intFromEnum(OpCode.equal)),
                .greater => self.parser.emitByte(@intFromEnum(OpCode.greater)),
                .greater_equal => self.parser.emitBytes(@intFromEnum(OpCode.less), @intFromEnum(OpCode.not)),
                .less => self.parser.emitByte(@intFromEnum(OpCode.less)),
                .less_equal => self.parser.emitBytes(@intFromEnum(OpCode.greater), @intFromEnum(OpCode.not)),
                else => unreachable,
            }
        },
        .logical => |logical| {
            try self.emitFromAst(logical.left);
            if (logical.op == .@"and") {
                const end_jump = self.parser.emitJump(@intFromEnum(OpCode.jump_if_false));
                self.parser.emitByte(@intFromEnum(OpCode.pop));
                try self.emitFromAst(logical.right);
                self.parser.patchJump(end_jump);
            } else {
                const else_jump = self.parser.emitJump(@intFromEnum(OpCode.jump_if_false));
                const end_jump = self.parser.emitJump(@intFromEnum(OpCode.jump));
                self.parser.patchJump(else_jump);
                self.parser.emitByte(@intFromEnum(OpCode.pop));
                try self.emitFromAst(logical.right);
                self.parser.patchJump(end_jump);
            }
        },
        .unary => |un| {
            try self.emitFromAst(un.right);
            switch (un.op) {
                .minus => self.parser.emitByte(@intFromEnum(OpCode.negate)),
                .bang => self.parser.emitByte(@intFromEnum(OpCode.not)),
                else => unreachable,
            }
        },
        .var_declaration => |decl| {
            if (decl.initializer) |initt| {
                try self.emitFromAst(initt);
            } else {
                self.parser.emitByte(@intFromEnum(OpCode.nil));
            }
            // Decide global vs local based on CURRENT scope_depth during emission
            if (self.parser.compiler.scope_depth == 0) {
                // It's a global
                const name_token = Token{
                    .ttype = .identifier,
                    .lexeme = decl.name,
                    .line = self.parser.previous.line,
                };
                const global = try self.parser.identifierConstant(name_token);
                self.parser.emitBytes(@intFromEnum(OpCode.define_global), global);
            } else {
                // It's a local
                const name_token = Token{
                    .ttype = .identifier,
                    .lexeme = decl.name,
                    .line = self.parser.previous.line,
                };
                try self.parser.compiler.addLocal(name_token);
                self.parser.markInitialized();
            }
        },
        .var_ref => |var_ref| {
            const token_name = Token{
                .ttype = .identifier,
                .lexeme = var_ref.name,
                .line = self.parser.previous.line,
            };
            try self.parser.emitVariableGet(token_name);
        },
        .assignment => |assign| {
            try self.emitFromAst(assign.value);
            const token_name = Token{
                .ttype = .identifier,
                .lexeme = assign.target,
                .line = self.parser.previous.line,
            };
            try self.parser.emitVariableSet(token_name);
        },
        .block => |blk| {
            self.parser.beginScope();
            for (blk.statements) |stmt| {
                try self.emitFromAst(stmt);
                if (stmt.* == .ret_statement) break;
            }
            self.parser.endScope();
        },
        .if_statement => |if_stmt| {
            try self.emitFromAst(if_stmt.condition);
            const then_jump = self.parser.emitJump(@intFromEnum(OpCode.jump_if_false));
            self.parser.emitByte(@intFromEnum(OpCode.pop));
            try self.emitFromAst(if_stmt.then_branch);
            const else_jump = self.parser.emitJump(@intFromEnum(OpCode.jump));
            self.parser.patchJump(then_jump);
            self.parser.emitByte(@intFromEnum(OpCode.pop));
            if (if_stmt.else_branch) |else_br| {
                try self.emitFromAst(else_br);
            }
            self.parser.patchJump(else_jump);
        },
        .for_statement => |for_stmt| {
            // Emit initializer
            if (for_stmt.initializer) |initt| {
                try self.emitFromAst(initt);
            }
            const loop_start = self.parser.currentChunk().code.items.len;
            // Emit condition (or assume true)
            var exit_jump: ?usize = null;
            if (for_stmt.condition) |cond| {
                try self.emitFromAst(cond);
                exit_jump = self.parser.emitJump(@intFromEnum(OpCode.jump_if_false));
                self.parser.emitByte(@intFromEnum(OpCode.pop));
            }
            // Jump over increment to body
            var body_jump: ?usize = null;
            if (for_stmt.increment != null) {
                body_jump = self.parser.emitJump(@intFromEnum(OpCode.jump));
            }
            // Emit increment
            const increment_start = self.parser.currentChunk().code.items.len;
            if (for_stmt.increment) |incr| {
                try self.emitFromAst(incr);
                self.parser.emitByte(@intFromEnum(OpCode.pop));
            }
            // Loop back to condition
            self.parser.emitLoop(loop_start);
            // Patch jump to body
            if (body_jump) |jump| {
                self.parser.patchJump(jump);
            }
            // Emit body
            try self.emitFromAst(for_stmt.body);
            // Loop back to increment (or condition if no increment)
            if (for_stmt.increment != null) {
                self.parser.emitLoop(increment_start);
            } else {
                self.parser.emitLoop(loop_start);
            }
            // Patch exit jump
            if (exit_jump) |jump| {
                self.parser.patchJump(jump);
                self.parser.emitByte(@intFromEnum(OpCode.pop));
            }
        },
        .while_statement => |while_stmt| {
            const loop_start = self.parser.currentChunk().code.items.len;
            // Emit condition
            try self.emitFromAst(while_stmt.condition);
            // Exit if false
            const exit_jump = self.parser.emitJump(@intFromEnum(OpCode.jump_if_false));
            self.parser.emitByte(@intFromEnum(OpCode.pop));
            // Emit body
            try self.emitFromAst(while_stmt.body);
            // Loop back
            self.parser.emitLoop(loop_start);
            self.parser.patchJump(exit_jump);
            self.parser.emitByte(@intFromEnum(OpCode.pop));
        },
        .function_declaration => |func_decl| {
            // Save current compiler
            const enclosing_compiler = self.parser.compiler;
            // Create new compiler for this function
            var func_compiler = try Compiler.init(self.parser.vm, .function, self.parser.compiler);
            defer func_compiler.deinit();
            self.parser.compiler = &func_compiler;
            self.parser.compiler.func.name = try Obj.String.copy(self.parser.vm, func_decl.name);
            self.parser.compiler.func.arity = func_decl.arity;
            // Begin function scope
            self.parser.beginScope();
            // Add parameters as locals
            for (func_decl.params) |param| {
                const param_token = Token{
                    .ttype = .identifier,
                    .lexeme = param,
                    .line = self.parser.previous.line,
                };
                try self.parser.compiler.addLocal(param_token);
                self.parser.markInitialized();
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
            self.parser.endScope();
            // End function compilation
            const func = self.parser.endCompiler();
            // Restore enclosing compiler
            self.parser.compiler = enclosing_compiler;
            // Emit closure instruction with upvalues
            const func_constant = try self.parser.makeConstant(Value.fromObj(&func.obj));
            self.parser.emitBytes(@intFromEnum(OpCode.closure), func_constant);
            // Emit upvalue information
            for (func_compiler.upvalues.items) |upvalue| {
                self.parser.emitByte(if (upvalue.source == .local) 1 else 0);
                self.parser.emitByte(upvalue.index);
            }
            // If it's a global, define it; if local, it's already on stack
            if (self.parser.compiler.scope_depth == 0) {
                const name_token = Token{
                    .ttype = .identifier,
                    .lexeme = func_decl.name,
                    .line = self.parser.previous.line,
                };
                const global = try self.parser.identifierConstant(name_token);
                self.parser.emitBytes(@intFromEnum(OpCode.define_global), global);
            } else {
                const name_token = Token{
                    .ttype = .identifier,
                    .lexeme = func_decl.name,
                    .line = self.parser.previous.line,
                };
                try self.parser.compiler.addLocal(name_token);
                self.parser.markInitialized();
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
            self.parser.emitBytes(@intFromEnum(OpCode.call), @intCast(call_expr.arguments.len));
        },
        .print_statement => |value| {
            try self.emitFromAst(value);
            self.parser.emitByte(@intFromEnum(OpCode.print));
        },
        .expression_statement => |expr| {
            try self.emitFromAst(expr);
            self.parser.emitByte(@intFromEnum(OpCode.pop));
        },
        .ret_statement => |ret| {
            if (ret.value) |val| {
                try self.emitFromAst(val);
                self.parser.emitByte(@intFromEnum(OpCode.ret));
            } else {
                self.parser.emitReturn();
            }
        },
    }
}

