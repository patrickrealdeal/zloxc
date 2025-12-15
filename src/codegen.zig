const std = @import("std");
const Token = @import("token.zig");
const Node = @import("ast.zig").Node;
const Parser = @import("compiler.zig").Parser;
const OpCode = @import("chunk.zig").OpCode;
const Value = @import("value.zig").Value;
const Compiler = @import("compiler.zig").Compiler;
const Obj = @import("object.zig");

pub fn emitFromAst(parser: *Parser, node: *Node) !void {
    switch (node.*) {
        .literal => |val| {
            try parser.emitConstant(val);
        },
        .binary => |bin| {
            try emitFromAst(parser, bin.left);
            try emitFromAst(parser, bin.right);

            switch (bin.op) {
                .plus => parser.emitByte(@intFromEnum(OpCode.add)),
                .minus => parser.emitByte(@intFromEnum(OpCode.sub)),
                .star => parser.emitByte(@intFromEnum(OpCode.mul)),
                .slash => parser.emitByte(@intFromEnum(OpCode.div)),
                .bang_equal => parser.emitBytes(@intFromEnum(OpCode.equal), @intFromEnum(OpCode.not)),
                .equal_equal => parser.emitByte(@intFromEnum(OpCode.equal)),
                .greater => parser.emitByte(@intFromEnum(OpCode.greater)),
                .greater_equal => parser.emitBytes(@intFromEnum(OpCode.less), @intFromEnum(OpCode.not)),
                .less => parser.emitByte(@intFromEnum(OpCode.less)),
                .less_equal => parser.emitBytes(@intFromEnum(OpCode.greater), @intFromEnum(OpCode.not)),
                else => unreachable,
            }
        },
        .logical => |logical| {
            try emitFromAst(parser, logical.left);

            if (logical.op == .@"and") {
                const end_jump = parser.emitJump(@intFromEnum(OpCode.jump_if_false));
                parser.emitByte(@intFromEnum(OpCode.pop));
                try emitFromAst(parser, logical.right);
                parser.patchJump(end_jump);
            } else {
                const else_jump = parser.emitJump(@intFromEnum(OpCode.jump_if_false));
                const end_jump = parser.emitJump(@intFromEnum(OpCode.jump));
                parser.patchJump(else_jump);
                parser.emitByte(@intFromEnum(OpCode.pop));
                try emitFromAst(parser, logical.right);
                parser.patchJump(end_jump);
            }
        },
        .unary => |un| {
            try emitFromAst(parser, un.right);

            switch (un.op) {
                .minus => parser.emitByte(@intFromEnum(OpCode.negate)),
                .bang => parser.emitByte(@intFromEnum(OpCode.not)),
                else => unreachable,
            }
        },
        .var_declaration => |decl| {
            if (decl.initializer) |initt| {
                try emitFromAst(parser, initt);
            } else {
                parser.emitByte(@intFromEnum(OpCode.nil));
            }

            // Decide global vs local based on CURRENT scope_depth during emission
            if (parser.compiler.scope_depth == 0) {
                // It's a global
                const name_token = Token{
                    .ttype = .identifier,
                    .lexeme = decl.name,
                    .line = parser.previous.line,
                };
                const global = try parser.identifierConstant(name_token);
                parser.emitBytes(@intFromEnum(OpCode.define_global), global);
            } else {
                // It's a local
                const name_token = Token{
                    .ttype = .identifier,
                    .lexeme = decl.name,
                    .line = parser.previous.line,
                };
                try parser.compiler.addLocal(name_token);
                parser.markInitialized();
            }
        },
        .var_ref => |var_ref| {
            const token_name = Token{
                .ttype = .identifier,
                .lexeme = var_ref.name,
                .line = parser.previous.line,
            };
            try parser.emitVariableGet(token_name);
        },
        .assignment => |assign| {
            try emitFromAst(parser, assign.value);

            const token_name = Token{
                .ttype = .identifier,
                .lexeme = assign.target,
                .line = parser.previous.line,
            };
            try parser.emitVariableSet(token_name);
        },
        .block => |blk| {
            parser.beginScope();

            for (blk.statements) |stmt| {
                try emitFromAst(parser, stmt);

                if (stmt.* == .ret_statement) break;
            }

            parser.endScope();
        },
        .if_statement => |if_stmt| {
            try emitFromAst(parser, if_stmt.condition);

            const then_jump = parser.emitJump(@intFromEnum(OpCode.jump_if_false));
            parser.emitByte(@intFromEnum(OpCode.pop));

            try emitFromAst(parser, if_stmt.then_branch);

            const else_jump = parser.emitJump(@intFromEnum(OpCode.jump));
            parser.patchJump(then_jump);
            parser.emitByte(@intFromEnum(OpCode.pop));

            if (if_stmt.else_branch) |else_br| {
                try emitFromAst(parser, else_br);
            }

            parser.patchJump(else_jump);
        },
        .for_statement => |for_stmt| {
            // Emit initializer
            if (for_stmt.initializer) |initt| {
                try emitFromAst(parser, initt);
            }

            const loop_start = parser.currentChunk().code.items.len;

            // Emit condition (or assume true)
            var exit_jump: ?usize = null;
            if (for_stmt.condition) |cond| {
                try emitFromAst(parser, cond);
                exit_jump = parser.emitJump(@intFromEnum(OpCode.jump_if_false));
                parser.emitByte(@intFromEnum(OpCode.pop));
            }

            // Jump over increment to body
            var body_jump: ?usize = null;
            if (for_stmt.increment != null) {
                body_jump = parser.emitJump(@intFromEnum(OpCode.jump));
            }

            // Emit increment
            const increment_start = parser.currentChunk().code.items.len;
            if (for_stmt.increment) |incr| {
                try emitFromAst(parser, incr);
                parser.emitByte(@intFromEnum(OpCode.pop));
            }

            // Loop back to condition
            parser.emitLoop(loop_start);

            // Patch jump to body
            if (body_jump) |jump| {
                parser.patchJump(jump);
            }

            // Emit body
            try emitFromAst(parser, for_stmt.body);

            // Loop back to increment (or condition if no increment)
            if (for_stmt.increment != null) {
                parser.emitLoop(increment_start);
            } else {
                parser.emitLoop(loop_start);
            }

            // Patch exit jump
            if (exit_jump) |jump| {
                parser.patchJump(jump);
                parser.emitByte(@intFromEnum(OpCode.pop));
            }
        },
        .while_statement => |while_stmt| {
            const loop_start = parser.currentChunk().code.items.len;

            // Emit condition
            try emitFromAst(parser, while_stmt.condition);

            // Exit if false
            const exit_jump = parser.emitJump(@intFromEnum(OpCode.jump_if_false));
            parser.emitByte(@intFromEnum(OpCode.pop));

            // Emit body
            try emitFromAst(parser, while_stmt.body);

            // Loop back
            parser.emitLoop(loop_start);

            parser.patchJump(exit_jump);
            parser.emitByte(@intFromEnum(OpCode.pop));
        },
        .function_declaration => |func_decl| {
            // Save current compiler
            const enclosing_compiler = parser.compiler;

            // Create new compiler for this function
            var func_compiler = try Compiler.init(parser.vm, .function, parser.compiler);
            defer func_compiler.deinit();

            parser.compiler = &func_compiler;
            parser.compiler.func.name = try Obj.String.copy(parser.vm, func_decl.name);
            parser.compiler.func.arity = func_decl.arity;

            // Begin function scope
            parser.beginScope();

            // Add parameters as locals
            for (func_decl.params) |param| {
                const param_token = Token{
                    .ttype = .identifier,
                    .lexeme = param,
                    .line = parser.previous.line,
                };
                try parser.compiler.addLocal(param_token);
                parser.markInitialized();
            }

            // Emit function body
            switch (func_decl.body.*) {
                .block => |body_block| {
                    for (body_block.statements) |stmt| {
                        try emitFromAst(parser, stmt);
                    }
                },
                else => return error.Undefined, // Function body should always be a block
            }

            parser.endScope();

            // End function compilation
            const func = parser.endCompiler();

            // Restore enclosing compiler
            parser.compiler = enclosing_compiler;

            // Emit closure instruction with upvalues
            const func_constant = try parser.makeConstant(Value.fromObj(&func.obj));
            parser.emitBytes(@intFromEnum(OpCode.closure), func_constant);

            // Emit upvalue information
            for (func_compiler.upvalues.items) |upvalue| {
                parser.emitByte(if (upvalue.source == .local) 1 else 0);
                parser.emitByte(upvalue.index);
            }

            // If it's a global, define it; if local, it's already on stack
            if (parser.compiler.scope_depth == 0) {
                const name_token = Token{
                    .ttype = .identifier,
                    .lexeme = func_decl.name,
                    .line = parser.previous.line,
                };
                const global = try parser.identifierConstant(name_token);
                parser.emitBytes(@intFromEnum(OpCode.define_global), global);
            } else {
                // Local function - add to locals and mark initialized
                const name_token = Token{
                    .ttype = .identifier,
                    .lexeme = func_decl.name,
                    .line = parser.previous.line,
                };
                try parser.compiler.addLocal(name_token);
                parser.markInitialized();
                // The closure is already on stack in the right position
            }
        },
        .call => |call_expr| {
            // Emit the callee
            try emitFromAst(parser, call_expr.callee);

            // Emit all arguments (they get pushed on stack)
            for (call_expr.arguments) |arg| {
                try emitFromAst(parser, arg);
            }

            // Emit call instruction with argument count
            parser.emitBytes(@intFromEnum(OpCode.call), @intCast(call_expr.arguments.len));
        },
        .print_statement => |value| {
            try emitFromAst(parser, value);

            parser.emitByte(@intFromEnum(OpCode.print));
        },
        .expression_statement => |expr| {
            try emitFromAst(parser, expr);
            parser.emitByte(@intFromEnum(OpCode.pop));
        },
        .ret_statement => |ret| {
            if (ret.value) |val| {
                // Return with a value: emit the value, then return
                try emitFromAst(parser, val);
                parser.emitByte(@intFromEnum(OpCode.ret));
            } else {
                // Return without value: emit nil, then return
                parser.emitReturn(); // This emits nil + ret
            }
        },
    }
}

fn isTruthy(val: Value) bool {
    return switch (val) {
        .bool => |b| b,
        .nil => false,
        else => true,
    };
}
