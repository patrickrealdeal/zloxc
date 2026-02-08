const std = @import("std");
const Token = @import("token.zig");
const TokenType = @import("token.zig").TokenType;
const Value = @import("value.zig").Value;
const Obj = @import("object.zig");
const Type = @import("type.zig").Type;
const TypeAnnotation = @import("type.zig").TypeAnnotation;

const Allocator = std.mem.Allocator;

pub const SourceLoc = struct {
    line: usize,
    column: usize,
    length: usize,
};

pub const Node = union(enum) {
    literal: Value,
    number_literal: struct {
        value: Value,
        lexeme: []const u8, // Original source text (e.g., "0.0" or "42")
    },
    binary: struct {
        left: *Node,
        op: TokenType,
        right: *Node,
        result_type: Type,
    },
    unary: struct {
        op: TokenType,
        right: *Node,
    },
    var_declaration: struct {
        name: []const u8,
        initializer: ?*Node,
        scope_depth_at_declaration: u32,
        type_annotation: ?TypeAnnotation,
        inferred_type: Type,
    },
    var_ref: struct {
        name: []const u8,
    },
    assignment: struct {
        target: []const u8,
        value: *Node,
        loc: SourceLoc,
    },
    function_declaration: struct {
        name: []const u8,
        params: []Parameter, // Parameter names
        body: *Node, // Should be a block
        arity: u8,
        scope_depth_at_declaration: u32,
        return_type: Type,
    },
    class_declaration: struct {
        name: []const u8,
        scope_depth_at_declaration: u32,
        //token: Token,
    },
    call: struct {
        callee: *Node, // What we're calling (usually a var_ref)
        arguments: []*Node,
    },
    logical: struct {
        left: *Node,
        op: TokenType,
        right: *Node,
    },
    block: struct { statements: []*Node },
    // Statements
    print_statement: *Node, // expr to be printed
    expression_statement: *Node,
    if_statement: struct {
        condition: *Node,
        then_branch: *Node,
        else_branch: ?*Node,
    },
    for_statement: struct {
        initializer: ?*Node, // var i = 0 or i = 0 or null
        condition: ?*Node, // i < 10 or null
        increment: ?*Node, // i = i + 1 or null
        body: *Node,
    },
    while_statement: struct {
        condition: *Node,
        body: *Node,
    },
    ret_statement: struct {
        value: ?*Node,
    },
};

/// Typed parameter for functions
pub const Parameter = struct {
    name: []const u8,
    param_type: Type,
};

pub fn printAst(node: *const Node, indent: usize) void {
    // Print indentation
    var i: usize = 0;
    while (i < indent) : (i += 1) {
        std.debug.print("  ", .{});
    }

    switch (node.*) {
        .literal => |val| {
            std.debug.print("Literal: ", .{});
            if (val.isNumber()) {
                std.debug.print("{d}\n", .{val.asNumber()});
            } else if (val.isBool()) {
                std.debug.print("{}\n", .{val.asBool()});
            } else if (val.isNil()) {
                std.debug.print("nil\n", .{});
            } else if (val.isObj()) {
                const o = val.asObj();
                if (o.obj_t == .string) {
                    const str: *Obj.String = @fieldParentPtr("obj", o);
                    std.debug.print("\"{s}\"\n", .{str.bytes});
                } else {
                    std.debug.print("<obj>\n", .{});
                }
            } else {
                std.debug.print("Unknown NaN-boxed value\n", .{});
            }

            //std.debug.print("Literal: ", .{});
            //switch (val) {
            //.number => |n| std.debug.print("{d}\n", .{n}),
            //.bool => |b| std.debug.print("{}\n", .{b}),
            //.nil => std.debug.print("nil\n", .{}),
            //.obj => |o| {
            //if (o.obj_t == .string) {
            //const str: *Obj.String = @fieldParentPtr("obj", o);
            //std.debug.print("\"{s}\"\n", .{str.bytes});
            //} else {
            //std.debug.print("<obj>\n", .{});
            //}
            //},
            //}
        },

        .number_literal => |num_lit| {
            std.debug.print("NumberLiteral: {s} (value: {d})\n", .{ num_lit.lexeme, num_lit.value.asNumber() });
        },

        .binary => |bin| {
            std.debug.print("Binary ({s}):\n", .{@tagName(bin.op)});
            printAst(bin.left, indent + 1);
            printAst(bin.right, indent + 1);
        },

        .unary => |un| {
            std.debug.print("Unary ({s}):\n", .{@tagName(un.op)});
            printAst(un.right, indent + 1);
        },

        .logical => |log| {
            std.debug.print("Logical ({s}):\n", .{@tagName(log.op)});
            printAst(log.left, indent + 1);
            printAst(log.right, indent + 1);
        },

        .var_declaration => |decl| {
            std.debug.print("VarDecl: {s}\n", .{decl.name});
            if (decl.initializer) |init| {
                printAst(init, indent + 1); // Changed: removed 'try', use 'indent' parameter
            }
        },
        .var_ref => |var_ref| {
            std.debug.print("VarRef: {s}\n", .{var_ref.name});
        },

        .assignment => |assign| {
            std.debug.print("Assignment: {s}\n", .{assign.target});
            printAst(assign.value, indent + 1);
        },

        .function_declaration => |func| {
            std.debug.print("Function: {s}(", .{func.name});
            for (func.params, 0..) |param, j| {
                std.debug.print("{s}", .{param});
                if (j < func.params.len - 1) std.debug.print(", ", .{});
            }
            std.debug.print(")\n", .{});
            printAst(func.body, indent + 1);
        },

        .call => |call| {
            std.debug.print("Call:\n", .{});
            printIndent(indent + 1);
            std.debug.print("Callee:\n", .{});
            printAst(call.callee, indent + 2);
            if (call.arguments.len > 0) {
                printIndent(indent + 1);
                std.debug.print("Arguments:\n", .{});
                for (call.arguments) |arg| {
                    printAst(arg, indent + 2);
                }
            }
        },

        .block => |blk| {
            std.debug.print("Block ({} statements):\n", .{blk.statements.len});
            for (blk.statements) |stmt| {
                printAst(stmt, indent + 1);
            }
        },
        .class_declaration => |class| {
            std.debug.print("Class: {s}(", .{class.name});
            std.debug.print(")\n", .{});
        },
        .if_statement => |if_stmt| {
            std.debug.print("If:\n", .{});
            printIndent(indent + 1);
            std.debug.print("Condition:\n", .{});
            printAst(if_stmt.condition, indent + 2);
            printIndent(indent + 1);
            std.debug.print("Then:\n", .{});
            printAst(if_stmt.then_branch, indent + 2);
            if (if_stmt.else_branch) |else_br| {
                printIndent(indent + 1);
                std.debug.print("Else:\n", .{});
                printAst(else_br, indent + 2);
            }
        },

        .while_statement => |while_stmt| {
            std.debug.print("While:\n", .{});
            printIndent(indent + 1);
            std.debug.print("Condition:\n", .{});
            printAst(while_stmt.condition, indent + 2);
            printIndent(indent + 1);
            std.debug.print("Body:\n", .{});
            printAst(while_stmt.body, indent + 2);
        },

        .for_statement => |for_stmt| {
            std.debug.print("For:\n", .{});
            if (for_stmt.initializer) |init| {
                printIndent(indent + 1);
                std.debug.print("Init:\n", .{});
                printAst(init, indent + 2);
            }
            if (for_stmt.condition) |cond| {
                printIndent(indent + 1);
                std.debug.print("Condition:\n", .{});
                printAst(cond, indent + 2);
            }
            if (for_stmt.increment) |incr| {
                printIndent(indent + 1);
                std.debug.print("Increment:\n", .{});
                printAst(incr, indent + 2);
            }
            printIndent(indent + 1);
            std.debug.print("Body:\n", .{});
            printAst(for_stmt.body, indent + 2);
        },

        .print_statement => |value| {
            std.debug.print("Print:\n", .{});
            printAst(value, indent + 1);
        },

        .expression_statement => |expr| {
            std.debug.print("ExprStmt:\n", .{});
            printAst(expr, indent + 1);
        },

        .ret_statement => |ret| {
            std.debug.print("Return:\n", .{});
            if (ret.value) |val| {
                printAst(val, indent + 1);
            }
        },
    }
}

// Helper function
fn printIndent(indent: usize) void {
    var i: usize = 0;
    while (i < indent) : (i += 1) {
        std.debug.print("  ", .{});
    }
}

