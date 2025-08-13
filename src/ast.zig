const std = @import("std");
const Token = @import("token.zig");
const Allocator = std.mem.Allocator;

const Tree = struct {
    root: *Node,
    allocator: Allocator,

    pub fn init(allocator: Allocator) Tree {
        return .{
            .root = undefined,
            .allocator = allocator,
        };
    }
};

const Node = struct {
    id: Id,

    pub const Id = enum {
        root,
        // Expressions
        binary_expressio,
        unary_expression,
        literal_expression,
        variable_expression,
        assign_expression,
        call_expression,
        grouping_expression,

        // Statements
        expression_statement,
        print_statement,
        var_decl,
        if_stmt,
        while_statement,
        for_stm,
        block_statement,
        fun_decl,
        return_statement,
    };

    pub fn Type(comptime id: Id) type {
        return switch (id) {
            .root => Root,
            .literal_expression => LiteralExpression,
            else => unreachable,
        };
    }

    pub const Root = struct {
        base: Node = .{ .id = .root },
        body: []*Node,
    };

    const LiteralExpression = struct {
        base: Node = .{ .id = .literal_expression },
        token: Token,
    };
};
