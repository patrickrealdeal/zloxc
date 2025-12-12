const std = @import("std");
const Token = @import("token.zig");
const TokenType = @import("token.zig").TokenType;
const Value = @import("value.zig").Value;
const Allocator = std.mem.Allocator;

pub const Node = union(enum) {
    literal: Value,
    binary: struct {
        left: *Node,
        op: TokenType,
        right: *Node,
    },
    unary: struct {
        op: TokenType,
        right: *Node,
    },

    // Statements
    print_statement: *Node, // expr to be printed
    expression_statement: *Node,
};

pub fn createBinary(allocator: Allocator, left: *Node, op: TokenType, right: *Node) !*Node {
    const node = try allocator.create(Node);
    node.* = .{ .binary = .{ .left = left, .op = op, .right = right } };
    return node;
}

pub fn createUnary(allocator: Allocator, op: TokenType, right: *Node) !*Node {
    const node = try allocator.create(Node);
    node.* = .{ .unary = .{ .op = op, .right = right } };
    return node;
}
