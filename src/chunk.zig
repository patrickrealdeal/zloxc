const std = @import("std");
const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;

// 1 byte opcodes
pub const OpCode = enum(u8) {
    OP_RETURN, // return from current function
};

pub const Chunk = struct {
    code: ArrayList(u8),

    pub fn init(allocator: Allocator) Chunk {
        return .{
            .code = ArrayList(u8).init(allocator),
        };
    }

    pub fn deinit(self: *Chunk) void {
        self.code.deinit();
    }

    pub fn write(self: *Chunk, byte: u8) !void {
        try self.code.append(byte);
    }

    pub fn disassemble(self: *Chunk, name: []const u8) !void {
        std.debug.print("== {s} ==\n", .{name});

        var i: usize = 0;
        while (i < self.code.items.len) {
            i = self.disassembleInstruction(i);
        }
    }

    fn disassembleInstruction(self: *Chunk, offset: usize) usize {
        std.debug.print("{:0>4} ", .{offset});

        const instruction: OpCode = @enumFromInt(self.code.items[offset]);
        return switch (instruction) {
            .OP_RETURN => self.simpleInstruction("OP_RETURN", offset),
        };
    }

    fn simpleInstruction(self: *Chunk, name: []const u8, offset: usize) usize {
        _ = self;
        std.debug.print("{s}\n", .{name});
        return offset + 1;
    }
};
