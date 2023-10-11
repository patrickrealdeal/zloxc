const std = @import("std");
const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;
const Value = @import("./value.zig").Value;

// 1 byte opcodes
pub const OpCode = enum(u8) {
    OP_CONSTANT,
    OP_RETURN, // return from current function
};

pub const Chunk = struct {
    code: ArrayList(u8),
    constants: ArrayList(Value),

    pub fn init(allocator: Allocator) Chunk {
        return .{
            .code = ArrayList(u8).init(allocator),
            .constants = ArrayList(Value).init(allocator),
        };
    }

    pub fn deinit(self: *Chunk) void {
        self.code.deinit();
        self.constants.deinit();
    }

    pub fn write(self: *Chunk, byte: u8) !void {
        try self.code.append(byte);
    }

    /// returns the index of the appended constant to locate it later
    pub fn addConstant(self: *Chunk, value: Value) !u8 {
        const index: u8 = @intCast(self.constants.items.len);
        try self.constants.append(value);
        return index;
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
            .OP_CONSTANT => self.constantInstruction("OP_CONSTANT", offset),
        };
    }

    fn simpleInstruction(self: *Chunk, name: []const u8, offset: usize) usize {
        _ = self;
        std.debug.print("{s}\n", .{name});
        return offset + 1;
    }

    fn constantInstruction(self: *Chunk, name: []const u8, offset: usize) usize {
        const constant = self.code.items[offset + 1];
        std.debug.print("{s} {d} {}\n", .{ name, constant, self.constants.items[constant] });
        return offset + 2;
    }
};
