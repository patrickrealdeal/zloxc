const std = @import("std");
const Value = @import("value.zig").Value;

pub const OpCode = enum(usize) {
    constant,
    negate,
    ret,
};

pub const Chunk = struct {
    code: std.ArrayList(u8),
    constants: std.ArrayList(Value),
    lines: std.ArrayList(usize),

    pub fn init(allocator: std.mem.Allocator) Chunk {
        return Chunk{
            .code = std.ArrayList(u8).init(allocator),
            .constants = std.ArrayList(Value).init(allocator),
            .lines = std.ArrayList(usize).init(allocator),
        };
    }

    pub fn deinit(self: *Chunk) void {
        self.code.deinit();
        self.constants.deinit();
        self.lines.deinit();
    }

    /// returns index where constants was appended
    pub fn addConstant(self: *Chunk, value: Value) !usize {
        try self.constants.append(value);
        return self.constants.items.len - 1;
    }

    pub fn write(self: *Chunk, byte: u8, line: usize) !void {
        try self.code.append(byte);
        try self.lines.append(line);
    }

    pub fn disassemble(self: *Chunk, name: []const u8) void {
        std.debug.print("=== {s} ===\n", .{name});

        var offset: usize = 0;
        while (offset < self.code.items.len) {
            offset = self.disassembleInstruction(offset);
        }
    }

    pub fn disassembleInstruction(self: *Chunk, offset: usize) usize {
        std.debug.print("{d:0>4} ", .{offset});
        if (offset > 0 and self.lines.items[offset] == self.lines.items[offset - 1]) {
            std.debug.print("    | ", .{});
        } else {
            std.debug.print("{d:4} ", .{self.lines.items[offset]});
        }

        const instruction: OpCode = @enumFromInt(self.code.items[offset]);
        switch (instruction) {
            .constant => return constantInstruction("op_constant", self, offset),
            .negate => return simpleInstruction("op_negate", offset),
            .ret => return simpleInstruction("op_ret", offset),
        }

        return offset + 1;
    }

    fn simpleInstruction(name: []const u8, offset: usize) usize {
        std.debug.print("{s}\n", .{name});
        return offset + 1;
    }

    fn constantInstruction(name: []const u8, chunk: *Chunk, offset: usize) usize {
        const constant = chunk.code.items[offset + 1];
        std.debug.print(" {s: <16} {d:4} '", .{ name, constant });
        std.debug.print("{d}'\n", .{chunk.constants.items[constant]});
        return offset + 2;
    }
};
