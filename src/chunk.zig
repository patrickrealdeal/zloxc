const std = @import("std");
const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;
const Value = @import("./value.zig").Value;

// 1 byte opcodes
pub const OpCode = enum(u8) {
    CONSTANT, // 2 bytes 1 for opcode 1 for operand (index)
    NIL,
    TRUE,
    FALSE,
    POP,
    DEFINE_GLOBAL,
    GET_LOCAL,
    SET_LOCAL,
    GET_GLOBAL,
    SET_GLOBAL,
    EQUAL,
    GREATER,
    LESS,
    ADD,
    SUBTRACT,
    MULTIPLY,
    DIVIDE,
    NOT,
    NEGATE,
    PRINT,
    JUMP,
    JUMP_IF_FALSE,
    LOOP,
    RETURN, // return from current function
};

const Line = struct {
    line: u32,
    offset: u32,
};

pub const Chunk = struct {
    code: ArrayList(u8),
    constants: ArrayList(Value),
    lines: ArrayList(usize),

    pub fn init(allocator: Allocator) Chunk {
        return .{
            .code = ArrayList(u8).init(allocator),
            .constants = ArrayList(Value).init(allocator),
            .lines = ArrayList(usize).init(allocator),
        };
    }

    pub fn deinit(self: *Chunk) void {
        self.code.deinit();
        self.constants.deinit();
        self.lines.deinit();
    }

    pub fn ptr(self: *const Chunk) [*]u8 {
        return self.code.items.ptr;
    }

    pub fn write(self: *Chunk, byte: u8, line: usize) !void {
        try self.code.append(byte);
        try self.lines.append(line);
    }

    pub fn writeOp(self: *Chunk, op: OpCode, line: usize) !void {
        try self.write(@intFromEnum(op), line);
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

        // Print line
        if (offset > 0 and self.lines.items[offset] == self.lines.items[offset - 1]) {
            std.debug.print("     | ", .{});
        } else {
            std.debug.print("{: >4} ", .{self.lines.items[offset]});
        }

        const instruction: OpCode = @enumFromInt(self.code.items[offset]);
        return switch (instruction) {
            .RETURN => self.simpleInstruction("RETURN", offset),
            .CONSTANT => self.constantInstruction("CONSTANT", offset),
            .NEGATE => self.simpleInstruction("NEGATE", offset),
            .ADD => self.simpleInstruction("ADD", offset),
            .SUBTRACT => self.simpleInstruction("SUBTRACT", offset),
            .MULTIPLY => self.simpleInstruction("MULTIPLY", offset),
            .DIVIDE => self.simpleInstruction("DIVIDE", offset),
            .TRUE => self.simpleInstruction("TRUE", offset),
            .FALSE => self.simpleInstruction("FALSE", offset),
            .NIL => self.simpleInstruction("NIL", offset),
            .NOT => self.simpleInstruction("NOT", offset),
            .GREATER => self.simpleInstruction("GREATER", offset),
            .EQUAL => self.simpleInstruction("EQUAL", offset),
            .LESS => self.simpleInstruction("LESS", offset),
            .PRINT => self.simpleInstruction("PRINT", offset),
            .POP => self.simpleInstruction("POP", offset),
            .DEFINE_GLOBAL => self.constantInstruction("DEFINE_GLOBAL", offset),
            .GET_GLOBAL => self.constantInstruction("GET_GLOBAL", offset),
            .SET_GLOBAL => self.constantInstruction("SET_GLOBAL", offset),
            .GET_LOCAL => self.byteInstruction("GET_LOCAL", offset),
            .SET_LOCAL => self.byteInstruction("SET_LOCAL", offset),
            .JUMP => self.jumpInstruction("JUMP", 1, offset),
            .JUMP_IF_FALSE => self.jumpInstruction("JUMP_IF_FALSE", 1, offset),
            .LOOP => self.jumpInstruction("LOOP", -1, offset),
        };
    }

    fn byteInstruction(self: *Chunk, name: []const u8, offset: usize) usize {
        const slot = self.code.items[offset + 1];
        std.debug.print("{s} {d}", .{ name, slot });
        return offset + 2;
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

    fn jumpInstruction(self: *Chunk, name: []const u8, sign: isize, offset: usize) usize {
        var jump = @as(u16, @intCast(self.code.items[offset + 1])) << 8;
        jump |= self.code.items[offset + 2];
        const target = @as(isize, @intCast(offset)) + 3 + sign * @as(isize, @intCast(jump));
        std.debug.print("{s} {d} -> {d}\n", .{ name, offset, target });
        return offset + 3;
    }
};
