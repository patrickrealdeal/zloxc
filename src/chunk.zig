const std = @import("std");
const Value = @import("value.zig").Value;

const Chunk = @This();

pub const OpCode = enum(usize) {
    constant,
    negate,
    add,
    sub,
    mul,
    div,
    nil,
    true,
    false,
    not,
    equal,
    greater,
    less,
    print,
    pop,
    define_global,
    get_global,
    set_global,
    get_local,
    set_local,
    jump_if_false,
    jump,
    loop,
    call,
    ret,
};

const Sign = enum { neg, pos };

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
    std.debug.print("{d:0>4}", .{offset});
    if (offset > 0 and self.lines.items[offset] == self.lines.items[offset - 1]) {
        std.debug.print("    | ", .{});
    } else {
        std.debug.print("{d:4}  ", .{self.lines.items[offset]});
    }

    const instruction: OpCode = @enumFromInt(self.code.items[offset]);
    switch (instruction) {
        .constant => return constantInstruction("op_constant", self, offset),
        .negate => return simpleInstruction("op_negate", offset),
        .add => return simpleInstruction("op_add", offset),
        .sub => return simpleInstruction("op_sub", offset),
        .mul => return simpleInstruction("op_mul", offset),
        .div => return simpleInstruction("op_div", offset),
        .true => return simpleInstruction("op_true", offset),
        .false => return simpleInstruction("op_false", offset),
        .nil => return simpleInstruction("op_nil", offset),
        .not => return simpleInstruction("op_not", offset),
        .equal => return simpleInstruction("op_equal", offset),
        .greater => return simpleInstruction("op_greater", offset),
        .less => return simpleInstruction("op_less", offset),
        .print => return simpleInstruction("op_print", offset),
        .pop => return simpleInstruction("op_pop", offset),
        .define_global => return constantInstruction("op_define_global", self, offset),
        .get_global => return constantInstruction("op_get_global", self, offset),
        .set_global => return constantInstruction("op_set_global", self, offset),
        .get_local => return byteInstruction("op_get_local", self, offset),
        .set_local => return byteInstruction("op_set_local", self, offset),
        .jump_if_false => return jumpInstruction("op_jump_if_false", .pos, self, offset),
        .jump => return jumpInstruction("op_jump", .pos, self, offset),
        .loop => return jumpInstruction("op_loop", .neg, self, offset),
        .call => return byteInstruction("op_call", self, offset),
        .ret => return simpleInstruction("op_ret", offset),
    }
}

fn simpleInstruction(name: []const u8, offset: usize) usize {
    std.debug.print("{s}\n", .{name});
    return offset + 1;
}

fn constantInstruction(name: []const u8, chunk: *Chunk, offset: usize) usize {
    const constant_idx = chunk.code.items[offset + 1];
    const constant = chunk.constants.items[constant_idx];
    std.debug.print("{s: <16} {d:4} '{d}'\n", .{ name, constant, constant_idx });
    return offset + 2;
}

fn byteInstruction(name: []const u8, chunk: *Chunk, offset: usize) usize {
    const slot = chunk.code.items[offset + 1];
    std.debug.print("{s: <16} {d:4}\n", .{ name, slot });
    return offset + 2;
}

fn jumpInstruction(name: []const u8, sign: Sign, chunk: *Chunk, offset: usize) usize {
    const byte1: u16 = chunk.code.items[offset + 1];
    const byte2 = chunk.code.items[offset + 2];
    const jump = (byte1 << 8) | byte2;
    const jump_addr = switch (sign) {
        .neg => offset + 3 - jump,
        .pos => offset + 3 + jump,
    };
    std.debug.print("{s: <16} {d:4} -> {d}\n", .{ name, offset, jump_addr });
    return offset + 3;
}
