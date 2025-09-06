const std = @import("std");
const Value = @import("value.zig").Value;
const Obj = @import("object.zig");

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
    get_upvalue,
    set_upvalue,
    jump_if_false,
    jump,
    loop,
    call,
    closure,
    close_upvalue,
    ret,
};

const Sign = enum { neg, pos };

code: std.ArrayList(u8),
constants: std.ArrayList(Value),
lines: std.ArrayList(usize),
allocator: std.mem.Allocator,

pub fn init(allocator: std.mem.Allocator) Chunk {
    return Chunk{
        .code = .empty,
        .constants = .empty,
        .lines = .empty,
        .allocator = allocator,
    };
}

pub fn deinit(chunk: *Chunk) void {
    chunk.code.deinit(chunk.allocator);
    chunk.constants.deinit(chunk.allocator);
    chunk.lines.deinit(chunk.allocator);
}

/// returns index where constants was appended
pub fn addConstant(chunk: *Chunk, value: Value) !usize {
    try chunk.constants.append(chunk.allocator, value);
    return chunk.constants.items.len - 1;
}

pub fn write(chunk: *Chunk, byte: u8, line: usize) !void {
    try chunk.code.append(chunk.allocator, byte);
    try chunk.lines.append(chunk.allocator, line);
}

pub fn disassemble(chunk: *Chunk, name: []const u8) void {
    std.debug.print("=== {s} ===\n", .{name});

    var offset: usize = 0;
    while (offset < chunk.code.items.len) {
        offset = chunk.disassembleInstruction(offset);
    }
}

pub fn disassembleInstruction(chunk: *Chunk, offset: usize) usize {
    std.debug.print("{d:0>4}", .{offset});
    if (offset > 0 and chunk.lines.items[offset] == chunk.lines.items[offset - 1]) {
        std.debug.print("    | ", .{});
    } else {
        std.debug.print("{d:4}  ", .{chunk.lines.items[offset]});
    }

    const instruction: OpCode = @enumFromInt(chunk.code.items[offset]);
    switch (instruction) {
        .constant => return constantInstruction("op_constant", chunk, offset),
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
        .define_global => return constantInstruction("op_define_global", chunk, offset),
        .get_global => return constantInstruction("op_get_global", chunk, offset),
        .set_global => return constantInstruction("op_set_global", chunk, offset),
        .get_local => return byteInstruction("op_get_local", chunk, offset),
        .set_local => return byteInstruction("op_set_local", chunk, offset),
        .get_upvalue => return byteInstruction("op_get_upvalue", chunk, offset),
        .set_upvalue => return byteInstruction("op_set_upvalue", chunk, offset),
        .jump_if_false => return jumpInstruction("op_jump_if_false", .pos, chunk, offset),
        .jump => return jumpInstruction("op_jump", .pos, chunk, offset),
        .loop => return jumpInstruction("op_loop", .neg, chunk, offset),
        .call => return byteInstruction("op_call", chunk, offset),
        .closure => return closureInstruction("op_closure", chunk, offset),
        .close_upvalue => return simpleInstruction("op_close_upvalue", offset),
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
    std.debug.print("{s: <16} \"{f}\" '{d}'\n", .{ name, constant, constant_idx });
    return offset + 2;
}

fn closureInstruction(name: []const u8, chunk: *Chunk, offset: usize) usize {
    var new_offset: usize = offset + 1;
    const constant = chunk.code.items[new_offset];
    new_offset += 1;
    std.debug.print("\"{s}\" {d} '{d}'\n", .{ name, constant, chunk.code.items[constant] });

    // Disassemble upvalues
    const func = chunk.constants.items[constant].asObj().as(Obj.Function);
    var i: usize = 0;
    while (i < func.upvalue_count) : (i += 1) {
        const is_local = chunk.code.items[new_offset] != 1;
        const value_t = if (is_local) "local" else "upvalue";
        new_offset += 1;
        const index = chunk.code.items[new_offset];
        new_offset += 1;
        std.debug.print("       {d} | {s} '{d}'\n", .{ new_offset - 2, value_t, index });
    }

    return new_offset;
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
