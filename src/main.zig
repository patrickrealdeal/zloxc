const std = @import("std");
const Chunk = @import("./chunk.zig").Chunk;
const OpCode = @import("./chunk.zig").OpCode;
const Value = @import("./value.zig").Value;

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    defer _ = gpa.deinit();

    var chunk = Chunk.init(allocator);
    defer chunk.deinit();

    try chunk.write(@intFromEnum(OpCode.OP_RETURN));
    const val = Value{ .Number = 1.2 };
    const val1 = Value{ .Number = 23 };
    const constant0 = try chunk.addConstant(val);
    const constant1 = try chunk.addConstant(val1);
    try chunk.write(@intFromEnum(OpCode.OP_CONSTANT));
    try chunk.write(@intFromEnum(OpCode.OP_CONSTANT));
    try chunk.write(constant0);
    try chunk.write(constant1);
    try chunk.disassemble("test chunk");
}
