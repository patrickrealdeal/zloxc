const std = @import("std");
const Chunk = @import("./chunk.zig").Chunk;
const OpCode = @import("./chunk.zig").OpCode;
const Value = @import("./value.zig").Value;
const VM = @import("./vm.zig").VM;

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    defer _ = gpa.deinit();

    var chunk = Chunk.init(allocator);
    defer chunk.deinit();

    const vm = VM.init();
    defer vm.deinit();

    const val = Value{ .Number = 1.2 };
    const val1 = Value{ .Number = 23 };
    const val2 = Value{ .Number = 12319 };
    const constant0 = try chunk.addConstant(val);
    const constant1 = try chunk.addConstant(val1);
    const constant2 = try chunk.addConstant(val2);
    try chunk.writeOp(OpCode.OP_CONSTANT, 124);
    try chunk.write(constant0, 124);
    try chunk.writeOp(OpCode.OP_CONSTANT, 125);
    try chunk.write(constant1, 125);
    try chunk.writeOp(OpCode.OP_CONSTANT, 126);
    try chunk.write(constant2, 126);
    try chunk.writeOp(OpCode.OP_RETURN, 127);
    try chunk.writeOp(OpCode.OP_RETURN, 128);
    try chunk.disassemble("test chunk");
    try vm.interpret(chunk);
}
