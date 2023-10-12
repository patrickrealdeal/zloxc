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

    var vm = VM.create();
    try vm.init(allocator);
    defer vm.deinit();

    const val0 = Value{ .Number = 1.2 };
    const constant0 = try chunk.addConstant(val0);
    try chunk.writeOp(OpCode.OP_CONSTANT, 123);
    try chunk.write(constant0, 123);

    const val1 = Value{ .Number = 3.4 };
    const constant1 = try chunk.addConstant(val1);
    try chunk.writeOp(OpCode.OP_CONSTANT, 124);
    try chunk.write(constant1, 124);

    try chunk.writeOp(.OP_ADD, 123);

    const val2 = Value{ .Number = 5.6 };
    const constant2 = try chunk.addConstant(val2);
    try chunk.writeOp(OpCode.OP_CONSTANT, 124);
    try chunk.write(constant2, 123);

    try chunk.writeOp(OpCode.OP_DIVIDE, 123);
    try chunk.writeOp(OpCode.OP_NEGATE, 123);
    try chunk.writeOp(OpCode.OP_RETURN, 123);
    // try chunk.disassemble("test chunk");
    const result = vm.interpret(chunk);
    std.debug.print("Result: {s}\n", .{@tagName(result)});
}
