const std = @import("std");
const Chunk = @import("chunk.zig").Chunk;
const OpCode = @import("chunk.zig").OpCode;
const VM = @import("vm.zig").VM;

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var chunk = Chunk.init(allocator);
    defer chunk.deinit();

    var constant: u8 = @intCast(try chunk.addConstant(3.0));
    try chunk.write(@intFromEnum(OpCode.constant), 123);
    try chunk.write(constant, 123);

    constant = @intCast(try chunk.addConstant(1));
    try chunk.write(@intFromEnum(OpCode.constant), 123);
    try chunk.write(constant, 123);

    try chunk.write(@intFromEnum(OpCode.add), 123);

    constant = @intCast(try chunk.addConstant(2));
    try chunk.write(@intFromEnum(OpCode.constant), 123);
    try chunk.write(constant, 123);

    try chunk.write(@intFromEnum(OpCode.div), 123);
    try chunk.write(@intFromEnum(OpCode.negate), 123);

    try chunk.write(@intFromEnum(OpCode.ret), 123);

    var vm = VM.init(&chunk);
    try vm.interpret();
}
