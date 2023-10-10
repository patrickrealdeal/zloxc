const std = @import("std");
const Chunk = @import("./chunk.zig").Chunk;
const OpCode = @import("./chunk.zig").OpCode;

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    defer _ = gpa.deinit();

    var chunk = Chunk.init(allocator);
    defer chunk.deinit();

    try chunk.write(@enumToInt(OpCode.OP_RETURN));
}
