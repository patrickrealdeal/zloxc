const std = @import("std");
const io = std.io;
const Errors = @import("/.vm.zig").Errors;
const Allocator = std.mem.Allocator;
const Chunk = @import("./chunk.zig").Chunk;
const OpCode = @import("./chunk.zig").OpCode;
const Value = @import("./value.zig").Value;
const VM = @import("./vm.zig").VM;

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    defer _ = gpa.deinit();

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    switch (args.len) {
        1 => try repl(allocator),
        2 => try runFile(allocator, args[1]),
        else => {
            const stderr = io.getStdErr().writer();
            try stderr.print("Usage: zloxc [path]\n", .{});
            std.process.exit(64);
        },
    }

    // var chunk = Chunk.init(allocator);
    // defer chunk.deinit();
}

fn repl(allocator: Allocator) !void {
    const stderr = io.getStdErr().writer();
    const stdin = io.getStdIn();

    var vm = VM.create();
    try vm.init(allocator);
    defer vm.deinit();

    var buf: [256]u8 = undefined;
    while (true) {
        try stderr.print("> ", .{});
        const input = try stdin.read(&buf);
        if (input == buf.len) {
            try stderr.print("Input too long.\n", .{});
            continue;
        }
        const source = buf[0..input];
        _ = source;
        // vm.interpret(source);
    }
}

fn runFile(allocator: Allocator, path: []const u8) !void {
    var vm = VM.create();
    try vm.init(allocator);
    defer vm.deinit();

    const source = try std.fs.cwd().readFileAlloc(allocator, path, 1_000_000);
    defer allocator.free(source);
    const result = try vm.interpret(source);
    switch (result) {
        .INTERPRET_OK => return,
        .INTERPRET_COMPILE_ERROR => std.process.exit(65),
        .INTERPRET_RUNTIME_ERROR => std.process.exit(70),
    }
}
