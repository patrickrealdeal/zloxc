const std = @import("std");
const VM = @import("vm.zig").VM;

const errout = std.io.getStdErr().writer();
const stdout = std.io.getStdOut().writer();
const stdin = std.io.getStdIn().reader();

pub fn main() !u8 {
    var arena = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = arena.allocator();

    defer _ = arena.deinit();
    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    var vm = VM.init(allocator);
    defer vm.deinit();

    switch (args.len) {
        1 => try repl(&vm),
        2 => try runFile(&vm, args[1], allocator),
        else => {
            errout.print("Usage zloxc [path]\n", .{}) catch {};
            return std.process.exit(64);
        },
    }

    return 0;
}

fn repl(vm: *VM) !void {
    var buf = std.io.bufferedReader(stdin);
    var reader = buf.reader();
    var line_buf: [1024]u8 = undefined;

    while (true) {
        try stdout.writeAll("> ");
        const line = try reader.readUntilDelimiterOrEof(&line_buf, '\n') orelse {
            try stdout.writeAll("\n");
            break;
        };
        if (line.len == 0) continue;

        try vm.interpret(line);
    }
}

fn runFile(vm: *VM, filename: []const u8, allocator: std.mem.Allocator) !void {
    const source = readFile(filename, allocator);
    defer allocator.free(source);

    try vm.interpret(source);
}

fn readFile(path: []const u8, allocator: std.mem.Allocator) []const u8 {
    const file = std.fs.cwd().openFile(path, .{}) catch |err| {
        errout.print("Could not open file \"{s}\". error {any}\n", .{ path, err }) catch {};
        std.process.exit(74);
    };
    defer file.close();

    return file.readToEndAlloc(allocator, 100000) catch |err| {
        errout.print("Could not read file \"{s}\", error: {any}\n", .{ path, err }) catch {};
        std.process.exit(74);
    };
}
