const std = @import("std");

const errout = std.io.getStdErr().writer();
const stdout = std.io.getStdOut().writer();
const stdin = std.io.getStdIn().reader();

pub fn main() !u8 {
    const args = try std.process.argsAlloc(std.heap.page_allocator);

    switch (args.len) {
        1 => repl(),
        2 => runFile(args[1], std.heap.page_allocator),
        else => {
            errout.print("Usage zloxc [path]\n", .{}) catch {};
            return std.process.exit(64);
        },
    }

    return 0;
}

fn repl() void {
    var buf = std.io.bufferedReader(stdin);
    var reader = buf.reader();
    var line_buf: [1024]u8 = undefined;

    while (true) {
        stdout.writeAll("> ") catch std.debug.panic("Couldn't write to stdout!\n", .{});
        var line = reader.readUntilDelimiterOrEof(&line_buf, '\n') catch {
            std.debug.panic("Couldn't read from stdin!\n", .{});
        } orelse {
            stdout.writeAll("\n") catch std.debug.panic("Couldn't write to stdout!\n", .{});
            break;
        };
        std.debug.print("{s}\n", .{line});
    }
}

fn runFile(filename: []const u8, allocator: std.mem.Allocator) void {
    const source = readFile(filename, allocator);
    defer allocator.free(source);

    std.debug.print("FILE CONTENTS: {s}\n", .{source});
}

fn readFile(path: []const u8, allocator: std.mem.Allocator) []const u8 {
    const file = std.fs.cwd().openFile(path, .{ .read = true }) catch |err| {
        errout.print("Could not open file \"{s}\". error {any}\n", .{ path, err }) catch {};
        std.process.exit(74);
    };
    defer file.close();

    return file.readToEndAlloc(allocator, 100000) catch |err| {
        errout.print("Could not read file \"{s}\", error: {any}\n", .{ path, err }) catch {};
        std.process.exit(74);
    };
}
