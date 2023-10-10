const std = @import("std");
const Allocator = std.mem.Allocator;
const Scanner = @import("./scanner.zig").Scanner;
const Token = @import("./scanner.zig").Token;
const TokenType = @import("./scanner.zig").TokenType;

var hadError: bool = false;

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    switch (args.len) {
        1 => try runPrompt(allocator),
        2 => {
            try runFile(allocator, args[1]);
            std.debug.print("File Read!\n", .{});
        },
        else => {
            const stderr = std.io.getStdErr().writer();
            try stderr.print("Usage: lox [path]\n", .{});
            std.process.exit(64);
        },
    }
}

fn runFile(allocator: Allocator, path: []const u8) !void {
    var source = try std.fs.cwd().readFileAlloc(allocator, path, 1_000_000);
    defer allocator.free(source);

    try run(allocator, source);

    if (hadError) std.process.exit(65);
}

fn runPrompt(allocator: Allocator) !void {
    while (true) {
        std.debug.print("> ", .{});
        var line = try readInput();
        if (std.mem.eql(u8, line, "")) {
            break;
        }
        try run(allocator, line);
        hadError = false;
    }
}

fn run(allocator: Allocator, source: []u8) !void {
    var scanner = Scanner.init(source);
    var token: Token = undefined;
    while (!scanner.isAtEnd()) {
        token = scanner.scanToken();
        const stoken = try token.toString(allocator);
        defer allocator.free(stoken);
        std.debug.print("{s}\n", .{stoken});
    }
}

fn _error(line: u32, message: []u8) void {
    report(line, "", message);
}

fn report(line: u32, where: []u8, message: []u8) void {
    std.debug.print("[line {d}] Error {s}: {s}\n", .{ line, where, message });
}

fn readInput() ![]u8 {
    const in = std.io.getStdIn();
    var buf = std.io.bufferedReader(in.reader());

    // Get the Reader interface from BufferedReader
    var r = buf.reader();

    std.debug.print("Write something: ", .{});
    // Ideally we would want to issue more than one read
    // otherwise there is no point in buffering.
    var msg_buf: [4096]u8 = undefined;
    var msg = try r.readUntilDelimiterOrEof(&msg_buf, '\n');

    var line: []u8 = undefined;
    if (msg) |m| {
        std.debug.print("msg: {s}\n", .{m});
        line = m;
        return line;
    }

    return "";
}
