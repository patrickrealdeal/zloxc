const std = @import("std");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);
    const args1 = args[1..];

    if (args1.len > 1) {
        std.debug.print("Usage: zloxi [script]\n", .{});
        std.os.exit(64);
    } else if (args1.len == 1) {
        std.debug.print("file read\n", .{});
        try runFile(args1[0]);
    } else {
        try runPrompt();
    }
}

fn runFile(path: []const u8) !void {
    var file = try std.fs.cwd().openFile(path, .{});
    defer file.close();

    var buf: [1024 * 4]u8 = undefined;
    try file.seekTo(0);
    _ = try file.read(&buf);

    try run(buf[0..]);
}

fn runPrompt() !void {
    while (true) {
        std.debug.print("> ", .{});
        var line = try readInput();
        if (std.mem.eql(u8, line, "")) {
            break;
        }
        try run(line);
    }
}

fn run(input: []u8) !void {
    _ = input;
    //const tokens = scanner.scanTokens();

    //for (tokens) |token| {
    //    std.debug.print("{s}\n", .{token});
    //}
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
