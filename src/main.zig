const std = @import("std");
const VM = @import("vm.zig");
const GCAllocator = @import("memory.zig").GCAllocator;

const errout = std.io.getStdErr().writer();
const stdout = std.io.getStdOut().writer();
const stdin = std.io.getStdIn().reader();

pub fn main() !u8 {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer _ = arena.deinit();
    const arena_allocator = arena.allocator();

    var gc = GCAllocator.init(arena_allocator);
    const allocator = gc.allocator();

    var vm = try VM.init(allocator);
    defer vm.deinit();

    gc.vm = vm;

    var args = std.process.args();
    const name = args.next() orelse "zlox";

    switch (args.inner.count) {
        1 => try repl(vm),
        2 => try runFile(vm, args.next().?, arena_allocator),
        else => {
            errout.print("Usage: {s} zloxc [path]\n", .{name}) catch {};
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

        vm.interpret(line) catch {
            continue;
        };
    }
}

fn runFile(vm: *VM, filename: []const u8, allocator: std.mem.Allocator) !void {
    const source = try readFile(filename, allocator);
    defer allocator.free(source);

    try vm.interpret(source);
}

fn readFile(path: []const u8, allocator: std.mem.Allocator) ![]const u8 {
    const file = std.fs.cwd().openFile(path, .{}) catch |err| {
        errout.print("Could not open file \"{s}\". error {any}\n", .{ path, err }) catch {};
        std.process.exit(74);
    };
    defer file.close();

    const file_stat: std.fs.File.Stat = try file.stat();

    return file.readToEndAlloc(allocator, @intCast(file_stat.size)) catch |err| {
        errout.print("Could not read file \"{s}\", error: {any}\n", .{ path, err }) catch {};
        std.process.exit(74);
    };
}
