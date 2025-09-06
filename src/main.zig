const std = @import("std");
const VM = @import("vm.zig");
const GCAllocator = @import("memory.zig").GCAllocator;

pub fn main() !u8 {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer _ = arena.deinit();
    const arena_allocator = arena.allocator();

    var gc = GCAllocator.init(arena_allocator);
    const allocator = gc.allocator();

    var vm: *VM = undefined;
    vm = try VM.init(allocator);
    defer vm.deinit();

    gc.vm = vm;

    var args = std.process.args();
    const name = args.next() orelse "zlox";

    var out_buf: [1024]u8 = undefined;
    var in_buf: [1024]u8 = undefined;
    var stdin = std.fs.File.stdin().reader(&in_buf);
    var stdout = std.fs.File.stdout().writer(&out_buf);
    const reader = &stdin.interface;
    const writer = &stdout.interface;

    switch (args.inner.count) {
        1 => {
            try repl(vm, writer, reader);
        },
        2 => {
            try runFile(vm, args.next().?, arena_allocator);
        },
        else => {
            std.debug.print("Usage: {s} zloxc [path]\n", .{name});
            return std.process.exit(64);
        },
    }

    return 0;
}

fn repl(vm: *VM, writer: *std.Io.Writer, reader: *std.Io.Reader) !void {
    _ = try writer.write("--- Welcome to the zig lox repl. ---\n");
    while (true) {
        _ = try writer.write("> ");
        try writer.flush();
        const input = try reader.takeDelimiterExclusive('\n');
        if (reader.buffer.len == 0) continue;

        vm.interpret(input) catch {
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
        std.debug.print("Could not open file \"{s}\". error {any}\n", .{ path, err });
        std.process.exit(74);
    };
    defer file.close();

    const file_stat: std.fs.File.Stat = try file.stat();

    return file.readToEndAlloc(allocator, @intCast(file_stat.size)) catch |err| {
        std.debug.print("Could not read file \"{s}\", error: {any}\n", .{ path, err });
        std.process.exit(74);
    };
}
