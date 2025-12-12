const std = @import("std");
const VM = @import("vm.zig");
const GCAllocator = @import("memory.zig").GCAllocator;
const Allocator = std.mem.Allocator;
const Writer = std.Io.Writer;
const Reader = std.Io.Reader;
const Io = std.Io;

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer _ = arena.deinit();
    const arena_allocator = arena.allocator();

    var gc = GCAllocator.init(arena_allocator);
    const allocator = gc.allocator();

    var vm: *VM = try .init(allocator);
    defer vm.deinit();

    gc.vm = vm;

    var args = std.process.args();
    const name = args.next() orelse "zlox";

    var out_buf: [4096]u8 = undefined;
    var in_buf: [4096]u8 = undefined;
    var stdin = std.fs.File.stdin().reader(&in_buf);
    var stdout = std.fs.File.stdout().writer(&out_buf);
    const writer = &stdout.interface;

    switch (args.inner.count) {
        1 => try repl(arena_allocator, vm, writer, &stdin.interface),
        2 => try runFile(arena_allocator, vm, args.next().?),
        else => {
            std.debug.print("Usage: {s} zloxc [path]\n", .{name});
            return std.process.exit(64);
        },
    }

    try writer.flush();
}

fn repl(allocator: Allocator, vm: *VM, writer: *Writer, reader: *Reader) !void {
    _ = try writer.write("--- Welcome to the zig lox repl. ---\n");

    var allocating_writer = std.Io.Writer.Allocating.init(allocator);
    defer allocating_writer.deinit();

    while (true) {
        _ = try writer.print("> ", .{});
        try writer.flush();

        _ = try reader.streamDelimiter(&allocating_writer.writer, '\n');
        const code = allocating_writer.written();
        allocating_writer.clearRetainingCapacity();
        reader.toss(1);

        vm.interpret(code) catch |err| {
            std.debug.print("{}\n", .{err});
        };
    }
}

fn runFile(allocator: Allocator, vm: *VM, filename: []const u8) !void {
    const source = try readFile(allocator, filename);
    defer allocator.free(source);

    try vm.interpret(source);
}

fn readFile(allocator: Allocator, path: []const u8) ![]const u8 {
    const file = std.fs.cwd().openFile(path, .{}) catch |err| {
        std.debug.print("Could not open file \"{s}\". error {any}\n", .{ path, err });
        std.process.exit(74);
    };
    defer file.close();

    const file_stat = try file.stat();

    return file.readToEndAlloc(allocator, @intCast(file_stat.size)) catch |err| {
        std.debug.print("Could not read file \"{s}\", error: {any}\n", .{ path, err });
        std.process.exit(74);
    };
}
