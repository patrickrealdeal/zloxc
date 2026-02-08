const std = @import("std");
const VM = @import("vm.zig");
const GCAllocator = @import("memory.zig").GCAllocator;
const Allocator = std.mem.Allocator;

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer _ = arena.deinit();
    const arena_allocator = arena.allocator();

    var dba: std.heap.DebugAllocator(.{}) = .init;
    defer std.debug.assert(dba.deinit() == .ok);
    //const debug_allocator = dba.allocator();

    var gc = GCAllocator.init(arena_allocator);
    const allocator = gc.allocator();

    var vm: *VM = try .init(allocator);
    defer vm.deinit();

    gc.vm = vm;

    var args = std.process.args();
    const name = args.next() orelse "zlox";

    switch (args.inner.count) {
        1 => try repl(arena_allocator, vm),
        2 => try runFile(arena_allocator, vm, args.next().?),
        3 => {
            const flag = args.next().?;
            const filename = args.next().?;
            if (std.mem.eql(u8, flag, "--benchmark") or std.mem.eql(u8, flag, "-b")) {
                try benchmarkFile(arena_allocator, vm, filename);
            } else if (std.mem.eql(u8, flag, "--compile") or std.mem.eql(u8, flag, "-c")) {
                try compileFile(arena_allocator, vm, filename);
            } else if (std.mem.eql(u8, flag, "--execute") or std.mem.eql(u8, flag, "-e")) {
                try executeFile(arena_allocator, vm, filename);
            } else {
                std.debug.print("Unknown flag: {s}\n", .{flag});
                printUsage(name);
                return std.process.exit(64);
            }
        },
        else => {
            printUsage(name);
            return std.process.exit(64);
        },
    }
}

fn printUsage(name: []const u8) void {
    std.debug.print("Usage:\n", .{});
    std.debug.print("  {s} [source.lox]                    - Run Lox source file\n", .{name});
    std.debug.print("  {s} --compile|-c [source.lox]      - Compile to bytecode (.zloxc)\n", .{name});
    std.debug.print("  {s} --execute|-e [bytecode.zloxc]  - Execute bytecode file\n", .{name});
    std.debug.print("  {s} --benchmark|-b [source.lox]    - Benchmark (separate compile/run)\n", .{name});
    std.debug.print("  {s}                                 - Start REPL\n", .{name});
}

fn repl(_: Allocator, vm: *VM) !void {
    // Allocate buffers for stdout and stdin
    var stdout_buffer: [4096]u8 = undefined;
    var stdout_writer = std.fs.File.stdout().writer(&stdout_buffer);
    const stdout: *std.Io.Writer = &stdout_writer.interface;

    var stdin_buffer: [4096]u8 = undefined;
    var stdin_reader = std.fs.File.stdin().reader(&stdin_buffer);
    const stdin: *std.Io.Reader = &stdin_reader.interface;

    try stdout.writeAll("--- Welcome to the zig lox repl. ---\n");
    try stdout.flush();

    // Enable REPL mode for persistent type checking
    vm.is_repl = true;

    while (true) {
        try stdout.writeAll("> ");
        try stdout.flush();

        // Use takeDelimiterExclusive to read a line
        if (stdin.takeDelimiterExclusive('\n')) |line| {
            // Toss the delimiter
            stdin.toss(1);

            vm.interpret(line) catch |err| {
                std.debug.print("{}\n", .{err});
            };
        } else |err| switch (err) {
            error.EndOfStream => break,
            error.StreamTooLong => {
                std.debug.print("Line too long\n", .{});
                // Skip until we find the newline delimiter
                stdin.toss(stdin_buffer.len);
            },
            else => return err,
        }
    }
}

fn runFile(allocator: Allocator, vm: *VM, filename: []const u8) !void {
    // Ensure REPL mode is disabled for file execution
    vm.is_repl = false;

    const source = try readFile(allocator, filename);
    defer allocator.free(source);

    try vm.interpret(source);
}

fn benchmarkFile(allocator: Allocator, vm: *VM, filename: []const u8) !void {
    // Ensure REPL mode is disabled for file execution
    vm.is_repl = false;

    const source = try readFile(allocator, filename);
    defer allocator.free(source);

    // Compile the program first (this is timed separately)
    const compile_start = std.time.milliTimestamp();
    const function = try @import("compiler.zig").compile(source, vm);
    const compile_end = std.time.milliTimestamp();
    const compile_time = compile_end - compile_start;

    if (function) |func| {
        // Print compilation time
        std.debug.print("Compilation time: {}ms\n", .{compile_time});
        std.debug.print("Starting execution...\n\n", .{});

        // Now execute the compiled code (user's clock() calls measure this)
        vm.push(@import("value.zig").Value.fromObj(&func.obj));

        const closure = try @import("object.zig").Obj.Closure.allocate(vm, func);
        _ = vm.pop();
        vm.push(@import("value.zig").Value.fromObj(&closure.obj));

        try vm.call(closure, 0);
        try vm.run();

        std.debug.print("\n--- Execution complete ---\n", .{});
    } else {
        return error.VmCompileError;
    }
}

fn compileFile(allocator: Allocator, vm: *VM, filename: []const u8) !void {
    vm.is_repl = false;

    const source = try readFile(allocator, filename);
    defer allocator.free(source);

    std.debug.print("Compiling {s}...\n", .{filename});

    const compile_start = std.time.milliTimestamp();
    const function = try @import("compiler.zig").compile(source, vm);
    const compile_end = std.time.milliTimestamp();

    if (function) |func| {
        // Generate output filename: source.lox -> source.zloxc
        const output_path = try std.fmt.allocPrint(allocator, "{s}c", .{filename});
        defer allocator.free(output_path);

        std.debug.print("Writing bytecode to {s}...\n", .{output_path});

        var serializer = @import("bytecode.zig").BytecodeSerializer.init(allocator);
        try serializer.serialize(func, output_path);

        const compile_time = compile_end - compile_start;
        std.debug.print("Compilation complete in {}ms\n", .{compile_time});
        std.debug.print("Bytecode saved to {s}\n", .{output_path});
    } else {
        return error.VmCompileError;
    }
}

fn executeFile(allocator: Allocator, vm: *VM, filename: []const u8) !void {
    vm.is_repl = false;

    std.debug.print("Loading bytecode from {s}...\n", .{filename});

    var serializer = @import("bytecode.zig").BytecodeSerializer.init(allocator);
    const function = try serializer.deserialize(vm, filename);

    std.debug.print("Executing...\n\n", .{});

    const exec_start = std.time.milliTimestamp();

    // Set up and execute the function
    vm.push(@import("value.zig").Value.fromObj(&function.obj));
    const closure = try @import("object.zig").Obj.Closure.allocate(vm, function);
    _ = vm.pop();
    vm.push(@import("value.zig").Value.fromObj(&closure.obj));

    try vm.call(closure, 0);
    try vm.run();

    const exec_end = std.time.milliTimestamp();

    std.debug.print("\n--- Execution complete in {}ms ---\n", .{exec_end - exec_start});
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
