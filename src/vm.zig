const std = @import("std");
const Chunk = @import("chunk.zig").Chunk;
const OpCode = @import("chunk.zig").OpCode;
const Value = @import("value.zig").Value;
const compiler = @import("compiler.zig");

const debug_trace_execution = false;
const debug_trace_stack = false;
const stack_max = 256;

pub const VM = struct {
    chunk: *Chunk,
    ip: [*]u8,
    stack: [stack_max]Value,
    stack_top: usize,
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator) VM {
        return VM{
            .chunk = undefined,
            .ip = undefined,
            .stack = std.mem.zeroes([stack_max]Value),
            .stack_top = 0,
            .allocator = allocator,
        };
    }

    pub fn interpret(self: *VM, source: []const u8) !void {
        var chunk = Chunk.init(self.allocator);
        defer chunk.deinit();

        std.debug.print("SOURCE: {s}\n", .{source});
        try compiler.compile(source, &chunk);
        self.chunk = &chunk;
        self.ip = self.chunk.code.items.ptr;

        try self.run();
    }

    fn run(self: *VM) !void {
        while (true) {
            if (comptime debug_trace_execution) {
                _ = Chunk.disassembleInstruction(self.chunk, @intFromPtr(self.ip - @intFromPtr(self.chunk.code.items.ptr)));
            }

            if (comptime debug_trace_stack) {
                self.traceStackExecution();
            }

            const instruction: OpCode = @enumFromInt(self.readByte());
            switch (instruction) {
                .constant => {
                    const constant = self.readConstant();
                    try self.push(constant);
                },
                .negate => self.stack[self.stack_top - 1] = -self.stack[self.stack_top - 1],
                .add => try self.binaryOp(.add),
                .sub => try self.binaryOp(.sub),
                .mul => try self.binaryOp(.mul),
                .div => try self.binaryOp(.div),
                .ret => {
                    std.debug.print("\n{d}\n", .{self.pop()});
                    return;
                },
            }
        }
    }

    fn readByte(self: *VM) usize {
        const byte = self.ip[0];
        self.ip += 1;
        return byte;
    }

    fn readConstant(self: *VM) Value {
        return self.chunk.constants.items[self.readByte()];
    }

    fn push(self: *VM, value: Value) !void {
        self.stack[self.stack_top] = value;
        self.stack_top += 1;
    }

    fn pop(self: *VM) Value {
        self.stack_top -= 1;
        return self.stack[self.stack_top];
    }

    const BinaryOp = enum { add, sub, mul, div };

    fn binaryOp(self: *VM, op: BinaryOp) !void {
        const b = self.pop();
        const a = self.pop();
        const result = switch (op) {
            .add => a + b,
            .sub => a - b,
            .mul => a * b,
            .div => a / b,
        };

        try self.push(result);
    }

    inline fn traceStackExecution(self: *VM) void {
        std.debug.print("       ", .{});
        var i: usize = 0;
        while (i < self.stack_top) : (i += 1) {
            std.debug.print("[ {d} ]", .{self.stack[i]});
        }
        std.debug.print("\n", .{});
    }
};

const InterpretError = error{
    Ok,
    CompileError,
    RuntimeError,
};
