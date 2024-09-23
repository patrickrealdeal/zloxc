const std = @import("std");
const Chunk = @import("chunk.zig").Chunk;
const OpCode = @import("chunk.zig").OpCode;
const Value = @import("value.zig").Value;

const trace_execution = true;
const trace_stack = true;
const stack_max = 256;

pub const VM = struct {
    chunk: *Chunk,
    ip: [*]u8,
    stack: [stack_max]Value,
    stack_top: usize,

    pub fn init(chunk: *Chunk) VM {
        return VM{
            .chunk = chunk,
            .ip = chunk.code.items.ptr,
            .stack = std.mem.zeroes([stack_max]Value),
            .stack_top = 0,
        };
    }

    pub fn interpret(self: *VM) !void {
        try self.run();
    }

    fn run(self: *VM) !void {
        while (true) {
            if (comptime trace_execution) {
                _ = Chunk.disassembleInstruction(self.chunk, @intFromPtr(self.ip - @intFromPtr(self.chunk.code.items.ptr)));
            }

            if (comptime trace_stack) {
                self.traceStackExecution();
            }

            const instruction: OpCode = @enumFromInt(self.readByte());
            switch (instruction) {
                .constant => {
                    const constant = self.readConstant();
                    try self.push(constant);
                },
                .negate => self.stack[self.stack_top - 1] = -self.stack[self.stack_top - 1],
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
