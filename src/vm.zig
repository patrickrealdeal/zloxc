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
    ip: usize,
    stack: [stack_max]Value,
    stack_top: usize,
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator) VM {
        return VM{
            .chunk = undefined,
            .ip = 0,
            .stack = undefined,
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
        self.ip = 0;

        try self.run();
    }

    fn resetStack(self: *VM) void {
        self.stack_top = 0;
    }

    fn run(self: *VM) !void {
        while (true) {
            if (comptime debug_trace_execution) {
                _ = Chunk.disassembleInstruction(self.chunk, self.ip);
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
                .negate => {
                    if (self.peek(0) != .number) {
                        self.runtimeErr("Operand must be a number!", .{});
                    }
                    self.stack[self.stack_top - 1].number = -self.stack[self.stack_top - 1].number;
                },
                .add => try self.binaryOp(.add),
                .sub => try self.binaryOp(.sub),
                .mul => try self.binaryOp(.mul),
                .div => try self.binaryOp(.div),
                .true => try self.push(Value{ .bool = true }),
                .false => try self.push(Value{ .bool = false }),
                .nil => try self.push(Value.nil),
                .not => try self.push(Value{ .bool = isFalsey(self.pop()) }),
                .equal => {
                    const b = self.pop();
                    const a = self.pop();
                    try self.push(Value{ .bool = Value.eq(a, b) });
                },
                .greater => try self.binaryOp(.gt),
                .less => try self.binaryOp(.lt),
                .ret => {
                    Value.printValue(self.pop());
                    std.debug.print("\n", .{});
                    return;
                },
            }
        }
    }

    fn readByte(self: *VM) usize {
        const byte = self.chunk.code.items[self.ip];
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

    fn isFalsey(value: Value) bool {
        return switch (value) {
            .nil => true,
            .bool => |b| !b,
            else => false,
        };
    }

    const BinaryOp = enum { add, sub, mul, div, gt, lt };

    fn binaryOp(self: *VM, op: BinaryOp) !void {
        if (self.peek(0) != .number or self.peek(1) != .number) {
            self.runtimeErr("Operands must be numbers", .{});
            return InterpretError.RuntimeError;
        }

        const b = self.pop().number;
        const a = self.pop().number;
        const result = switch (op) {
            .add => Value{ .number = a + b },
            .sub => Value{ .number = a - b },
            .mul => Value{ .number = a * b },
            .div => Value{ .number = a / b },
            .gt => Value{ .bool = a > b },
            .lt => Value{ .bool = a < b },
        };

        try self.push(result);
    }

    fn peek(self: *VM, distance: usize) Value {
        return self.stack[self.stack_top - distance - 1];
    }

    inline fn traceStackExecution(self: *VM) void {
        const print = std.debug.print;
        print("          ", .{});
        for (self.stack) |value| {
            print("[", .{});
            Value.printValue(value);
            print("]", .{});
        }
        print("\n", .{});
    }

    fn runtimeErr(self: *VM, comptime fmt: []const u8, args: anytype) void {
        const errWriter = std.io.getStdErr().writer();
        errWriter.print(fmt ++ "\n", args) catch {};
        errWriter.print("[line {d}] in script.\n", .{self.chunk.lines.items[self.ip]}) catch {};
        self.resetStack();
    }
};

const InterpretError = error{
    Ok,
    CompileError,
    RuntimeError,
};
