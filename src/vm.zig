const std = @import("std");
const Allocator = std.mem.Allocator;
const Chunk = @import("./chunk.zig").Chunk;
const OpCode = @import("./chunk.zig").OpCode;
const Value = @import("./value.zig").Value;
const Stack = @import("./stack.zig").Stack;
const Compiler = @import("./compiler.zig");

const STACK_MAX = 256;

pub const InterpretResult = enum {
    INTERPRET_OK,
    INTERPRET_COMPILE_ERROR,
    INTERPRET_RUNTIME_ERROR,
};

pub const VM = struct {
    chunk: Chunk,
    ip: usize, // instruction pointe
    stack: Stack(Value),
    allocator: Allocator,

    pub fn create() VM {
        return VM{
            .chunk = undefined,
            .ip = undefined,
            .stack = undefined,
            .allocator = undefined,
        };
    }

    pub fn init(self: *VM, allocator: Allocator) !void {
        self.allocator = allocator;
        self.ip = 0;
        self.stack = try Stack(Value).init(allocator, STACK_MAX);
        self.chunk = Chunk.init(allocator);
    }

    pub fn deinit(self: *VM) void {
        self.stack.deinit();
        self.chunk.deinit();
    }

    pub fn interpret(self: *VM, source: []const u8) InterpretResult {
        // Assert the Stack is empty at beginning and end
        std.debug.assert(self.stack.items.len == 0);
        defer std.debug.assert(self.stack.items.len == 0);

        if (!Compiler.compile(self, source, &self.chunk)) {
            self.chunk.deinit();
            return .INTERPRET_COMPILE_ERROR;
        }

        const result = self.run();
        return result;
    }

    fn run(self: *VM) InterpretResult {
        while (true) {
            const instruction = self.readByte();
            const opCode: OpCode = @enumFromInt(instruction);
            self.runOp(opCode) catch unreachable;
            if (opCode == .OP_RETURN and self.stack.items.len == 0) break;
        }

        return .INTERPRET_OK;
    }

    fn runOp(self: *VM, opCode: OpCode) !void {
        switch (opCode) {
            .OP_ADD => {
                const rhs = self.pop().asNumber();
                const lhs = self.pop().asNumber();
                self.push(Value.fromNumber(lhs + rhs));
            },
            .OP_SUBTRACT => try self.binaryOp(sub),
            .OP_MULTIPLY => try self.binaryOp(mul),
            .OP_DIVIDE => try self.binaryOp(div),
            .OP_NEGATE => {
                const val = self.pop();
                // if (!val.isNumber()) return .INTERPRET_RUNTIME_ERROR;
                self.push(Value.fromNumber(-val.asNumber()));
            },
            .OP_CONSTANT => {
                const constant = self.readByte();
                const value = self.chunk.constants.items[constant];
                self.push(value);
            },
            .OP_RETURN => {
                // self.printStack();
                const result = self.pop();
                std.debug.print("RESULT: {d}\n", .{result.asNumber()});
                if (self.stack.items.len == 0) return;
                self.push(result);
            },
        }
    }

    fn readByte(self: *VM) u8 {
        const byte = self.chunk.code.items[self.ip];
        self.ip += 1;
        return byte;
    }

    fn binaryOp(self: *VM, comptime op: anytype) !void {
        const rhs = self.pop().asNumber();
        const lhs = self.pop().asNumber();
        self.push(Value.fromNumber(op(lhs, rhs)));
    }

    pub fn push(self: *VM, value: Value) void {
        self.stack.append(value);
    }

    pub fn pop(self: *VM) Value {
        return self.stack.pop();
    }

    pub fn printStack(self: *VM) void {
        std.debug.print("--- STACK ---\n", .{});
        std.debug.print("          ", .{});
        for (self.stack.items) |item| {
            std.debug.print("[ {} ]", .{item});
        }
        std.debug.print("\n", .{});
    }
};

fn sub(x: f64, y: f64) f64 {
    return x - y;
}

fn mul(x: f64, y: f64) f64 {
    return x * y;
}

fn div(x: f64, y: f64) f64 {
    return x / y;
}
