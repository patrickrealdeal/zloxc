const std = @import("std");
const Allocator = std.mem.Allocator;
const Chunk = @import("./chunk.zig").Chunk;
const OpCode = @import("./chunk.zig").OpCode;
const Value = @import("./value.zig").Value;
const Stack = @import("./stack.zig").Stack;
const Compiler = @import("./compiler.zig");
const Obj = @import("./object.zig").Obj;
const Table = @import("./table.zig").Table;

const STACK_MAX = 256;

pub const InterpretResult = enum {
    INTERPRET_OK,
    INTERPRET_COMPILE_ERROR,
    INTERPRET_RUNTIME_ERROR,
};

pub const RuntimeError = error{RuntimeError};

pub const VM = struct {
    chunk: Chunk,
    ip: usize, // instruction pointer
    stack: Stack(Value),
    allocator: Allocator,
    objects: ?*Obj, // tracks heap allocated Objs
    strings: Table,

    pub fn create() VM {
        return VM{
            .chunk = undefined,
            .ip = undefined,
            .stack = undefined,
            .allocator = undefined,
            .objects = null,
            .strings = undefined,
        };
    }

    pub fn init(self: *VM, allocator: Allocator) !void {
        self.allocator = allocator;
        self.ip = 0;
        self.stack = try Stack(Value).init(allocator, STACK_MAX);
        self.chunk = Chunk.init(allocator);
        self.strings = Table.init(allocator);
    }

    pub fn deinit(self: *VM) void {
        self.stack.deinit();
        self.chunk.deinit();
        self.strings.deinit();
        self.freeObjects();
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
            if (opCode == .RETURN and self.stack.items.len == 0) break;
        }

        return .INTERPRET_OK;
    }

    fn runOp(self: *VM, opCode: OpCode) !void {
        switch (opCode) {
            .ADD, .SUBTRACT, .MULTIPLY, .DIVIDE => |op| try self.runBinaryOp(op),
            .NEGATE => {
                if (!self.peek(0).isNumber()) {
                    return self.runtimeError("Operand must be a number.", .{});
                }
                const val = self.pop().asNumber();
                self.push(Value.fromNumber(-val));
            },
            .CONSTANT => {
                const constant = self.readByte();
                const value = self.chunk.constants.items[constant];
                self.push(value);
            },
            .NIL => self.push(Value.nil()),
            .TRUE => self.push(Value.fromBool(true)),
            .FALSE => self.push(Value.fromBool(false)),
            .NOT => {
                const value = Value.fromBool(self.pop().isFalsey());
                self.push(value);
            },
            .EQUAL => {
                const b = self.pop();
                const a = self.pop();
                self.push(Value.fromBool(a.equals(b)));
            },
            .GREATER, .LESS => |o| try self.runBinaryComparison(o),
            .RETURN => {
                const result = self.pop();
                std.debug.print("RESULT: {s}\n", .{result});
                if (self.stack.items.len == 0) return;
                self.push(result);
            },
        }
    }

    fn peek(self: *VM, distance: usize) Value {
        return self.stack.items[self.stack.items.len - 1 - distance];
    }

    pub fn freeObjects(self: *VM) void {
        var object = self.objects;
        while (object) |o| {
            const next = o.next;
            o.destroy(self);
            object = next;
        }
    }

    fn runtimeError(self: *VM, comptime format: []const u8, args: anytype) RuntimeError {
        std.debug.print(format, args);
        std.debug.print("\n", .{});

        var i = self.stack.items.len;
        const line = self.chunk.lines.items[i];
        std.debug.print("[line {d}] in script.\n", .{line});
        self.resetStack();

        return error.RuntimeError;
    }
    fn runBinaryOp(self: *VM, op: OpCode) !void {
        if (self.peek(1).isObjType(.String) or self.peek(0).isObjType(.String)) {
            try self.concatenate();
        } else if (self.peek(1).isNumber() and self.peek(0).isNumber()) {
            const rhs = self.pop().asNumber();
            const lhs = self.pop().asNumber();

            const number = switch (op) {
                .ADD => lhs + rhs,
                .SUBTRACT => lhs - rhs,
                .MULTIPLY => lhs * rhs,
                .DIVIDE => lhs / rhs,
                else => unreachable,
            };

            self.push(Value.fromNumber(number));
        } else {
            return self.runtimeError("Operands must be numbers", .{});
        }
    }
    fn runBinaryComparison(self: *VM, op: OpCode) !void {
        if (!self.peek(1).isNumber() or !self.peek(0).isNumber()) {
            return self.runtimeError("Operands must be numbers", .{});
        }

        const rhs = self.pop().asNumber();
        const lhs = self.pop().asNumber();

        const result = switch (op) {
            .LESS => lhs < rhs,
            //.LESS_EQUAL => lhs <= rhs,
            .GREATER => lhs > rhs,
            // .GREATER_EQUAL => lhs >= rhs,
            else => unreachable,
        };

        self.push(Value.fromBool(result));
    }

    pub fn concatenate(self: *VM) !void {
        const a = self.peek(1).asObjType(.String);
        const b = self.peek(0).asObjType(.String);

        const length = a.bytes.len + b.bytes.len;
        var bytes = try self.allocator.alloc(u8, length);

        std.mem.copy(u8, bytes[0..a.bytes.len], a.bytes);
        std.mem.copy(u8, bytes[a.bytes.len..], b.bytes);

        const result = try Obj.String.take(self, bytes);

        _ = self.pop();
        _ = self.pop();

        self.push(result.obj.value());
    }

    fn resetStack(self: *VM) void {
        self.stack.resize(0) catch unreachable;
    }

    fn readByte(self: *VM) u8 {
        const byte = self.chunk.code.items[self.ip];
        self.ip += 1;
        return byte;
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
