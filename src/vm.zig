const std = @import("std");
const Allocator = std.mem.Allocator;
const Chunk = @import("./chunk.zig").Chunk;
const OpCode = @import("./chunk.zig").OpCode;
const Value = @import("./value.zig").Value;
const Stack = @import("./stack.zig").Stack;
const Compiler = @import("./compiler.zig");
const Obj = @import("./object.zig").Obj;
const Table = @import("./table.zig").Table;
const debug = @import("./debug.zig");

const STACK_MAX = 256;

pub const InterpretResult = enum {
    INTERPRET_OK,
    INTERPRET_COMPILE_ERROR,
    INTERPRET_RUNTIME_ERROR,
};

pub const RuntimeError = error{RuntimeError};

pub const CallFrame = struct {
    function: *Obj.Function,
    ip: [*]u8,
    start: u32,

    pub fn readByte(self: *CallFrame) u8 {
        const byte = self.ip[0];
        self.ip += 1;
        return byte;
    }

    pub fn readShort(self: *CallFrame) u16 {
        const value = @as(u16, @intCast(self.ip[0])) << 8 | self.ip[1];
        self.ip += 2;
        return value;
    }

    pub fn readConstant(self: *CallFrame) Value {
        const chunk = self.currentChunk();
        return chunk.constants.items[self.readByte()];
    }

    pub fn readString(self: *CallFrame) *Obj.String {
        const chunk = self.currentChunk();
        return chunk.constants.items[self.readByte()].asObjType(.String);
    }

    pub fn currentChunk(self: CallFrame) *Chunk {
        return &self.function.chunk;
    }
};

pub const VM = struct {
    chunk: Chunk,
    ip: usize, // instruction pointer
    stack: Stack(Value),
    allocator: Allocator,
    objects: ?*Obj, // tracks heap allocated Objs
    strings: Table,
    globals: Table,
    frames: std.ArrayList(CallFrame),

    pub fn create() VM {
        return VM{
            .chunk = undefined,
            .ip = undefined,
            .stack = undefined,
            .allocator = undefined,
            .objects = null,
            .strings = undefined,
            .globals = undefined,
            .frames = undefined,
        };
    }

    pub fn init(self: *VM, allocator: Allocator) !void {
        self.allocator = allocator;
        self.ip = 0;
        self.stack = try Stack(Value).init(allocator, STACK_MAX);
        self.chunk = Chunk.init(allocator);
        self.strings = Table.init(allocator);
        self.globals = Table.init(allocator);
        self.frames = std.ArrayList(CallFrame).init(allocator);
    }

    pub fn deinit(self: *VM) void {
        self.stack.deinit();
        self.chunk.deinit();
        self.strings.deinit();
        self.globals.deinit();
        self.frames.deinit();
        self.freeObjects();
    }

    pub fn interpret(self: *VM, source: []const u8) !InterpretResult {
        // Assert the Stack is empty at beginning and end
        std.debug.assert(self.stack.items.len == 0);
        defer std.debug.assert(self.stack.items.len == 0);
        const func = Compiler.compile(self, source) catch {
            return .INTERPRET_COMPILE_ERROR;
        };

        self.push(func.obj.value());
        try self.call(func, 0);

        _ = self.pop();

        if (debug.trace_parser) {
            std.debug.print("STACK: {any}\n", .{self.stack.items});
        }

        const result = self.run();

        return result;
    }

    fn run(self: *VM) InterpretResult {
        while (true) {
            const instruction = self.currentFrame().readByte();
            const opCode: OpCode = @enumFromInt(instruction);
            self.runOp(opCode) catch |err| {
                if (err == error.RuntimeError) return .INTERPRET_RUNTIME_ERROR;
            };
            if (opCode == .RETURN and self.frames.items.len == 0) break;
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
                const constant = self.currentFrame().readConstant();
                self.push(constant);
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
            .PRINT => {
                const stdout = std.io.getStdOut().writer();
                try stdout.print("{any}\n", .{self.pop()});
            },
            .POP => _ = self.pop(),
            .DEFINE_GLOBAL => {
                const name = self.currentFrame().readString();
                const value = self.peek(0);
                _ = try self.globals.set(name, value);
                // NOTE: that we don’t pop the value until after we add it to the hash table.
                // That ensures the VM can still find the value if a garbage collection is
                // triggered right in the middle of adding it to the hash table.
                // That’s a distinct possibility since the hash table requires dynamic allocation when it resizes.
                _ = self.pop();
            },
            .GET_GLOBAL => {
                const name = self.currentFrame().readString();
                var value: Value = undefined;
                if (!self.globals.get(name, &value)) {
                    return self.runtimeError("Undefined variable '{s}'.", .{name.bytes});
                }

                self.push(value);
            },
            .SET_GLOBAL => {
                const name = self.currentFrame().readString();
                if (try self.globals.set(name, self.peek(0))) {
                    _ = try self.globals.delete(name);
                    return self.runtimeError("Undefined variable '{s}'.", .{name.bytes});
                }
            },
            .GET_LOCAL => {
                const slot = self.currentFrame().readByte();
                self.push(self.stack.items[self.currentFrame().start + slot]);
            },
            .SET_LOCAL => {
                const slot = self.currentFrame().readByte();
                self.stack.items[self.currentFrame().start + slot] = self.peek(0);
            },
            .JUMP => {
                const offset = self.currentFrame().readShort();
                self.currentFrame().ip += offset;
            },
            .JUMP_IF_FALSE => {
                const offset = self.currentFrame().readShort();
                if (self.peek(0).isFalsey()) self.currentFrame().ip += offset;
            },
            .LOOP => {
                const offset = self.currentFrame().readShort();
                self.currentFrame().ip -= offset;
            },
            .CALL => {
                const arg_count = self.currentFrame().readByte();
                std.debug.print("ARG_COUNT: {}\n", .{arg_count});
                try self.callValue(self.peek(arg_count), arg_count);
            },
            .RETURN => {
                const result = self.pop();
                const frame = self.frames.pop();

                if (self.frames.items.len == 0) return;

                try self.stack.resize(frame.start);
                self.push(result);
            },
        }
    }

    fn callValue(self: *VM, callee: Value, arg_count: u8) !void {
        if (!callee.isObj()) return self.runtimeError("Can only call functions and classes.", .{});
        const obj = callee.asObj();
        switch (obj.objType) {
            .Function => try self.call(obj.asFunction(), arg_count),
            .String => return self.runtimeError("Can only call functions and classes.", .{}),
        }
    }

    fn call(self: *VM, func: *Obj.Function, arg_count: u8) !void {
        if (arg_count != func.arity) {
            return self.runtimeError("Expected {} arguments but got {}.", .{
                func.arity,
                arg_count,
            });
        }

        if (self.frames.items.len == 255) {
            return self.runtimeError("STACK OVERFLOW.", .{});
        }

        try self.frames.append(CallFrame{
            .function = func,
            .ip = func.chunk.ptr(),
            .start = @as(u32, @intCast(self.stack.items.len)) - arg_count - 1,
        });
    }

    fn currentFrame(self: *VM) *CallFrame {
        return &self.frames.items[self.frames.items.len - 1];
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

        var i = self.frames.items.len;
        while (i != 0) {
            i -= 1;
            const frame = &self.frames.items[i];
            const function = frame.function;
            const line = function.chunk.lines.items[i];
            const name = if (function.name) |str| str.bytes else "<script>";
            std.debug.print("[line {d}] in {s}\n", .{ line, name });
        }
        self.resetStack();

        return error.RuntimeError;
    }

    fn resetStack(self: *VM) void {
        self.stack.resize(0) catch unreachable;
    }

    fn runBinaryBool(self: *VM, op: OpCode) !void {
        if (!self.peek(1).isBool() or !self.peek(0).isBool()) {
            return self.runtimeError("Operands must be boolean", .{});
        }
        const rhs = self.pop().asBool();
        const lhs = self.pop().asBool();
        const result = switch (op) {
            .AND => lhs and rhs,
            .OR => lhs or rhs,
            else => unreachable,
        };
        self.push(Value.fromBool(result));
    }

    fn runBinaryOp(self: *VM, op: OpCode) !void {
        if (self.peek(1).isObjType(.String) and self.peek(0).isObjType(.String)) {
            // TODO: Disallow other operators for string concat.
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
            .GREATER => lhs > rhs,
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
