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
    closure: *Obj.Closure,
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
        return &self.closure.function.chunk;
    }
};

pub const VM = struct {
    chunk: Chunk,
    stack: Stack(Value),
    allocator: Allocator,
    objects: ?*Obj, // tracks heap allocated Objs
    strings: Table,
    globals: Table,
    frames: std.ArrayList(CallFrame),
    openUpvalues: ?*Obj.Upvalue,

    pub fn create() VM {
        return VM{
            .chunk = undefined,
            .stack = undefined,
            .allocator = undefined,
            .objects = null,
            .strings = undefined,
            .globals = undefined,
            .frames = undefined,
            .openUpvalues = null,
        };
    }

    pub fn init(self: *VM, allocator: Allocator) !void {
        self.allocator = allocator;
        self.stack = try Stack(Value).init(allocator, STACK_MAX);
        self.chunk = Chunk.init(allocator);
        self.strings = Table.init(allocator);
        self.globals = Table.init(allocator);
        self.frames = std.ArrayList(CallFrame).init(allocator);

        try self.defineNative("clock", clock);
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
        errdefer self.resetStack();

        const func = Compiler.compile(self, source) catch {
            return .INTERPRET_COMPILE_ERROR;
        };

        self.push(func.obj.value());
        const closure = try Obj.Closure.create(self, func);
        _ = self.pop();

        self.push(closure.obj.value());
        try self.callValue(closure.obj.value(), 0);

        if (debug.trace_parser) {
            std.debug.print("STACK: {any}\n", .{self.stack.items});
        }

        const result = self.run();
        _ = self.pop();
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
            .ADD => {
                self.printDebug();
                const rhs = self.pop();
                const lhs = self.pop();

                if (lhs.isObj() and rhs.isObj()) {
                    try self.concatenate(lhs.asObj(), rhs.asObj());
                } else if (lhs.isNumber() and rhs.isNumber()) {
                    self.push(Value.fromNumber(lhs.asNumber() + rhs.asNumber()));
                } else {
                    return self.runtimeError("Operands must be two numbers or two strings.", .{});
                }
            },
            .SUBTRACT => try self.binaryNumericOp(sub),
            .MULTIPLY => try self.binaryNumericOp(mul),
            .DIVIDE => try self.binaryNumericOp(div),
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
                self.printStack();
            },
            .POP => _ = self.pop(),
            .DEFINE_GLOBAL => {
                const name = self.currentFrame().readString();
                const value = self.peek(0);
                _ = try self.globals.set(name, value);
                _ = self.pop();
                // NOTE: that we don’t pop the value until after we add it to the hash table.
                // That ensures the VM can still find the value if a garbage collection is
                // triggered right in the middle of adding it to the hash table.
                // That’s a distinct possibility since the hash table requires dynamic allocation when it resizes.
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
                    _ = self.globals.delete(name);
                    return self.runtimeError("Undefined variable '{s}'.", .{name.bytes});
                }
            },
            .GET_LOCAL => {
                const slot = self.currentFrame().readByte();
                const local = self.stack.items[self.currentFrame().start + slot];
                self.push(local);
            },
            .SET_LOCAL => {
                const slot = self.currentFrame().readByte();
                self.stack.items[self.currentFrame().start + slot] = self.peek(0);
            },
            .GetUpvalue => {
                const slot = self.currentFrame().readByte();
                self.push(self.currentFrame().closure.upvalues[slot].?.location.*);
            },
            .SetUpvalue => {
                const slot = self.currentFrame().readByte();
                self.currentFrame().closure.upvalues[slot].?.location.* = self.peek(0);
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
                self.printStack();
                const arg_count = self.currentFrame().readByte();
                std.debug.print("ARG_COUNT: {}\n", .{arg_count});
                try self.callValue(self.peek(arg_count), arg_count);
            },
            .CLOSURE => {
                self.printStack();
                const function = self.currentFrame().readConstant().asObj().asFunction();
                var closure = try Obj.Closure.create(self, function);
                self.push(closure.obj.value());
                for (closure.upvalues) |*u| {
                    const isLocal = self.currentFrame().readByte() != 0;
                    const index = self.currentFrame().readByte();
                    if (isLocal) {
                        u.* = try self.captureUpvalue(&self.stack.items[self.currentFrame().start + index]);
                    } else {
                        u.* = self.currentFrame().closure.upvalues[index];
                    }
                }
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

    fn captureUpvalue(self: *VM, local: *Value) !*Obj.Upvalue {
        if (self.openUpvalues) |o| {
            var prevUpvalue: ?*Obj.Upvalue = null;
            var upvalue: ?*Obj.Upvalue = o;
            while (upvalue) |u| {
                if (@intFromPtr(u.location) > @intFromPtr(local)) {
                    prevUpvalue = u;
                    upvalue = u.next;
                } else break;
            }

            if (upvalue) |u| if (u.location == local) return u;

            var createdUpvalue = try Obj.Upvalue.create(self, local, upvalue);
            if (prevUpvalue) |p| {
                p.next = createdUpvalue;
            } else {
                self.openUpvalues = createdUpvalue;
            }

            return createdUpvalue;
        } else {
            self.openUpvalues = try Obj.Upvalue.create(self, local, null);
            return self.openUpvalues.?;
        }
    }

    fn callValue(self: *VM, callee: Value, arg_count: u8) !void {
        if (!callee.isObj()) return self.runtimeError("Can only call functions and classes.", .{});
        const o = callee.asObj();
        switch (o.objType) {
            .Closure => try self.call(o.asClosure(), arg_count),
            .Native => try self.callNative(o.asNative(), arg_count),
            .String, .Upvalue, .Function => unreachable,
        }
    }

    fn callNative(self: *VM, native: *Obj.Native, arg_count: u8) !void {
        const result = try native.function(self, self.tail(arg_count));
        self.stack.items.len -= arg_count + 1;
        self.push(result);
        return;
    }

    fn defineNative(self: *VM, name: []const u8, function: *const Obj.Native.Fn) !void {
        const string = try Obj.String.copy(self, name);
        const native = try Obj.Native.create(self, string, function);
        self.push(string.obj.value());
        self.push(native.obj.value());
        _ = try self.globals.set(string, self.peek(0));
        _ = self.pop();
        _ = self.pop();
    }

    fn clockNative(self: *VM, args: []const Value) error{RuntimeError}!Value {
        _ = self;
        _ = args;
        return Value.fromNumber(@as(f64, @floatFromInt(std.time.milliTimestamp())) / 1000);
    }

    extern fn now() f64;

    const clock = clockNative;

    fn call(self: *VM, closure: *Obj.Closure, arg_count: u8) !void {
        if (arg_count != closure.function.arity) {
            return self.runtimeError("Expected {} arguments but got {}.", .{
                closure.function.arity,
                arg_count,
            });
        }

        if (self.frames.items.len == 255) {
            return self.runtimeError("STACK OVERFLOW.", .{});
        }

        try self.frames.append(CallFrame{
            .closure = closure,
            .ip = closure.function.chunk.ptr(),
            .start = @as(u32, @intCast(self.stack.items.len)) - arg_count - 1,
        });
    }

    fn currentFrame(self: *VM) *CallFrame {
        return &self.frames.items[self.frames.items.len - 1];
    }

    fn peek(self: *VM, distance: usize) Value {
        return self.stack.items[self.stack.items.len - 1 - distance];
    }

    pub fn tail(self: *VM, distance: u32) []Value {
        return self.stack.items[self.stack.items.len - 1 - distance ..];
    }

    pub fn freeObjects(self: *VM) void {
        var object = self.objects;
        while (object) |o| {
            const next = o.next;
            o.destroy(self);
            object = next;
        }
    }

    fn runtimeError(self: *VM, comptime format: []const u8, args: anytype) !void {
        std.debug.print(format, args);
        std.debug.print("\n", .{});

        while (self.frames.items.len > 0) {
            const frame = self.frames.pop();
            const function = frame.closure.function;
            const line = function.chunk.lines.items[@intFromPtr(frame.ip) - 1];
            const name = if (function.name) |str| str.bytes else "<script>";
            std.debug.print("[line {}] in {s}\n", .{ line, name });
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

    fn binaryNumericOp(self: *VM, comptime op: anytype) !void {
        const rhs = self.pop();
        const lhs = self.pop();
        if (!lhs.isNumber() or !rhs.isNumber()) {
            return self.runtimeError("Operands must be numbers.", .{});
        }
        self.push(Value.fromNumber(op(lhs.asNumber(), rhs.asNumber())));
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

    fn concatenate(self: *VM, lhs: *Obj, rhs: *Obj) !void {
        switch (lhs.objType) {
            .Function, .Native, .Closure, .Upvalue => {
                try self.runtimeError("Operands must be strings.", .{});
            },
            .String => switch (rhs.objType) {
                .Function, .Native, .Closure, .Upvalue => {
                    try self.runtimeError("Operands must be strings.", .{});
                },
                .String => {
                    // Temporarily put the strings back on the stack so
                    // they're visible to the GC when we allocate
                    self.push(lhs.value());
                    self.push(rhs.value());
                    const lhsStr = lhs.asString();
                    const rhsStr = rhs.asString();
                    const buffer = try self.allocator.alloc(u8, lhsStr.bytes.len + rhsStr.bytes.len);
                    std.mem.copy(u8, buffer[0..lhsStr.bytes.len], lhsStr.bytes);
                    std.mem.copy(u8, buffer[lhsStr.bytes.len..], rhsStr.bytes);
                    _ = self.pop();
                    _ = self.pop();
                    self.push((try Obj.String.copy(self, buffer)).obj.value());
                },
            },
        }
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
        for (self.stack.items, 0..) |v, i| std.debug.print("[ {}: {} ]", .{ i, v });
        std.debug.print("\n", .{});
    }

    fn printDebug(self: *VM) void {
        const frame = self.currentFrame();
        const chunk = frame.currentChunk();
        const instruction = @intFromPtr(frame.ip) - @intFromPtr(chunk.ptr());
        self.printStack();
        _ = chunk.disassembleInstruction(instruction);
        std.debug.print("\n", .{});
    }
};

fn add(x: f64, y: f64) f64 {
    return x + y;
}

fn sub(x: f64, y: f64) f64 {
    return x - y;
}

fn mul(x: f64, y: f64) f64 {
    return x * y;
}

fn div(x: f64, y: f64) f64 {
    return x / y;
}
