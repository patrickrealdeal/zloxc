const std = @import("std");
const Chunk = @import("chunk.zig");
const Value = @import("value.zig").Value;
const compiler = @import("compiler.zig");
const Obj = @import("object.zig");
const OpCode = Chunk.OpCode;

const u8_max = std.math.maxInt(u8) + 1;

const debug_trace_execution = false;
const debug_trace_stack = false;
const debug_gc = false;
const stack_max = frames_max * u8_max;
const frames_max = 64;

const CallFrame = struct {
    function: *Obj.Function,
    ip: usize,
    slot_offset: usize,

    pub fn init() CallFrame {
        return .{
            .function = undefined,
            .ip = 0,
            .slot_offset = 0,
        };
    }
};

const Self = @This();

//chunk: *Chunk,
ip: usize,
frames: [frames_max]CallFrame,
frame: *CallFrame,
frame_count: usize,
stack: std.ArrayList(Value),
stack_top: usize,
allocator: std.mem.Allocator,
objects: ?*Obj,
strings: std.StringHashMap(*Obj.String),
globals: std.StringHashMap(Value),

pub fn init(allocator: std.mem.Allocator) Self {
    return .{
        // .chunk = undefined,
        .ip = 0,
        .frames = [_]CallFrame{CallFrame.init()} ** frames_max,
        .frame = undefined,
        .frame_count = 0,
        .stack = std.ArrayList(Value).init(allocator),
        .stack_top = 0,
        .allocator = allocator,
        .objects = null,
        .strings = std.StringHashMap(*Obj.String).init(allocator),
        .globals = std.StringHashMap(Value).init(allocator),
    };
}

pub fn deinit(self: *Self) void {
    if (comptime debug_gc) {
        std.debug.print("-----------------\nUninitializing VM\n", .{});
    }
    self.resetStack();
    self.freeObjects();
    self.strings.deinit();
    self.globals.deinit();
    self.stack.deinit();

    for (self.stack.items) |elem| {
        if (@typeInfo(@TypeOf(elem)) == .pointer) {
            self.allocator.free(elem);
        }
    }
}

pub fn interpret(self: *Self, source: []const u8) !void {
    std.debug.assert(self.stack.items.len == 0);
    defer std.debug.assert(self.stack.items.len == 0);

    const func = try compiler.compile(source, self);
    try self.push(Value{ .obj = &func.obj });
    try self.call(func, 0);

    try self.run();
}

fn resetStack(self: *Self) void {
    self.stack_top = 0;
}

fn run(self: *Self) !void {
    self.frame = &self.frames[self.frame_count - 1];
    while (true) {
        if (comptime debug_trace_execution) {
            _ = Chunk.disassembleInstruction(self.frame.function.chunk, self.frame.ip);
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
                    try self.runtimeErr("Operand must be a number!", .{});
                }
                self.stack.items[self.stack.items.len - 1].number = -self.stack.items[self.stack.items.len - 1].number;
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
            .print => {
                const writer = std.io.getStdOut().writer();
                try writer.print("{}\n", .{self.pop()});
            },
            .pop => _ = self.pop(),
            .define_global => {
                const name = self.readString();
                try self.globals.put(name.bytes, self.peek(0));
                _ = self.pop();
            },
            .get_global => {
                const name = self.readString();
                const value = self.globals.get(name.bytes) orelse {
                    try self.runtimeErr("Undefined variable {s}.\n", .{name.bytes});
                    return VmError.UndefinedVariable;
                };
                try self.push(value);
            },
            .set_global => {
                const name = self.readString();
                if (!self.globals.contains(name.bytes)) {
                    try self.runtimeErr("Undefined variable {s}.\n", .{name.bytes});
                    return VmError.UndefinedVariable;
                }
                try self.globals.put(name.bytes, self.peek(0));
            },
            .get_local => {
                const slot = self.readByte();
                try self.push(self.stack.items[self.frame.slot_offset + slot]);
            },
            .set_local => {
                const slot = self.readByte();
                self.stack.items[self.frame.slot_offset + slot] = self.peek(0);
            },
            .jump_if_false => {
                const offset = self.readU16();
                if (isFalsey(self.peek(0))) self.frame.ip += offset;
            },
            .jump => {
                const offset = self.readU16();
                self.frame.ip += offset;
            },
            .loop => {
                const offset = self.readU16();
                self.frame.ip -= offset;
            },
            .call => {
                const arg_count = self.readByte();
                try self.callValue(self.peek(arg_count), arg_count);
                self.frame = &self.frames[self.frame_count - 1];
            },
            .ret => {
                const result = self.pop();
                self.frame_count -= 1;

                if (self.frame_count == 0) {
                    _ = self.pop();
                    return;
                }

                self.stack.items.len -= self.stack.items.len - self.frame.slot_offset;
                try self.push(result);
                self.frame = &self.frames[self.frame_count - 1];
            },
        }
    }
}

fn readByte(self: *Self) usize {
    const byte = self.frame.function.chunk.code.items[self.frame.ip];
    self.frame.ip += 1;
    return byte;
}

fn readU16(self: *Self) usize {
    const byte1 = @as(u16, self.frame.function.chunk.code.items[self.frame.ip]);
    const byte2 = self.frame.function.chunk.code.items[self.frame.ip + 1];
    self.frame.ip += 2;
    return (byte1 << 8) | byte2;
}

inline fn readString(self: *Self) *Obj.String {
    return self.readConstant().asObj().asString();
}

fn readConstant(self: *Self) Value {
    return self.frame.function.chunk.constants.items[self.readByte()];
}

fn push(self: *Self, value: Value) !void {
    try self.stack.append(value);
}

fn pop(self: *Self) Value {
    return self.stack.pop();
}

fn stackTop(self: *Self) !usize {
    return self.stack.items.len;
}

fn isFalsey(value: Value) bool {
    return switch (value) {
        .nil => true,
        .bool => |b| !b,
        else => false,
    };
}

fn concat(self: *Self) !void {
    const b = self.pop().asObj().asString();
    const a = self.pop().asObj().asString();
    const res = try std.mem.concat(self.allocator, u8, &[_][]const u8{ a.bytes, b.bytes });
    const str = try Obj.String.take(self, res);
    try self.push(Value{ .obj = &str.obj });
}

const BinaryOp = enum { add, sub, mul, div, gt, lt };

fn binaryOp(self: *Self, op: BinaryOp) !void {
    if (self.peek(0).is(.string) and self.peek(1).is(.string)) {
        try self.concat();
    } else if (self.peek(0).isNumber() and self.peek(1).isNumber()) {
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
    } else {
        try self.runtimeErr("Operands must be two numbers or strings {any} {any}", .{ self.peek(0), self.peek(1) });
        return VmError.RuntimeError;
    }
}

fn peek(self: *Self, distance: usize) Value {
    return self.stack.items[self.stack.items.len - distance - 1];
}

fn call(self: *Self, func: *Obj.Function, arg_count: usize) !void {
    if (arg_count != func.arity) {
        return self.runtimeErr("Expected {d} arguments but got: {d}", .{ func.arity, arg_count });
    }

    if (self.frame_count == frames_max) {
        try self.runtimeErr("Stack Overflow.", .{});
    }

    // Add a new call frame
    const frame: *CallFrame = &self.frames[self.frame_count];
    frame.function = func;
    frame.ip = 0;
    frame.slot_offset = self.stack.items.len - arg_count - 1;
    self.frame_count += 1;
}

fn callValue(self: *Self, callee: Value, arg_count: usize) !void {
    switch (callee) {
        .obj => |obj| {
            switch (obj.obj_t) {
                .function => try self.call(obj.asFunction(), arg_count),
                else => try self.runtimeErr("Can only call functions.", .{}),
            }
        },
        else => try self.runtimeErr("Can only call functions.", .{}),
    }
}

inline fn traceStackExecution(self: *Self) void {
    const print = std.debug.print;
    print("          ", .{});
    for (self.stack.items) |value| {
        print("[{}]", .{value});
    }
    print("\n", .{});
}

fn runtimeErr(self: *Self, comptime fmt: []const u8, args: anytype) !void {
    const errWriter = std.io.getStdErr().writer();
    errWriter.print(fmt ++ "\n", args) catch {};

    while (self.frame_count > 0) {
        const frame = self.frame;
        const func = frame.function;
        const line = func.chunk.lines.items[frame.ip];
        const name = if (func.name) |name| name.bytes else "<script>";
        errWriter.print("[line {d}] in {s}.\n", .{ line, name }) catch {};
        self.frame_count -= 1;
    }

    self.resetStack();
    return VmError.RuntimeError;
}

fn freeObjects(self: *Self) void {
    var obj = self.objects;
    var total_objects: u64 = 0;
    while (obj) |object| {
        if (comptime debug_gc) {
            total_objects += 1;
        }
        const next = object.next;
        object.destroy(self);
        obj = next;
    }

    if (comptime debug_gc) {
        std.debug.print("Objects freed {d}\n", .{total_objects});
    }
}

const VmError = error{
    Ok,
    CompileError,
    UndefinedVariable,
    RuntimeError,
};
