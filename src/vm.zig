const std = @import("std");
const Allocator = std.mem.Allocator;

const Chunk = @import("chunk.zig");
const OpCode = Chunk.OpCode;
const compiler = @import("compiler.zig");
const debug = @import("debug.zig");
const FixedCapacityStack = @import("stack.zig").FixedCapacityStack;
const GCAllocator = @import("memory.zig").GCAllocator;
const Obj = @import("object.zig");
const NativeFn = Obj.Native.NativeFn;
const Parser = @import("compiler.zig").Parser;
const Table = @import("table.zig");
const TypeChecker = @import("typechecker.zig").TypeChecker;
const Value = @import("value.zig").Value;
const Type = @import("type.zig").Type;

const u8_max = std.math.maxInt(u8) + 1;

const stack_max = frames_max * u8_max;
const frames_max = 128;

const VM = @This();

ip: usize,
frames: [frames_max]CallFrame,
frame: *CallFrame,
frame_count: usize,
stack: FixedCapacityStack(Value),
allocator: Allocator,
objects: ?*Obj,
open_upvalues: ?*Obj.Upvalue, // the head pointer goes right inside the main VM struct
strings: Table.StringTable,
globals: Table.GlobalsTable,
gray_stack: std.ArrayList(*Obj),
parser: ?*Parser,
type_checker: TypeChecker, // Persistent type checker for REPL
is_repl: bool, // Track if we're in REPL mode

const CallFrame = struct {
    closure: *Obj.Closure,
    ip: usize,
    slot_offset: usize,

    pub fn init() CallFrame {
        return .{
            .closure = undefined,
            .ip = 0,
            .slot_offset = 0,
        };
    }
};

inline fn resetStack(vm: *VM) void {
    vm.open_upvalues = null;
    vm.frame_count = 0;
}

pub fn init(gc_allocator: Allocator) !*VM {
    const static = struct {
        var frames: [frames_max]CallFrame = [1]CallFrame{CallFrame.init()} ** frames_max;
    };

    const gc: *GCAllocator = @ptrCast(@alignCast(gc_allocator.ptr));
    var vm = try gc.parent_allocator.create(VM);

    vm.* = .{
        .stack = try .init(gc.parent_allocator, stack_max),
        .strings = Table.createStringTable(gc_allocator),
        .globals = Table.createGlobalsTable(gc_allocator),
        .gray_stack = .empty,
        .ip = 0,
        .allocator = gc_allocator,
        .frames = static.frames,
        .frame = undefined,
        .frame_count = 0,
        .objects = null,
        .open_upvalues = null,
        .parser = null,
        .type_checker = TypeChecker.init(gc_allocator),
        .is_repl = false,
    };

    try vm.defineNative("clock", clockNative);
    try vm.defineNative("sqrt", sqrtNative);
    try vm.defineNative("cos", cosNative);
    try vm.defineNative("sin", sinNative);
    try vm.defineNative("str", strNative);

    // Register native functions with type checker
    try vm.type_checker.registerNative("clock", &[_]Type{}, Type.float); // clock() -> float
    try vm.type_checker.registerNative("sqrt", &[_]Type{Type.float}, Type.float); // sqrt(float) -> float
    try vm.type_checker.registerNative("cos", &[_]Type{Type.float}, Type.float); // cos(float) -> float
    try vm.type_checker.registerNative("sin", &[_]Type{Type.float}, Type.float); // sin(float) -> float
    try vm.type_checker.registerNative("str", &[_]Type{Type.float}, Type.string); // str(float) -> string

    return vm;
}

pub fn deinit(vm: *VM) void {
    if (comptime debug.log_gc) {
        std.debug.print("-----------------\nUninitializing VM\n", .{});
    }
    vm.freeObjects();
    vm.strings.deinit();
    vm.globals.deinit();
    vm.stack.deinit();
    vm.type_checker.deinit();
}

pub fn interpret(vm: *VM, source: []const u8) !void {
    std.debug.assert(vm.stack.items.len == 0);
    defer vm.stack.resize(0) catch {};

    if (try compiler.compile(source, vm)) |func| {
        vm.push(Value.fromObj(&func.obj));

        const closure = try Obj.Closure.allocate(vm, func);
        _ = vm.pop();
        vm.push(Value.fromObj(&closure.obj));

        try vm.call(closure, 0);

        try vm.run();
        return;
    }
    return error.VmCompileError;
}

pub fn run(vm: *VM) !void {
    vm.frame = &vm.frames[vm.frame_count - 1];

    // SUPER OPTIMIZATION: Cache hot pointers to reduce indirection
    // This is what Ruby/Lua do - keep IP and code in local variables
    var frame = vm.frame;
    var ip = frame.ip;
    var code = frame.closure.func.chunk.code.items;

    while (true) {
        if (comptime debug.trace_execution) {
            frame.ip = ip; // Write back for debugging
            _ = Chunk.disassembleInstruction(&frame.closure.func.chunk, ip);
        }

        if (comptime debug.trace_stack) {
            vm.traceStackExecution();
        }

        // OPTIMIZATION: Direct pointer access instead of method call
        const instruction: OpCode = @enumFromInt(code[ip]);
        ip += 1;

        switch (instruction) {
            .constant => {
                const idx = code[ip];
                ip += 1;
                const constant = frame.closure.func.chunk.constants.items[idx];
                vm.push(constant);
            },
            .negate => {
                if (!Value.isNumber(vm.peek(0))) {
                    frame.ip = ip;
                    try vm.runtimeErr("Operand must be a number!", .{});
                }
                const val_ptr = &vm.stack.items[vm.stack.items.len - 1];
                val_ptr.* = Value.fromNumber(-val_ptr.asNumber());
            },
            .add => {
                frame.ip = ip;
                try vm.binaryOp(.add);
                ip = frame.ip;
            },
            .sub => {
                frame.ip = ip;
                try vm.binaryOp(.sub);
                ip = frame.ip;
            },
            .mul => {
                frame.ip = ip;
                try vm.binaryOp(.mul);
                ip = frame.ip;
            },
            .div => {
                frame.ip = ip;
                try vm.binaryOp(.div);
                ip = frame.ip;
            },
            .true => vm.push(Value.fromBool(true)),
            .false => vm.push(Value.fromBool(false)),
            .nil => vm.push(Value.fromNil()),
            .not => vm.push(Value.fromBool(isFalsey(vm.pop()))),
            .equal => {
                const b = vm.pop();
                const a = vm.pop();
                vm.push(Value.fromBool(Value.eq(a, b)));
            },
            .greater => {
                frame.ip = ip;
                try vm.binaryOp(.gt);
                ip = frame.ip;
            },
            .less => {
                frame.ip = ip;
                try vm.binaryOp(.lt);
                ip = frame.ip;
            },
            .print => {
                std.debug.print("{f}\n", .{vm.pop()});
            },
            .pop => _ = vm.pop(),
            .define_global => {
                const idx = code[ip];
                ip += 1;
                const name = frame.closure.func.chunk.constants.items[idx].asObj().as(Obj.String);
                _ = try vm.globals.set(name, vm.peek(0));
                _ = vm.pop();
            },
            .get_global => {
                const idx = code[ip];
                ip += 1;
                const name = frame.closure.func.chunk.constants.items[idx].asObj().as(Obj.String);
                const value = vm.globals.get(name) orelse {
                    frame.ip = ip;
                    _ = vm.pop();
                    try vm.runtimeErr("Undefined variable {s}.\n", .{name.bytes});
                    return error.VmUndefinedVariable;
                };
                vm.push(value);
            },
            .set_global => {
                const idx = code[ip];
                ip += 1;
                const name = frame.closure.func.chunk.constants.items[idx].asObj().as(Obj.String);
                if (try vm.globals.set(name, vm.peek(0))) {
                    _ = vm.globals.delete(name);
                    frame.ip = ip;
                    try vm.runtimeErr("Undefined variable {s}.\n", .{name.bytes});
                    return error.VmUndefinedVariable;
                }
            },
            .get_local => {
                const slot = code[ip];
                ip += 1;
                vm.push(vm.stack.items[frame.slot_offset + slot]);
            },
            .set_local => {
                const slot = code[ip];
                ip += 1;
                vm.stack.items[frame.slot_offset + slot] = vm.peek(0);
            },
            .get_upvalue => {
                const slot = code[ip];
                ip += 1;
                vm.push(frame.closure.upvalues[slot].?.location.*);
            },
            .set_upvalue => {
                const slot = code[ip];
                ip += 1;
                frame.closure.upvalues[slot].?.location.* = vm.peek(0);
            },
            .close_upvalue => {
                vm.closeUpvalues(&vm.stack.items[vm.stack.items.len - 1]);
                _ = vm.pop();
            },
            .jump_if_false => {
                const byte1: u16 = code[ip];
                const byte2: u16 = code[ip + 1];
                ip += 2;
                const offset = (byte1 << 8) | byte2;
                if (isFalsey(vm.peek(0))) ip += offset;
            },
            .jump => {
                const byte1: u16 = code[ip];
                const byte2: u16 = code[ip + 1];
                ip += 2;
                const offset = (byte1 << 8) | byte2;
                ip += offset;
            },
            .loop => {
                const byte1: u16 = code[ip];
                const byte2: u16 = code[ip + 1];
                ip += 2;
                const offset = (byte1 << 8) | byte2;
                ip -= offset;
            },
            .call => {
                const arg_count = code[ip];
                ip += 1;
                frame.ip = ip;
                try vm.callValue(vm.peek(arg_count), arg_count);
                // Reload all cached pointers after call
                frame = &vm.frames[vm.frame_count - 1];
                ip = frame.ip;
                code = frame.closure.func.chunk.code.items;
            },
            .class => {
                const idx = code[ip];
                ip += 1;
                const name = frame.closure.func.chunk.constants.items[idx].asObj().as(Obj.String);
                const klass = try Obj.Class.allocate(vm, name);
                vm.push(Value.fromObj(&klass.obj));
            },
            .closure => {
                const idx = code[ip];
                ip += 1;
                const func = frame.closure.func.chunk.constants.items[idx].asObj().as(Obj.Function);
                const closure = try Obj.Closure.allocate(vm, func);
                vm.push(Value.fromObj(&closure.obj));

                for (closure.upvalues) |*upvalue| {
                    const is_local = code[ip] != 0;
                    ip += 1;
                    const index = code[ip];
                    ip += 1;
                    if (is_local) {
                        upvalue.* = try vm.captureUpvalue(&vm.stack.items[frame.slot_offset + index]);
                    } else {
                        upvalue.* = frame.closure.upvalues[index];
                    }
                }
            },
            .type_of => {
                const value = vm.peek(0);
                frame.ip = ip;
                const type_str = try vm.getTypeString(value);
                _ = vm.pop();
                vm.push(.fromObj(&type_str.obj));
                ip = frame.ip;
            },
            .ret => {
                const result = vm.pop();
                vm.frame_count -= 1;

                if (vm.frame_count == 0) {
                    _ = vm.pop();
                    return;
                }

                vm.stack.items.len = frame.slot_offset;
                vm.push(result);
                vm.closeUpvalues(&vm.stack.items[frame.slot_offset]);
                // Reload all cached pointers after return
                frame = &vm.frames[vm.frame_count - 1];
                ip = frame.ip;
                code = frame.closure.func.chunk.code.items;
            },
        }
    }
}

fn getTypeString(self: *VM, value: Value) !*Obj.String {
    const type_name: []const u8 = if (value.isNumber()) blk: {
        // Check if it's an int or float based on the value
        const num = value.asNumber();
        if (@floor(num) == num and num >= std.math.minInt(i32) and num <= std.math.maxInt(i32)) {
            break :blk "int";
        } else {
            break :blk "float";
        }
    } else if (value.isBool()) blk: {
        break :blk "bool";
    } else if (value.isNil()) blk: {
        break :blk "nil";
    } else if (value.isObj()) blk: {
        const obj = value.asObj();
        break :blk switch (obj.obj_t) {
            .string => "string",
            .function => "function",
            .closure => "function",
            .upvalue => "upvalue",
            .class => "class",
            else => "unkwonk",
            //.instance => "instance",
            //.bound_method => "method",
        };
    } else blk: {
        break :blk "unknown";
    };

    return try Obj.String.copy(self, type_name);
}

inline fn readByte(vm: *VM) u8 {
    const byte = vm.frame.closure.func.chunk.code.items[vm.frame.ip];
    vm.frame.ip += 1;
    return byte;
}

inline fn readU16(vm: *VM) u16 {
    const byte1: u16 = vm.frame.closure.func.chunk.code.items[vm.frame.ip];
    const byte2 = vm.frame.closure.func.chunk.code.items[vm.frame.ip + 1];
    vm.frame.ip += 2;
    return (byte1 << 8) | byte2;
}

inline fn readString(vm: *VM) *Obj.String {
    return vm.readConstant().asObj().as(Obj.String);
}

inline fn readConstant(vm: *VM) Value {
    return vm.frame.closure.func.chunk.constants.items[vm.readByte()];
}

pub inline fn push(vm: *VM, value: Value) void {
    vm.stack.append(value);
}

pub inline fn pop(vm: *VM) Value {
    return vm.stack.pop();
}

fn stackTop(vm: *VM) !usize {
    return vm.stack.items.len;
}

inline fn isFalsey(value: Value) bool {
    return value.isFalsey();
}

fn concat(vm: *VM) !void {
    const b = vm.pop().asObj().as(Obj.String);
    const a = vm.pop().asObj().as(Obj.String);
    const res = try std.mem.concat(vm.allocator, u8, &[_][]const u8{ a.bytes, b.bytes });
    const str = try Obj.String.take(vm, res);
    vm.push(Value.fromObj(&str.obj));
}

const BinaryOp = enum { add, sub, mul, div, gt, lt };

inline fn binaryOp(vm: *VM, op: BinaryOp) !void {
    const b = vm.peek(0);
    const a = vm.peek(1);

    // Fast path: both numbers (most common case in fib)
    if (a.isNumber() and b.isNumber()) {
        const left = a.asNumber();
        const right = b.asNumber();
        _ = vm.pop();
        _ = vm.pop();

        const result = switch (op) {
            .add => Value.fromNumber(left + right),
            .sub => Value.fromNumber(left - right),
            .mul => Value.fromNumber(left * right),
            .div => Value.fromNumber(left / right),
            .gt => Value.fromBool(left > right),
            .lt => Value.fromBool(left < right),
        };

        vm.push(result);
    } else if (op == .add and a.isObj() and a.asObj().obj_t == .string and b.isObj() and b.asObj().obj_t == .string) {
        try vm.concat();
    } else {
        try vm.runtimeErr("Operands must be two numbers or strings [{f},  {f}]", .{ vm.peek(0), vm.peek(1) });
        return error.VmRuntimeError;
    }
}

inline fn peek(vm: *VM, distance: usize) Value {
    return vm.stack.items[vm.stack.items.len - distance - 1];
}

pub fn call(vm: *VM, closure: *Obj.Closure, arg_count: usize) !void {
    if (arg_count != closure.func.arity) {
        return vm.runtimeErr("Expected {d} arguments but got: {d}", .{ closure.func.arity, arg_count });
    }

    if (vm.frame_count == frames_max) {
        try vm.runtimeErr("Stack Overflow.", .{});
    }

    // Add a new call frame
    const frame: *CallFrame = &vm.frames[vm.frame_count];
    frame.closure = closure;
    frame.ip = 0;
    frame.slot_offset = vm.stack.items.len - arg_count - 1;
    vm.frame_count += 1;
}

fn callValue(vm: *VM, callee: Value, arg_count: usize) !void {
    if (callee.isObj()) {
        const obj = callee.asObj();
        switch (obj.obj_t) {
            .native => {
                const native = obj.as(Obj.Native).func;
                const result = try native(vm, @truncate(arg_count));
                vm.stack.items.len -= arg_count + 1;
                vm.push(result);
            },
            .closure => return vm.call(obj.as(Obj.Closure), arg_count),
            else => try vm.runtimeErr("Can only call functions.", .{}),
        }
    } else try vm.runtimeErr("Can only call functions.", .{});
}

fn captureUpvalue(vm: *VM, local: *Value) !*Obj.Upvalue {
    if (vm.open_upvalues) |oup| {
        var prev_upvalue: ?*Obj.Upvalue = null;
        var upvalue: ?*Obj.Upvalue = oup;
        while (upvalue) |uv| {
            if (@intFromPtr(uv.location) > @intFromPtr(local)) {
                prev_upvalue = uv;
                upvalue = uv.next;
            } else break;
        }

        if (upvalue) |uv| if (uv.location == local) return uv;

        const created_upvalue = try Obj.Upvalue.allocate(vm, local);
        if (prev_upvalue) |puv| {
            puv.next = created_upvalue;
        } else {
            vm.open_upvalues = created_upvalue;
        }

        return created_upvalue;
    } else {
        vm.open_upvalues = try Obj.Upvalue.allocate(vm, local);
        return vm.open_upvalues.?;
    }
}

fn closeUpvalues(vm: *VM, last: *Value) void {
    while (vm.open_upvalues) |uv| {
        if (@intFromPtr(uv.location) >= @intFromPtr(last)) {
            uv.closed = uv.location.*;
            uv.location = &uv.closed;
            vm.open_upvalues = uv.next;
        } else {
            return;
        }
    }
}

inline fn traceStackExecution(vm: *VM) void {
    const print = std.debug.print;
    print("          ", .{});
    for (vm.stack.items) |value| {
        print("[{}]", .{value});
    }
    print("\n", .{});
}

fn runtimeErr(vm: *VM, comptime fmt: []const u8, args: anytype) !void {
    //const errWriter = std.io.getStdErr().writer();
    var stderr = std.fs.File.stderr().writer(&.{});
    const err_writer = &stderr.interface;

    try err_writer.print("ERROR: " ++ fmt ++ "\n", args);

    while (vm.frame_count > 0) {
        const frame = vm.frame;
        const func = frame.closure.func;
        const line = func.chunk.lines.items[frame.ip];
        const name = if (func.name) |name| name.bytes else "<script>";
        try err_writer.print("[line {d}] in {s}.\n", .{ line, name });
        vm.frame_count -= 1;
        _ = vm.pop();
        try err_writer.flush();
    }
}

fn defineNative(vm: *VM, name: []const u8, func: NativeFn) !void {
    const str = try Obj.String.copy(vm, name);
    vm.push(Value.fromObj(&str.obj)); // push on the stack to avoid GCAllocator
    const native = try Obj.Native.allocate(vm, func, name);
    const native_val: Value = Value.fromObj(&native.obj);
    vm.push(native_val);

    //try str.obj.mark(vm);
    const name_str = vm.peek(1).asObj().as(Obj.String);
    _ = try vm.globals.set(name_str, vm.peek(0));
    _ = vm.pop();
    _ = vm.pop();
}

fn freeObjects(vm: *VM) void {
    var obj = vm.objects;
    var total_objects: u64 = 0;
    while (obj) |object| {
        if (comptime debug.log_gc) {
            total_objects += 1;
        }
        const next = object.next;
        object.destroy(vm);
        obj = next;
    }

    if (comptime debug.log_gc) {
        std.debug.print("Objects freed {d}\n", .{total_objects});
    }
}

fn clockNative(vm: *VM, arg_count: u8) !Value {
    _ = arg_count;
    _ = vm;

    const mills = std.time.milliTimestamp();
    const seconds = @as(f64, @floatFromInt(mills)) / @as(f64, @floatFromInt(std.time.ms_per_s));

    return Value.fromNumber(seconds);
}

fn sqrtNative(vm: *VM, arg_count: u8) !Value {
    _ = arg_count;

    const val = vm.peek(0);
    if (!Value.isNumber(val)) {
        try vm.runtimeErr("ERROR: sqrt parameter must be a number!\n", .{});
        return error.VmRuntimeError;
    }

    // sqrt in place to avoid pop and push codes
    return Value.fromNumber(@sqrt(val.asNumber()));
}

fn cosNative(vm: *VM, arg_count: u8) !Value {
    _ = arg_count;
    const val = vm.peek(0);
    if (!Value.isNumber(val)) {
        try vm.runtimeErr("ERROR: cos parameter must be a number!\n", .{});
        return error.VmRuntimeError;
    }

    return Value.fromNumber(@cos(val.asNumber()));
}

fn sinNative(vm: *VM, arg_count: u8) !Value {
    _ = arg_count;
    const val = vm.peek(0);
    if (!Value.isNumber(val)) {
        try vm.runtimeErr("ERROR: cos parameter must be a number!\n", .{});
        return error.VmRuntimeError;
    }

    return Value.fromNumber(@sin(val.asNumber()));
}

fn strNative(vm: *VM, arg_count: u8) !Value {
    if (arg_count != 1) {
        return error.WrongArgumentCount;
    }

    const val = vm.peek(0);

    // Use the VM's allocator to format the value into a new string.
    const str_bytes = try std.fmt.allocPrint(vm.allocator, "{f}", .{val});

    // Take ownership of the allocated bytes and create a new Obj.String.
    // This will be managed by the GC.
    const str_obj = try Obj.String.take(vm, str_bytes);

    return Value.fromObj(&str_obj.obj);
}
