const std = @import("std");
const VM = @import("vm.zig");
const Value = @import("value.zig").Value;
const Chunk = @import("chunk.zig");
const hashFn = std.hash.Fnv1a_32.hash;
const debug = @import("debug.zig");

const Obj = @This();
pub const ObjType = enum {
    string,
    function,
    native,
    closure,
    upvalue,
};

obj_t: ObjType,
next: ?*Obj,
is_marked: bool,
next_gray: ?*Obj,

pub fn create(vm: *VM, comptime T: type, obj_t: ObjType) !*T {
    const ptr_t = try vm.allocator.create(T);
    ptr_t.obj = Obj{
        .obj_t = obj_t,
        .next = vm.objects,
        .is_marked = false,
        .next_gray = null,
    };

    if (comptime debug.log_gc) std.debug.print("{*} allocate {} for {s}\n", .{ &ptr_t.obj, @sizeOf(T), @typeName(T) });

    vm.objects = &ptr_t.obj;
    return ptr_t;
}

pub fn as(self: *Obj, comptime T: type) *T {
    return @alignCast(@fieldParentPtr("obj", self));
}

pub fn is(self: *Obj, obj_t: ObjType) bool {
    return self.obj_t == obj_t;
}

pub fn mark(self: *Obj, vm: *VM) !void {
    if (self.is_marked) return;
    if (comptime debug.log_gc) std.debug.print("{*} mark {}\n", .{ self, Value{ .obj = self } });
    self.is_marked = true;
    try vm.gray_stack.append(self);
}

pub fn blacken(self: *Obj, vm: *VM) !void {
    switch (self.obj_t) {
        .native, .string => return,
        .upvalue => try self.as(Upvalue).closed.mark(vm),
        .function => {
            const function = self.as(Function);
            if (function.name) |name| {
                try name.obj.mark(vm);
            }
            try function.markConstants(vm);
        },
        .closure => {
            const closure = self.as(Closure);
            try closure.func.obj.mark(vm);
            for (0..closure.func.upvalue_count) |i| {
                if (closure.upvalues[i]) |upvalue|
                    try upvalue.obj.mark(vm);
            }
        },
    }
    if (comptime debug.log_gc) std.debug.print("{*} blacken {}\n", .{ self, Value{ .obj = self } });
}

pub fn destroy(obj: *Obj, vm: *VM) void {
    switch (obj.obj_t) {
        .string => {
            const self: *String = @fieldParentPtr("obj", obj);
            self.destroy(vm);
        },
        .function => {
            const self: *Function = @fieldParentPtr("obj", obj);
            self.destroy(vm);
        },
        .native => {
            const self: *Native = @fieldParentPtr("obj", obj);
            self.destroy(vm);
        },
        .closure => {
            const self: *Closure = @fieldParentPtr("obj", obj);
            self.destroy(vm);
        },
        .upvalue => {
            const self: *Upvalue = @fieldParentPtr("obj", obj);
            self.destroy(vm);
        },
    }
}

pub const String = struct {
    obj: Obj,
    bytes: []const u8,
    hash: u32,

    pub fn allocate(vm: *VM, bytes: []const u8, hash: u32) !*String {
        const str = try Obj.create(vm, String, .string);
        str.bytes = bytes;
        str.hash = hash;
        vm.push(Value{ .obj = &str.obj });
        _ = try vm.strings.set(str, Value.nil);
        _ = vm.pop();
        return str;
    }

    pub fn copy(vm: *VM, bytes: []const u8) !*String {
        const hash = hashFn(bytes);
        if (vm.strings.findString(bytes, hash)) |s| {
            return s;
        }

        const buffer = try vm.allocator.alloc(u8, bytes.len);
        std.mem.copyForwards(u8, buffer, bytes);
        return allocate(vm, buffer, hash);
    }

    pub fn take(vm: *VM, bytes: []const u8) !*String {
        const hash = hashFn(bytes);
        if (vm.strings.findString(bytes, hash)) |s| {
            vm.allocator.free(bytes);
            return s;
        }

        return try allocate(vm, bytes, hash);
    }

    pub fn mark(self: *String, vm: *VM) !void {
        try self.obj.mark(vm);
    }

    pub fn equal(self: *String, other: *String) bool {
        return (self.bytes.len == other.bytes.len) and
            std.mem.eql(u8, self.bytes, other.bytes);
    }

    pub fn getHash(self: *String) u32 {
        return self.hash;
    }

    pub fn isMarked(self: *String) bool {
        return self.obj.is_marked;
    }

    pub fn destroy(self: *String, vm: *VM) void {
        vm.allocator.free(self.bytes);
        vm.allocator.destroy(self);
    }
};

pub const Function = struct {
    obj: Obj,
    arity: usize,
    chunk: Chunk,
    name: ?*String,
    upvalue_count: u8,

    pub fn allocate(vm: *VM) !*Function {
        const func = try Obj.create(vm, Function, .function);
        func.arity = 0;
        func.chunk = Chunk.init(vm.allocator);
        func.name = null;
        func.upvalue_count = 0;
        return func;
    }

    pub fn destroy(self: *Function, vm: *VM) void {
        self.chunk.deinit();
        vm.allocator.destroy(self);
    }

    pub fn markConstants(self: *Function, vm: *VM) !void {
        for (self.chunk.constants.items) |constant| {
            try constant.mark(vm);
        }
    }
};

pub const Native = struct {
    obj: Obj,
    func: NativeFn,
    name: []const u8,

    pub const NativeFn = *const fn (arg_count: u8) Value;

    pub fn allocate(vm: *VM, func: NativeFn, name: []const u8) !*Native {
        const native = try Obj.create(vm, Native, .native);
        native.func = func;
        native.name = name;
        return native;
    }

    pub fn destroy(self: *Native, vm: *VM) void {
        vm.allocator.destroy(self);
    }
};

pub const Closure = struct {
    obj: Obj,
    func: *Function,
    upvalues: []?*Upvalue,

    pub fn allocate(vm: *VM, func: *Function) !*Closure {
        const upvalues = try vm.allocator.alloc(?*Upvalue, func.upvalue_count);
        for (upvalues) |*upvalue| upvalue.* = null;

        const closure = try Obj.create(vm, Closure, .closure);
        closure.func = func;
        closure.upvalues = upvalues;
        return closure;
    }

    pub fn destroy(self: *Closure, vm: *VM) void {
        vm.allocator.free(self.upvalues);
        vm.allocator.destroy(self);
    }
};

pub const Upvalue = struct {
    obj: Obj,
    location: *Value,
    closed: Value,
    next: ?*Upvalue,

    pub fn allocate(vm: *VM, location: *Value) !*Upvalue {
        const upvalue = try Obj.create(vm, Upvalue, .upvalue);
        upvalue.location = location;
        upvalue.next = null;
        upvalue.closed = Value.nil;
        return upvalue;
    }

    pub fn destroy(self: *Upvalue, vm: *VM) void {
        vm.allocator.destroy(self);
    }
};
