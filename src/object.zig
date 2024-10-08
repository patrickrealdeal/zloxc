const std = @import("std");
const VM = @import("vm.zig");
const Value = @import("value.zig").Value;
const Chunk = @import("chunk.zig");

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

pub fn create(vm: *VM, comptime T: type, obj_t: ObjType) !*T {
    const ptr_t = try vm.allocator.create(T);
    ptr_t.obj = Obj{
        .obj_t = obj_t,
        .next = vm.objects,
    };
    vm.objects = &ptr_t.obj;
    return ptr_t;
}

pub fn asString(self: *Obj) *String {
    return @alignCast(@fieldParentPtr("obj", self));
}

pub fn asFunction(self: *Obj) *Function {
    return @alignCast(@fieldParentPtr("obj", self));
}

pub fn asNative(self: *Obj) *Native {
    return @alignCast(@fieldParentPtr("obj", self));
}

pub fn asClosure(self: *Obj) *Closure {
    return @alignCast(@fieldParentPtr("obj", self));
}

pub fn asUpvalue(self: *Obj) *Upvalue {
    return @alignCast(@fieldParentPtr("obj", self));
}

pub fn is(self: *Obj, obj_t: ObjType) bool {
    return self.obj_t == obj_t;
}

pub fn destroy(obj: *Obj, vm: *VM) void {
    switch (obj.obj_t) {
        .string => {
            const self: *String = @fieldParentPtr("obj", obj);
            vm.allocator.free(self.bytes);
            vm.allocator.destroy(self);
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

    pub fn allocate(vm: *VM, bytes: []const u8) !*String {
        const str = try Obj.create(vm, String, .string);
        str.bytes = bytes;
        try vm.strings.put(bytes, str);
        return str;
    }

    pub fn copy(vm: *VM, bytes: []const u8) !*String {
        const interned = vm.strings.get(bytes);
        if (interned) |str| {
            //vm.allocator.free(bytes);
            return str;
        }
        const heap_bytes = try vm.allocator.dupe(u8, bytes);
        return allocate(vm, heap_bytes);
    }

    pub fn take(vm: *VM, bytes: []const u8) !*String {
        const interned = vm.strings.get(bytes);
        if (interned) |str| {
            vm.allocator.free(bytes);
            return str;
        }

        return try allocate(vm, bytes);
    }

    pub fn destroy(self: *String, vm: *VM) void {
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
};

pub const Native = struct {
    obj: Obj,
    func: NativeFn,

    pub const NativeFn = *const fn (arg_count: u8) Value;

    pub fn allocate(vm: *VM, func: NativeFn) !*Native {
        const native = try Obj.create(vm, Native, .native);
        native.func = func;
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
