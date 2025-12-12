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
    class,
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

pub fn as(object: *Obj, comptime T: type) *T {
    return @alignCast(@fieldParentPtr("obj", object));
}

pub fn asValue(object: *Obj) Value {
    return .{ .obj = object };
}

pub fn is(object: *Obj, obj_t: ObjType) bool {
    return object.obj_t == obj_t;
}

pub fn mark(object: *Obj, vm: *VM) !void {
    if (object.is_marked) return;
    if (comptime debug.log_gc) std.debug.print("{*} mark {f}\n", .{ object, Value{ .obj = object } });
    object.is_marked = true;
    try vm.gray_stack.append(vm.allocator, object);
}

pub fn blacken(object: *Obj, vm: *VM) !void {
    switch (object.obj_t) {
        .native, .string => return,
        .upvalue => try object.as(Upvalue).closed.mark(vm),
        .function => {
            const function = object.as(Function);
            if (function.name) |name| {
                try name.obj.mark(vm);
            }
            try function.markConstants(vm);
        },
        .closure => {
            const closure = object.as(Closure);
            try closure.func.obj.mark(vm);
            for (0..closure.func.upvalue_count) |i| {
                if (closure.upvalues[i]) |upvalue|
                    try upvalue.obj.mark(vm);
            }
        },
        .class => {
            const class = object.as(Class);
            try class.name.obj.mark(vm);
        },
    }
    //if (comptime debug.log_gc) std.debug.print("{*} blacken {}\n", .{ self, Value{ .obj = self } });
}

pub fn destroy(object: *Obj, vm: *VM) void {
    switch (object.obj_t) {
        .string => {
            const obj: *String = @fieldParentPtr("obj", object);
            obj.destroy(vm);
        },
        .function => {
            const obj: *Function = @fieldParentPtr("obj", object);
            obj.destroy(vm);
        },
        .native => {
            const obj: *Native = @fieldParentPtr("obj", object);
            obj.destroy(vm);
        },
        .closure => {
            const obj: *Closure = @fieldParentPtr("obj", object);
            obj.destroy(vm);
        },
        .upvalue => {
            const obj: *Upvalue = @fieldParentPtr("obj", object);
            obj.destroy(vm);
        },
        .class => {
            const obj: *Class = @fieldParentPtr("obj", object);
            obj.destroy(vm);
        },
    }
}

pub const String = struct {
    obj: Obj,
    bytes: []const u8,
    hash: u32,
    owns_bytes: bool = false,

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
        @memcpy(buffer, bytes);
        const str = try allocate(vm, buffer, hash);
        str.owns_bytes = true;
        return str;
    }

    pub fn take(vm: *VM, bytes: []const u8) !*String {
        const hash = hashFn(bytes);
        if (vm.strings.findString(bytes, hash)) |s| {
            vm.allocator.free(bytes);
            return s;
        }

        const str = try allocate(vm, bytes, hash);
        str.owns_bytes = false;
        return str;
    }

    pub fn mark(object: *String, vm: *VM) !void {
        try object.obj.mark(vm);
    }

    pub fn equal(object: *String, other: *String) bool {
        return (object.bytes.len == other.bytes.len) and
            std.mem.eql(u8, object.bytes, other.bytes);
    }

    pub fn getHash(object: *String) u32 {
        return object.hash;
    }

    pub fn isMarked(object: *String) bool {
        return object.obj.is_marked;
    }

    pub fn destroy(object: *String, vm: *VM) void {
        if (object.owns_bytes) {
            vm.allocator.free(object.bytes);
            vm.allocator.destroy(object);
        }
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

    pub fn destroy(object: *Function, vm: *VM) void {
        object.chunk.deinit();
        vm.allocator.destroy(object);
    }

    pub fn markConstants(object: *Function, vm: *VM) !void {
        for (object.chunk.constants.items) |constant| {
            try constant.mark(vm);
        }
    }
};

pub const Native = struct {
    obj: Obj,
    func: NativeFn,
    name: []const u8,

    pub const NativeFn = *const fn (vm: *VM, arg_count: u8) anyerror!Value;

    pub fn allocate(vm: *VM, func: NativeFn, name: []const u8) !*Native {
        const native = try Obj.create(vm, Native, .native);
        native.func = func;
        native.name = name;
        return native;
    }

    pub fn destroy(object: *Native, vm: *VM) void {
        vm.allocator.destroy(object);
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

    pub fn destroy(object: *Closure, vm: *VM) void {
        vm.allocator.free(object.upvalues);
        vm.allocator.destroy(object);
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

    pub fn destroy(object: *Upvalue, vm: *VM) void {
        vm.allocator.destroy(object);
    }
};

pub const Class = struct {
    obj: Obj,
    name: *String,

    pub fn allocate(vm: *VM, name: *String) !*Class {
        const class = try Obj.create(vm, Class, .class);
        class.name = name;
        return class;
    }

    //pub fn mark(object: *String, vm: *VM) !void {
    //try object.mark(vm);
    //}

    fn destroy(object: *Class, vm: *VM) void {
        vm.allocator.destroy(object);
    }
};
