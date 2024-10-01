const std = @import("std");
const VM = @import("vm.zig").VM;
const Value = @import("value.zig").Value;
const Table = @import("table.zig");

const ObjType = enum {
    string,
};

pub const Obj = struct {
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

    pub fn is(self: *Obj, obj_t: ObjType) bool {
        return self.obj_t == obj_t;
    }

    pub fn destroy(obj: *Obj, vm: *VM) void {
        const self: *String = @fieldParentPtr("obj", obj);
        vm.allocator.free(self.bytes);
        vm.allocator.destroy(self);
    }
};

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
