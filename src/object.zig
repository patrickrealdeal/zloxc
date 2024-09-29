const std = @import("std");
const VM = @import("vm.zig").VM;

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

    pub fn destroy(self: *Obj, vm: *VM) void {
        switch (self.obj_t) {
            .string => self.asString().destroy(vm),
        }
    }
};

pub const String = struct {
    obj: Obj,
    bytes: []const u8,

    pub fn allocate(vm: *VM, bytes: []const u8) !*String {
        const str = try Obj.create(vm, String, .string);
        str.bytes = bytes;
        return str;
    }

    pub fn copy(vm: *VM, bytes: []const u8) !*String {
        const heap_bytes = try vm.allocator.alloc(u8, bytes.len);
        std.mem.copyForwards(u8, heap_bytes, bytes);
        return allocate(vm, heap_bytes);
    }

    pub fn take(vm: *VM, bytes: []const u8) !*String {
        return try allocate(vm, bytes);
    }

    pub fn destroy(self: *String, vm: *VM) void {
        vm.allocator.free(self.bytes);
        vm.allocator.destroy(self);
    }
};

test "object" {
    const obj = try String.allocate(std.testing.allocator, "hello, world!");
    defer std.testing.allocator.destroy(obj);
    try std.testing.expect(std.mem.eql(u8, obj.bytes, "hello, world!"));
}
