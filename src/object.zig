const std = @import("std");
const VM = @import("./vm.zig").VM;
const Value = @import("./value.zig").Value;

pub const Obj = struct {
    objType: Type,
    next: ?*Obj,

    pub const Type = enum(u8) {
        String,
    };

    pub fn create(vm: *VM, comptime T: type, objType: Type) !*Obj {
        const ptr = try vm.allocator.create(T);

        ptr.obj = Obj{
            .objType = objType,
            .next = vm.objects,
        };

        // every time we allocate an Obj we insert it in the list
        vm.objects = &ptr.obj;

        return &ptr.obj;
    }

    pub fn destroy(self: *Obj, vm: *VM) void {
        switch (self.objType) {
            .String => self.asString().destroy(vm),
        }
    }

    pub fn is(self: *Obj, objType: Type) bool {
        return self.objType == objType;
    }

    pub fn asString(self: *Obj) *String {
        return @fieldParentPtr(String, "obj", self);
    }

    pub fn asObjType(self: *Obj, comptime objType: Obj.Type) *ObjType(objType) {
        return switch (objType) {
            .String => self.asString(),
        };
    }

    pub fn ObjType(comptime objType: Obj.Type) type {
        return switch (objType) {
            .String => String,
        };
    }

    pub fn equal(self: *const Obj, other: *const Obj) bool {
        switch (self.objType) {
            .String => return self == other,
        }
    }

    pub fn value(self: *Obj) Value {
        return Value.fromObj(self);
    }

    pub fn toString(self: *Obj) []const u8 {
        switch (self.objType) {
            .String => {
                return self.asString().bytes;
            },
        }
    }

    pub const String = struct {
        obj: Obj,
        bytes: []const u8,

        /// Does not take ownership of the the bytes we pass int
        pub fn copy(vm: *VM, bytes: []const u8) !*String {
            const heapChars = try vm.allocator.alloc(u8, bytes.len);
            std.mem.copy(u8, heapChars, bytes);
            return allocate(vm, heapChars);
        }

        fn allocate(vm: *VM, bytes: []const u8) !*String {
            var obj = try Obj.create(vm, String, .String);
            const string = obj.asString();
            string.bytes = bytes;

            // Here we will push the string on the stack
            return string;
        }

        /// Takes ownership of bytes
        pub fn take(vm: *VM, bytes: []const u8) !*String {
            return allocate(vm, bytes);
        }

        pub fn destroy(self: *String, vm: *VM) void {
            vm.allocator.free(self.bytes);
            vm.allocator.destroy(self);
        }
    };
};
