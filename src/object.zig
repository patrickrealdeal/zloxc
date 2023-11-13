const std = @import("std");
const VM = @import("./vm.zig").VM;
const Value = @import("./value.zig").Value;
const Table = @import("./table.zig").Table;
const Chunk = @import("./chunk.zig").Chunk;
const debug = @import("./debug.zig");

pub const Obj = struct {
    objType: Type,
    next: ?*Obj,

    pub const Type = enum(u8) {
        String,
        Function,
        Native,
    };

    pub fn create(vm: *VM, comptime T: type, objType: Type) !*Obj {
        const ptr = try vm.allocator.create(T);

        ptr.obj = Obj{
            .objType = objType,
            .next = vm.objects,
        };

        // every time we allocate an Obj we insert it in the list
        vm.objects = &ptr.obj;

        if (debug.trace_parser) {
            std.debug.print("{x} allocate {} for {s}\n", .{ @intFromPtr(&ptr.obj), @sizeOf(T), @typeName(T) });
        }

        return &ptr.obj;
    }

    pub fn destroy(self: *Obj, vm: *VM) void {
        switch (self.objType) {
            .String => self.asString().destroy(vm),
            .Function => self.asFunction().destroy(vm),
            .Native => self.asNative().destroy(vm),
        }
    }

    pub fn is(self: *Obj, objType: Type) bool {
        return self.objType == objType;
    }

    pub fn asString(self: *Obj) *String {
        return @fieldParentPtr(String, "obj", self);
    }

    pub fn asFunction(self: *Obj) *Function {
        return @fieldParentPtr(Function, "obj", self);
    }

    pub fn asNative(self: *Obj) *Native {
        return @fieldParentPtr(Native, "obj", self);
    }

    pub fn asObjType(self: *Obj, comptime objType: Obj.Type) *ObjType(objType) {
        return switch (objType) {
            .String => self.asString(),
            .Function => self.asFunction(),
            .Native => self.asNative(),
        };
    }

    pub fn ObjType(comptime objType: Obj.Type) type {
        return switch (objType) {
            .String => String,
            .Function => Function,
            .Native => Native,
        };
    }

    pub fn equal(self: *const Obj, other: *const Obj) bool {
        switch (self.objType) {
            .String, .Function, .Native => return self == other,
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
            .Function => {
                return if (self.asFunction().name) |n| n.bytes else "Function";
            },
            .Native => {
                return "Native";
            },
        }
    }

    pub const String = struct {
        obj: Obj,
        bytes: []const u8,
        hash: usize,

        /// Does not take ownership of the the bytes we pass int
        pub fn copy(vm: *VM, bytes: []const u8) !*String {
            const heapChars = try vm.allocator.alloc(u8, bytes.len);
            std.mem.copy(u8, heapChars, bytes);
            return allocate(vm, heapChars);
        }

        fn allocate(vm: *VM, bytes: []const u8) !*String {
            const hash = hashFn(bytes);

            if (vm.strings.findString(bytes, hash)) |interned| {
                vm.allocator.free(bytes);
                return interned;
            }

            var obj = try Obj.create(vm, String, .String);
            const string = obj.asString();
            string.bytes = bytes;
            string.hash = hash;

            // Add string to HashTable
            _ = try vm.strings.set(string, Value.fromBool(true));

            // Here we will push the string on the stack
            return string;
        }

        pub fn destroy(self: *String, vm: *VM) void {
            vm.allocator.free(self.bytes);
            vm.allocator.destroy(self);
        }

        /// Takes ownership of bytes
        pub fn take(vm: *VM, bytes: []const u8) !*String {
            return allocate(vm, bytes);
        }

        // This is the FNV-1a has function
        // Zig has an implementation already in std.hash.fnv
        pub fn hashFn(bytes: []const u8) u32 {
            var hash: u32 = 2166136261;

            for (bytes) |byte| {
                hash ^= byte;

                // zig special operator to wrap around on overflow - neat
                hash *%= 16777619;
            }

            return hash;
        }
    };

    pub const Function = struct {
        obj: Obj,
        arity: u8,
        chunk: Chunk,
        name: ?*Obj.String,

        pub fn create(vm: *VM) !*Function {
            const obj = try Obj.create(vm, Function, .Function);
            const func = obj.asFunction();
            func.* = Function{
                .obj = obj.*,
                .arity = 0,
                .name = null,
                .chunk = Chunk.init(vm.allocator),
            };

            return func;
        }

        pub fn destroy(self: *Function, vm: *VM) void {
            self.chunk.deinit();
            vm.allocator.destroy(self);
        }
    };

    pub const Native = struct {
        obj: Obj,
        name: *Obj.String,
        function: *const Fn,

        pub const Fn = fn (vm: *VM, args: []Value) error{RuntimeError}!Value;

        pub fn create(vm: *VM, name: *Obj.String, function: *const Fn) !*Native {
            const obj = try Obj.create(vm, Native, .Native);
            const native = obj.asNative();
            native.* = Native{
                .obj = obj.*,
                .name = name,
                .function = function,
            };

            return native;
        }

        pub fn destroy(self: *Native, vm: *VM) void {
            vm.allocator.destroy(self);
        }
    };
};
