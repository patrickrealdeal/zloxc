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
        Closure,
        Upvalue,
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
            .Closure => self.asClosure().destroy(vm),
            .Upvalue => self.asUpvalue().destroy(vm),
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

    pub fn asClosure(self: *Obj) *Closure {
        return @fieldParentPtr(Closure, "obj", self);
    }

    pub fn asUpvalue(self: *Obj) *Upvalue {
        return @fieldParentPtr(Upvalue, "obj", self);
    }

    pub fn asObjType(self: *Obj, comptime objType: Obj.Type) *ObjType(objType) {
        return switch (objType) {
            .String => self.asString(),
            .Function => self.asFunction(),
            .Native => self.asNative(),
            .Closure => self.asClosure(),
            .Upvalue => self.asUpvalue(),
        };
    }

    pub fn ObjType(comptime objType: Obj.Type) type {
        return switch (objType) {
            .String => String,
            .Function => Function,
            .Native => Native,
            .Closure => Closure,
            .Upvalue => Upvalue,
        };
    }

    pub fn equal(self: *const Obj, other: *const Obj) bool {
        switch (self.objType) {
            .String, .Function, .Native, .Closure, .Upvalue => return self == other,
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
            .Closure => {
                return "Closure";
            },
            .Upvalue => {
                return "Upvalue";
            },
        }
    }

    pub const String = struct {
        obj: Obj,
        bytes: []const u8,
        hash: usize,

        pub fn create(vm: *VM, bytes: []const u8) !*String {
            const hash = hashFn(bytes);

            if (vm.strings.findString(bytes, hash)) |interned| {
                vm.allocator.free(bytes);
                return interned;
            } else {
                const obj = try Obj.create(vm, String, .String);
                const out = obj.asString();
                out.* = String{
                    .obj = obj.*,
                    .hash = hash,
                    .bytes = bytes,
                };
                // Make sure string is visible to the GC, since adding
                // to the table may allocate
                vm.push(out.obj.value());
                _ = try vm.strings.set(out, Value.fromBool(true));
                _ = vm.pop();
                return out;
            }
        }

        pub fn copy(vm: *VM, source: []const u8) !*String {
            const buffer = try vm.allocator.alloc(u8, source.len);
            std.mem.copy(u8, buffer, source);
            return String.create(vm, buffer);
        }
        pub fn destroy(self: *String, vm: *VM) void {
            vm.allocator.free(self.bytes);
            vm.allocator.destroy(self);
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
        upvalueCount: u8,
        chunk: Chunk,
        name: ?*Obj.String,

        pub fn create(vm: *VM) !*Function {
            const obj = try Obj.create(vm, Function, .Function);
            const func = obj.asFunction();
            func.* = Function{
                .obj = obj.*,
                .arity = 0,
                .upvalueCount = 0,
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

    pub const Closure = struct {
        obj: Obj,
        function: *Function,
        upvalues: []?*Upvalue,

        pub fn create(vm: *VM, function: *Function) !*Closure {
            const upvalues = try vm.allocator.alloc(?*Upvalue, function.upvalueCount);
            for (upvalues) |*upvalue| upvalue.* = null;

            const obj = try Obj.create(vm, Closure, .Closure);
            const closure = obj.asClosure();
            closure.* = Closure{
                .obj = obj.*,
                .function = function,
                .upvalues = upvalues,
            };

            return closure;
        }

        pub fn destroy(self: *Closure, vm: *VM) void {
            vm.allocator.destroy(self);
        }
    };

    pub const Upvalue = struct {
        obj: Obj,
        location: *Value,
        closed: Value,
        next: ?*Upvalue,

        pub fn create(vm: *VM, location: *Value, next: ?*Upvalue) !*Upvalue {
            _ = next;
            const obj = try Obj.create(vm, Upvalue, .Upvalue);
            const upvalue = obj.asUpvalue();
            upvalue.* = Upvalue{
                .obj = obj.*,
                .location = location,
                .closed = Value.nil(),
                .next = null,
            };

            return upvalue;
        }

        pub fn destroy(self: *Upvalue, vm: *VM) void {
            vm.allocator.destroy(self);
        }
    };
};
