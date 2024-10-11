const std = @import("std");
const VM = @import("vm.zig");
const Value = @import("value.zig").Value;
const Obj = @import("object.zig");
const debug = @import("debug.zig");
const Compiler = @import("compiler.zig").Compiler;

/// Custom Allocator that follows the zig interface
/// https://github.com/ziglang/zig/blob/master/lib/std/mem/Allocator.zig
pub const GCAllocator = struct {
    vm: *VM,
    parent_allocator: std.mem.Allocator,
    bytes_allocated: usize,
    next_gc: usize,

    const heap_grow_factor = 2;
    const vtable: std.mem.Allocator.VTable = .{ .alloc = alloc, .resize = resize, .free = free };

    pub fn init(vm: *VM, parent_allocator: std.mem.Allocator) GCAllocator {
        return .{
            .vm = vm,
            .parent_allocator = parent_allocator,
            .bytes_allocated = 0,
            .next_gc = 100 * 100,
        };
    }

    pub fn allocator(self: *GCAllocator) std.mem.Allocator {
        return .{ .ptr = self, .vtable = &vtable };
    }

    fn alloc(ctx: *anyopaque, len: usize, ptr_align: u8, ret_addr: usize) ?[*]u8 {
        const self: *GCAllocator = @ptrCast(@alignCast(ctx));
        if (self.bytes_allocated + len > self.next_gc or debug.stress_gc) {
            self.collectGarbage();
        }

        const out = self.parent_allocator.rawAlloc(len, ptr_align, ret_addr) orelse null;
        self.bytes_allocated += len;
        return out;
    }

    fn resize(ctx: *anyopaque, buf: []u8, buf_align: u8, new_len: usize, ret_addr: usize) bool {
        const self: *GCAllocator = @ptrCast(@alignCast(ctx));
        if (new_len > buf.len) {
            if ((self.bytes_allocated + (new_len - buf.len) > self.next_gc) or debug.stress_gc) {
                self.collectGarbage();
            }
        }

        if (self.parent_allocator.rawResize(buf, buf_align, new_len, ret_addr)) {
            if (new_len > buf.len) {
                self.bytes_allocated += new_len - buf.len;
            } else {
                self.bytes_allocated -= buf.len - new_len;
            }
            return true;
        } else {
            return false;
        }
    }

    fn free(ctx: *anyopaque, buf: []u8, buf_align: u8, ret_addr: usize) void {
        const self: *GCAllocator = @ptrCast(@alignCast(ctx));
        self.parent_allocator.rawFree(buf, buf_align, ret_addr);
        self.bytes_allocated -= buf.len;
    }

    fn collectGarbage(self: *GCAllocator) void {
        if (debug.log_gc) {
            std.debug.print("-- gc begin\n", .{});
        }

        self.markRoots();
        self.traceReferences();
        self.removeUnreferencedStrings();
        self.sweep();

        self.next_gc = self.bytes_allocated * heap_grow_factor;

        if (debug.log_gc) {
            std.debug.print("-- gc end\n", .{});
        }
    }

    fn markRoots(self: *GCAllocator) void {
        for (self.vm.stack.items) |value| {
            self.markValue(value);
        }

        for (0..self.vm.frame_count) |frame| {
            self.markObject(&self.vm.frames[frame].closure.obj);
        }

        var maybeUpvalue = self.vm.open_upvalues;
        while (maybeUpvalue) |upvalue| {
            self.markObject(&upvalue.obj);
            maybeUpvalue = upvalue.next;
        }

        self.markHashMap();
        self.markCompilerRoots();
    }

    fn markValue(self: *GCAllocator, value: Value) void {
        if (value.isObj()) self.markObject(value.asObj());
    }

    fn markObject(self: *GCAllocator, obj: *Obj) void {
        if (obj.is_marked) return;

        if (debug.log_gc) {
            std.debug.print("{} mark {}\n", .{ @intFromPtr(obj), Value{ .obj = obj } });
        }

        obj.is_marked = true;

        obj.next_gray = self.vm.next_gray;
        self.vm.next_gray = obj;
    }

    fn markHashMap(self: *GCAllocator) void {
        var iterator = self.vm.globals.iterator();
        while (iterator.next()) |entry| {
            self.markObject(&entry.key_ptr.*.obj);
            self.markValue(entry.value_ptr.*);
        }
    }

    fn markCompilerRoots(self: *GCAllocator) void {
        if (self.vm.parser) |parser| {
            var maybeCompiler: ?*Compiler = parser.compiler;

            while (maybeCompiler) |compiler| {
                std.debug.print("OBJ: {any}\n", .{compiler.func.obj});
                self.markObject(&compiler.func.obj);
                maybeCompiler = compiler.enclosing;
            }
        }
    }

    fn traceReferences(self: *GCAllocator) void {
        while (self.vm.next_gray) |obj| {
            self.vm.next_gray = obj.next_gray;
            obj.next_gray = null;
            self.blackenObject(obj);
        }
    }

    fn sweep(self: *GCAllocator) void {
        var previous: ?*Obj = null;
        var maybe_obj = self.vm.objects;
        while (maybe_obj) |obj| {
            if (obj.is_marked) {
                obj.is_marked = false;
                previous = obj;
                maybe_obj = obj.next;
            } else {
                const unreach = obj;
                maybe_obj = obj.next;
                if (previous) |p| {
                    p.next = maybe_obj;
                } else {
                    self.vm.objects = maybe_obj;
                }

                unreach.destroy(self.vm);
            }
        }
    }

    fn removeUnreferencedStrings(self: *GCAllocator) void {
        var iterator = self.vm.strings.iterator();
        while (iterator.next()) |*entry| {
            if (!entry.value_ptr.*.obj.is_marked) {
                std.debug.print("ARE WE HERE?", .{});
                if (debug.log_gc) {
                    std.debug.print("REMOVED {s}\n", .{entry.key_ptr.*});
                    _ = self.vm.strings.remove(entry.key_ptr.*);
                }
            }
        }
    }

    fn blackenObject(self: *GCAllocator, obj: *Obj) void {
        if (debug.log_gc) {
            std.debug.print("{} blacken {}\n", .{ @intFromPtr(obj), Value{ .obj = obj } });
        }

        switch (obj.obj_t) {
            .upvalue => self.markValue(obj.as(Obj.Upvalue).closed),
            .function => {
                const func = obj.as(Obj.Function);
                if (func.name) |str| {
                    self.markObject(&str.obj);
                    self.markArray(func.chunk.constants.items);
                }
            },
            .closure => {
                const closure = obj.as(Obj.Closure);
                self.markObject(&closure.func.obj);
                for (closure.upvalues) |maybe_upval| {
                    if (maybe_upval) |val| {
                        self.markObject(&val.obj);
                    }
                }
            },
            .native, .string => {},
        }
    }

    fn markArray(self: *GCAllocator, values: []Value) void {
        for (values) |val| self.markValue(val);
    }
};
