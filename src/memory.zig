const std = @import("std");
const VM = @import("vm.zig");
const Value = @import("value.zig").Value;
const Obj = @import("object.zig");
const debug = @import("debug.zig");
const Compiler = @import("compiler.zig").Compiler;

/// Custom Allocator that follows the zig interface
/// https://github.com/ziglang/zig/blob/master/lib/std/mem/Allocator.zig
const GC_HEAP_GROW_FACTOR = 2;

pub const GCAllocator = struct {
    vm: ?*VM,
    parent_allocator: std.mem.Allocator,
    bytes_allocated: usize,
    next_gc: usize,

    pub fn init(parent_allocator: std.mem.Allocator) GCAllocator {
        return .{
            .vm = null,
            .parent_allocator = parent_allocator,
            .bytes_allocated = 0,
            .next_gc = 4,
        };
    }

    pub fn allocator(self: *GCAllocator) std.mem.Allocator {
        return .{
            .ptr = self,
            .vtable = &.{
                .alloc = alloc,
                .resize = resize,
                .free = free,
                .remap = remap,
            },
        };
    }

    fn alloc(ctx: *anyopaque, len: usize, ptr_align: std.mem.Alignment, ret_address: usize) ?[*]u8 {
        const self: *GCAllocator = @ptrCast(@alignCast(ctx));
        if ((self.bytes_allocated + len > self.next_gc) or debug.stress_gc) {
            self.collectGarbage() catch return null;
        }

        const result = self.parent_allocator.rawAlloc(len, ptr_align, ret_address);
        if (result != null) self.bytes_allocated += len;
        return result;
    }

    fn resize(ctx: *anyopaque, buf: []u8, buf_align: std.mem.Alignment, new_len: usize, ret_address: usize) bool {
        const self: *GCAllocator = @ptrCast(@alignCast(ctx));
        if (new_len > buf.len) {
            if ((self.bytes_allocated + (new_len - buf.len) > self.next_gc) or debug.stress_gc) {
                self.collectGarbage() catch return false;
            }
        }

        if (self.parent_allocator.rawResize(buf, buf_align, new_len, ret_address)) {
            if (new_len > buf.len) {
                self.bytes_allocated += new_len - buf.len;
            } else {
                self.bytes_allocated -= buf.len - new_len;
            }

        }
        std.debug.assert(new_len > buf.len);
        return self.parent_allocator.rawResize(buf, buf_align, new_len, ret_address);
    }

    fn remap(ctx: *anyopaque, memory: []u8, alignment: std.mem.Alignment, new_len: usize, ret_addr: usize) ?[*]u8 {
        _ = ctx;
        _ = memory;
        _ = new_len;
        _ = alignment;
        _ = ret_addr;
        return null;
    }

    fn free(ctx: *anyopaque, buf: []u8, buf_align: std.mem.Alignment, ret_address: usize) void {
        const self: *GCAllocator = @ptrCast(@alignCast(ctx));
        self.parent_allocator.rawFree(buf, buf_align, ret_address);
        self.bytes_allocated -= buf.len;
    }

    // Garbage collector implementation

    fn markCompilerRoots(self: *GCAllocator) !void {
        const vm = self.vm orelse return;
        if (vm.parser) |p| {
            var comp: ?*Compiler = p.compiler;
            while (comp) |c| {
                try c.func.obj.mark(vm);
                comp = c.enclosing;
            }
        }
    }

    fn markRoots(self: *GCAllocator) !void {
        //std.debug.print("Called MARKROOTS\n", .{});
        const vm = self.vm orelse return;

        for (vm.stack.items) |item| {
            try item.mark(vm);
        }

        try vm.globals.mark(vm);

        var i: u32 = 0;
        while (i < vm.frame_count) : (i += 1) {
            try vm.frames[i].closure.obj.mark(vm);
        }

        var upvalue = vm.open_upvalues;
        while (upvalue) |u| {
            try u.obj.mark(vm);
            upvalue = u.next;
        }

        //try vm.strings.mark(vm);
        try self.markCompilerRoots();
    }

    fn traceReferences(self: *GCAllocator) !void {
        const vm = self.vm orelse return;

        while (vm.gray_stack.items.len > 0) {
            const object = vm.gray_stack.pop();

            if (comptime debug.stress_gc) std.debug.print("GS: {} {any}\n", .{ object.?.obj_t, object.?.is_marked });
            try object.?.blacken(vm);
        }
    }

    fn sweep(self: *GCAllocator) void {
        const vm = self.vm orelse return;
        var previous: ?*Obj = null;
        var object = vm.objects;
        while (object) |o| {
            if (o.is_marked) {
                o.is_marked = false;
                previous = o;
                object = o.next;
            } else {
                var unreached = o;
                object = o.next;
                if (previous) |p| {
                    p.next = object;
                } else {
                    vm.objects = object;
                }
                std.debug.print("CALLED unreached.destroy()\n", .{});
                self.bytes_allocated -= @sizeOf(@TypeOf(unreached));
                unreached.destroy(vm);
            }
        }
    }

    pub fn collectGarbage(self: *GCAllocator) !void {
        const vm = self.vm orelse return;
        const before = self.bytes_allocated;

        if (comptime debug.log_gc) std.debug.print("BEFORE {d}\n", .{before});
        if (comptime debug.log_gc) std.debug.print("BYTES ALLOCATED {d}\n", .{self.bytes_allocated});
        if (comptime debug.log_gc) std.debug.print("-- gc begin\n", .{});

        try self.markRoots();
        try self.traceReferences();
        vm.globals.removeWhite();
        vm.strings.removeWhite();
        self.sweep();

        self.next_gc = self.bytes_allocated * GC_HEAP_GROW_FACTOR;

        if (comptime debug.log_gc) std.debug.print(
            "-- gc end\ncollected {d} bytes (from {d} to {d}) next at {d}\n",
            .{ before - self.bytes_allocated, before, self.bytes_allocated, self.next_gc },
        );
    }
};

test "gc" {
    var gc = GCAllocator.init(std.heap.page_allocator);
    const allocator = gc.allocator();

    const vm = try VM.init(allocator);
    gc.vm = vm;
    //defer vm.deinit();

    const str = try Obj.String.copy(vm, "hello");
    _ = str;
    try gc.collectGarbage();
}
