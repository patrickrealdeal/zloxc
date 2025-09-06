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
    is_collecting: bool,

    pub fn init(parent_allocator: std.mem.Allocator) GCAllocator {
        return .{
            .vm = null,
            .parent_allocator = parent_allocator,
            .bytes_allocated = 0,
            .next_gc = 4,
            .is_collecting = false,
        };
    }

    pub fn allocator(gca: *GCAllocator) std.mem.Allocator {
        return .{
            .ptr = gca,
            .vtable = &.{
                .alloc = alloc,
                .resize = resize,
                .free = free,
                .remap = remap,
            },
        };
    }

    fn alloc(ctx: *anyopaque, len: usize, ptr_align: std.mem.Alignment, ret_address: usize) ?[*]u8 {
        const gca: *GCAllocator = @ptrCast(@alignCast(ctx));
        if ((gca.bytes_allocated + len > gca.next_gc) or debug.stress_gc) {
            gca.collectGarbage() catch return null;
        }

        if (!gca.is_collecting) {
            gca.bytes_allocated += len;
        }

        //gca.bytes_allocated += len;
        return gca.parent_allocator.rawAlloc(len, ptr_align, ret_address);
    }

    fn resize(ctx: *anyopaque, buf: []u8, buf_align: std.mem.Alignment, new_len: usize, ret_address: usize) bool {
        const gca: *GCAllocator = @ptrCast(@alignCast(ctx));
        if (new_len > buf.len) {
            if ((gca.bytes_allocated + (new_len - buf.len) > gca.next_gc) or debug.stress_gc) {
                gca.collectGarbage() catch return false;
            }
        }

        if (gca.parent_allocator.rawResize(buf, buf_align, new_len, ret_address)) {
            if (!gca.is_collecting) {
                if (new_len > buf.len) {
                    gca.bytes_allocated += new_len - buf.len;
                } else {
                    gca.bytes_allocated -= buf.len - new_len;
                }
            }
            return true;
        }
        std.debug.assert(new_len > buf.len);
        return false;
        //return gca.parent_allocator.rawResize(buf, buf_align, new_len, ret_address);
    }

    //fn remap(ctx: *anyopaque, memory: []u8, alignment: std.mem.Alignment, new_len: usize, ret_addr: usize) ?[*]u8 {
    //_ = ctx;
    //_ = memory;
    //_ = new_len;
    //_ = alignment;
    //_ = ret_addr;
    //return null;
    //}

    fn remap(ctx: *anyopaque, memory: []u8, alignment: std.mem.Alignment, new_len: usize, ret_addr: usize) ?[*]u8 {
        _ = alignment;
        _ = ret_addr;
        const gca: *GCAllocator = @ptrCast(@alignCast(ctx));

        // Calculate the change in memory size.
        const delta_bytes = if (new_len > memory.len) new_len - memory.len else 0;

        // Trigger garbage collection if needed before allocating more memory.
        if ((gca.bytes_allocated + delta_bytes > gca.next_gc) or debug.stress_gc) {
            gca.collectGarbage() catch return null;
        }

        // Use the parent allocator to perform the remapping.
        const new_ptr = gca.parent_allocator.realloc(memory, new_len) catch return null orelse {
            // If realloc fails, return null.
            return null;
        };

        // Update the allocated bytes counter.
        if (!gca.is_collecting) {
            if (new_len > memory.len) {
                gca.bytes_allocated += delta_bytes;
            } else {
                gca.bytes_allocated -= memory.len - new_len;
            }
        }

        return new_ptr.ptr;
    }

    fn free(ctx: *anyopaque, buf: []u8, buf_align: std.mem.Alignment, ret_address: usize) void {
        const gca: *GCAllocator = @ptrCast(@alignCast(ctx));
        gca.parent_allocator.rawFree(buf, buf_align, ret_address);
        gca.bytes_allocated -= buf.len;
    }

    // Garbage collector implementation

    fn markCompilerRoots(gca: *GCAllocator) !void {
        const vm = gca.vm orelse return;
        if (vm.parser) |p| {
            var comp: ?*Compiler = p.compiler;
            while (comp) |c| {
                try c.func.obj.mark(vm);
                comp = c.enclosing;
            }
        }
    }

    fn markRoots(gca: *GCAllocator) !void {
        const vm = gca.vm orelse return;

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
        try gca.markCompilerRoots();
    }

    fn traceReferences(gca: *GCAllocator) !void {
        const vm = gca.vm orelse return;

        while (vm.gray_stack.items.len > 0) {
            // NOTE: Unwrapping here
            const object = vm.gray_stack.pop().?;

            if (comptime debug.log_gc) std.debug.print("GS: {} {any}\n", .{ object.obj_t, object.is_marked });
            try object.blacken(vm);
        }
    }

    fn sweep(gca: *GCAllocator) void {
        const vm = gca.vm orelse return;
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
                //std.debug.print("CALLED unreached.destroy()\n", .{});
                gca.bytes_allocated -= @sizeOf(@TypeOf(unreached));
                unreached.destroy(vm);
            }
        }
    }

    pub fn collectGarbage(gca: *GCAllocator) !void {
        if (gca.is_collecting) return;
        gca.is_collecting = true;
        defer gca.is_collecting = false;

        const vm = gca.vm orelse return;
        const before = gca.bytes_allocated;

        if (comptime debug.log_gc) std.debug.print("BEFORE {d}\n", .{before});
        if (comptime debug.log_gc) std.debug.print("BYTES ALLOCATED {d}\n", .{gca.bytes_allocated});
        if (comptime debug.log_gc) std.debug.print("-- gc begin\n", .{});

        try gca.markRoots();
        try gca.traceReferences();
        vm.globals.removeWhite();
        vm.strings.removeWhite();
        gca.sweep();

        gca.next_gc = gca.bytes_allocated * GC_HEAP_GROW_FACTOR;

        if (comptime debug.log_gc) std.debug.print(
            "-- gc end\ncollected {d} bytes (from {d} to {d}) next at {d}\n",
            .{ before - gca.bytes_allocated, before, gca.bytes_allocated, gca.next_gc },
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
