const std = @import("std");
const Allocator = std.mem.Allocator;

pub fn Stack(comptime T: type) type {
    return struct {
        buffer: []T,
        items: []T,
        allocator: Allocator,

        const Self = Stack(T);

        pub fn init(allocator: Allocator, capacity: usize) !Self {
            var buffer = try allocator.alloc(T, capacity);

            return Self{
                .buffer = buffer,
                .items = buffer[0..0],
                .allocator = allocator,
            };
        }

        pub fn deinit(self: *Self) void {
            self.allocator.free(self.buffer);
        }

        pub fn append(self: *Self, item: T) void {
            std.debug.assert(self.items.len < self.buffer.len);

            self.items = self.buffer[0 .. self.items.len + 1]; // set pointer to next element in the buffer
            self.items[self.items.len - 1] = item; // set the previous to the item we are pushing in the stack
        }

        pub fn pop(self: *Self) T {
            const val = self.items[self.items.len - 1];
            self.items = self.buffer[0 .. self.items.len - 1];
            return val;
        }
    };
}
