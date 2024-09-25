const std = @import("std");

const ValueType = enum {
    bool,
    number,
    nil,
};

pub const Value = union(ValueType) {
    bool: bool,
    number: f64,
    nil,

    pub fn printValue(self: Value) void {
        switch (self) {
            .bool => |b| std.debug.print("{any}", .{b}),
            .number => |n| std.debug.print("{d}", .{n}),
            .nil => std.debug.print("nil", .{}),
        }
    }

    pub fn eq(a: Value, b: Value) bool {
        const at = @as(ValueType, a);
        const bt = @as(ValueType, b);
        if (at != bt) return false;

        return switch (a) {
            .number => |n| n == b.number,
            .bool => |b1| b1 == b.bool,
            .nil => true,
        };
    }
};

test "size of a Value" {
    try std.testing.expect(@sizeOf(Value) == 16);
}
test "use of union with tags" {
    const v = Value{ .bool = true };
    try std.testing.expect(v == .bool);
    try std.testing.expect(v != .number);
    try std.testing.expect(v != .nil);
    try std.testing.expect(v.bool);
}
