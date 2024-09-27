const std = @import("std");

pub const Tag = enum {
    bool,
    number,
    nil,
};

pub const Value = union(Tag) {
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

    /// Inspired by zigs own implementaion of std.meta.eql, clean
    pub fn eq(a: Value, b: Value) bool {
        const info = @typeInfo(@TypeOf(a)).@"union";
        const at = std.meta.activeTag(a);
        const bt = std.meta.activeTag(b);
        if (at != bt) return false;

        inline for (info.fields) |field_info| {
            if (@field(info.tag_type.?, field_info.name) == at) {
                // std.meta.eql first compares Tag the recursevly compares values if Tag is eql
                return std.meta.eql(@field(a, field_info.name), @field(b, field_info.name));
            }
        }
        // When using `inline for` the compiler doesn't know that every
        // possible case has been handled requiring an explicit `unreachable`.
        unreachable;
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
