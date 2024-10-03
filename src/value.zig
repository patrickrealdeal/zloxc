const std = @import("std");
const Obj = @import("object.zig").Obj;

pub const Tag = enum {
    bool,
    number,
    obj,
    nil,
};

pub const Value = union(Tag) {
    bool: bool,
    number: f64,
    obj: *Obj,
    nil,

    pub fn printValue(self: Value) void {
        switch (self) {
            .bool => |b| std.debug.print("{any}", .{b}),
            .number => |n| std.debug.print("{d}", .{n}),
            .obj => |obj| std.debug.print("{s}", .{obj.asString().bytes}),
            .nil => std.debug.print("nil", .{}),
        }
    }

    pub fn asObj(self: Value) *Obj {
        std.debug.assert(std.meta.activeTag(self) == .obj);
        return self.obj;
    }

    pub fn isObj(self: Value) bool {
        return self == .obj;
    }

    pub fn asNumber(self: Value) f64 {
        std.debug.assert(std.meta.activeTag(self) == .number);
        return self.number;
    }

    pub fn isNumber(self: Value) bool {
        return self == .number;
    }

    pub fn isString(self: Value) bool {
        return self.isObj() and self.asObj().is(.string);
    }

    pub fn eq(a: Value, b: Value) bool {
        if (std.meta.activeTag(a) == .obj and std.meta.activeTag(b) == .obj) {
            return std.mem.eql(u8, a.obj.asString().bytes, b.obj.asString().bytes);
        }

        // std.meta.eql checks Tag equality
        // and if present compares values
        return std.meta.eql(a, b);
    }
};

test "size of a Value" {
    try std.testing.expect(@sizeOf(Value) == 16);
}
