const std = @import("std");
const Obj = @import("object.zig");

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

    pub fn format(self: Value, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = fmt;
        _ = options;
        switch (self) {
            .bool => |b| try writer.print("{any}", .{b}),
            .number => |n| try writer.print("{d}", .{n}),
            .obj => |obj| try printObj(obj, writer),
            .nil => try writer.print("nil", .{}),
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

    pub fn is(self: Value, t: Obj.ObjType) bool {
        return switch (t) {
            .string => return self.isObj() and self.asObj().is(.string),
            .function => return self.isObj() and self.asObj().is(.function),
            .native => return self.isObj() and self.asObj().is(.native),
            .closure => return self.isObj() and self.asObj().is(.closure),
            .upvalue => return self.isObj() and self.asObj().is(.upvalue),
        };
    }

    pub fn eq(a: Value, b: Value) bool {
        if (std.meta.activeTag(a) == .obj and std.meta.activeTag(b) == .obj) {
            return std.mem.eql(u8, a.obj.as(Obj.String).bytes, b.obj.as(Obj.String).bytes);
        }

        // std.meta.eql checks Tag equality
        // and if present compares values
        return std.meta.eql(a, b);
    }

    pub fn printObj(obj: *Obj, writer: anytype) !void {
        switch (obj.obj_t) {
            .string => try writer.print("{s}", .{obj.as(Obj.String).bytes}),
            .function => {
                const name = if (obj.as(Obj.Function).name) |str| str.bytes else "script";
                try writer.print("<fn {s}>", .{name});
            },
            .native => try writer.print("<native fn", .{}),
            .closure => {
                const name = if (obj.as(Obj.Closure).func.name) |str| str.bytes else "script";
                try writer.print("<fn {s}>", .{name});
            },
            .upvalue => try writer.print("upvalue", .{}),
        }
    }
};

test "size of a Value" {
    try std.testing.expect(@sizeOf(Value) == 16);
}
