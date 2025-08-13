const std = @import("std");
const Obj = @import("object.zig");
const VM = @import("vm.zig");

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

    pub fn format(self: Value, writer: *std.Io.Writer) !void {
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

    pub inline fn is(self: Value, comptime t: @TypeOf(.enum_literal)) bool {
        if (comptime @hasField(Obj.ObjType, @tagName(t))) {
            return self == .obj and self.obj.obj_t == t;
        } else {
            return self == t;
        }
    }

    pub fn mark(self: Value, vm: *VM) !void {
        switch (self) {
            .obj => |obj| try obj.mark(vm),
            else => {},
        }
    }

    pub fn eq(a: Value, b: Value) bool {
        if (std.meta.activeTag(a) == .obj and std.meta.activeTag(b) == .obj) {
            return std.mem.eql(u8, a.obj.as(Obj.String).bytes, b.obj.as(Obj.String).bytes);
        }

        // std.meta.eql checks Tag equality
        // and if present compares values
        return std.meta.eql(a, b);
    }
};

pub fn printObj(obj: *Obj, writer: *std.Io.Writer) !void {
    switch (obj.obj_t) {
        .string => try writer.print("{s}", .{obj.as(Obj.String).bytes}),
        .function => {
            const name = if (obj.as(Obj.Function).name) |str| str.bytes else "script";
            try writer.print("<fn {s}>", .{name});
        },
        .native => {
            try writer.print("<native fn>", .{});
        },
        .closure => {
            const name = if (obj.as(Obj.Closure).func.name) |str| str.bytes else "script";
            try writer.print("<fn {s}>", .{name});
        },
        .upvalue => try writer.print("upvalue", .{}),
        .class => {
            const name = obj.as(Obj.Class).name;
            try writer.print("class <{s}>", .{name.bytes});
        },
    }
}

test "size of a Value" {
    try std.testing.expect(@sizeOf(Value) == 16);
}
