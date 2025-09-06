const std = @import("std");
const Obj = @import("object.zig");
const VM = @import("vm.zig");
const NAN_BOXING = @import("debug.zig").NAN_BOXING;

pub const Value = if (NAN_BOXING) NanBoxedValue else UnionValue;

pub const NanBoxedValue = packed struct {
    bits: u64,

    const QNAN: u64 = 0x7ff8000000000000;
    const SIGN_BIT: u64 = 0x8000000000000000;

    pub const NIL_VAL: NanBoxedValue = .{ .bits = QNAN | 1 };
    pub const FALSE_VAL: NanBoxedValue = .{ .bits = QNAN | 2 };
    pub const TRUE_VAL: NanBoxedValue = .{ .bits = QNAN | 3 };

    pub fn isNumber(v: NanBoxedValue) bool {
        // A value is a number if its bits do NOT form a quiet NaN pattern.
        return (v.bits & QNAN) != QNAN;
    }

    pub fn isNil(v: NanBoxedValue) bool {
        return v.bits == NIL_VAL.bits;
    }

    pub fn isBool(v: NanBoxedValue) bool {
        // This clever trick works because FALSE_VAL | 1 == TRUE_VAL
        return (v.bits | 1) == TRUE_VAL.bits;
    }

    pub fn isObj(v: NanBoxedValue) bool {
        // An object is anything that isn't a number, nil, or bool.
        // The pointer is stored directly in the bits.
        // Pointers have high bits as 0, so they don't look like QNAN.
        // We check for the SIGN_BIT to identify it as a tagged pointer.
        return (v.bits & (QNAN | SIGN_BIT)) == (QNAN | SIGN_BIT);
    }

    pub fn asNumber(v: NanBoxedValue) f64 {
        std.debug.assert(v.isNumber());
        return @bitCast(v.bits);
    }

    pub fn asBool(v: NanBoxedValue) bool {
        std.debug.assert(v.isBool());
        return v.bits == TRUE_VAL.bits;
    }

    pub fn asObj(v: NanBoxedValue) *Obj {
        std.debug.assert(v.isObj());
        // We clear the sign bit to get the raw pointer address.
        return @as(*Obj, @ptrFromInt(@as(u64, @intCast(v.bits & ~(SIGN_BIT | QNAN)))));
    }

    // CONSTRUCTORS
    pub fn fromNumber(n: f64) NanBoxedValue {
        return .{ .bits = @bitCast(n) };
    }

    pub fn fromBool(b: bool) NanBoxedValue {
        return if (b) TRUE_VAL else FALSE_VAL;
    }

    pub fn fromNil() NanBoxedValue {
        return .NIL_VAL;
    }

    pub fn fromObj(o: *Obj) NanBoxedValue {
        return .{ .bits = SIGN_BIT | QNAN | @intFromPtr(o) };
    }

    pub fn isFalsey(self: NanBoxedValue) bool {
        if (self.isBool()) return !self.asBool();
        if (self.isNil()) return true;
        return false;
    }

    pub fn eq(a: Value, b: Value) bool {
        if (a.isNumber() and b.isNumber()) {
            return a.asNumber() == b.asNumber();
        }
        // For nil, bools, and objects, a direct bit comparison is sufficient
        // as long as you've properly canonicalized strings during interning.
        return a.bits == b.bits;
    }

    pub fn format(self: Value, writer: *std.Io.Writer) !void {
        if (self.isNumber()) {
            try writer.print("{d}", .{self.asNumber()});
        } else if (self.isNil()) {
            try writer.print("nil", .{});
        } else if (self.isBool()) {
            try writer.print("{any}", .{self.asBool()});
        } else if (self.isObj()) {
            try printObj(self.asObj(), writer);
        } else {
            // Should not happen
            try writer.print("Unknown Value", .{});
        }
    }
};

pub const Tag = enum {
    bool,
    number,
    obj,
    nil,
};

pub const UnionValue = union(Tag) {
    bool: bool,
    number: f64,
    obj: *Obj,
    nil,

    pub const empty: Value = .nil;

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
        //if (std.meta.activeTag(a) == .obj and std.meta.activeTag(b) == .obj) {
        //return std.mem.eql(u8, a.obj.as(Obj.String).bytes, b.obj.as(Obj.String).bytes);
        //}
        //
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
