const std = @import("std");
const Obj = @import("object.zig");
const VM = @import("vm.zig");
const NAN_BOXING = @import("debug.zig").NAN_BOXING;

pub const Value = if (NAN_BOXING) NanBoxedValue else UnionValue;

// NAN-BOXED VALUE (8 bytes)
pub const NanBoxedValue = packed struct {
    bits: u64,

    const QNAN: u64 = 0x7ffc000000000000;
    const SIGN_BIT: u64 = 0x8000000000000000;
    const TAG_NIL: u64 = 1;
    const TAG_FALSE: u64 = 2;
    const TAG_TRUE: u64 = 3;

    pub const nil = NanBoxedValue{ .bits = QNAN | TAG_NIL };
    pub const true_val = NanBoxedValue{ .bits = QNAN | TAG_TRUE };
    pub const false_val = NanBoxedValue{ .bits = QNAN | TAG_FALSE };

    // CONSTRUCTORS
    pub fn fromNumber(n: f64) NanBoxedValue {
        return .{ .bits = @bitCast(n) };
    }

    pub fn fromBool(b: bool) NanBoxedValue {
        return if (b) true_val else false_val;
    }

    pub fn fromNil() NanBoxedValue {
        return nil;
    }

    pub fn fromObj(o: *Obj) NanBoxedValue {
        return .{ .bits = SIGN_BIT | QNAN | @intFromPtr(o) };
    }

    // TYPE CHECKS
    pub fn isNumber(self: NanBoxedValue) bool {
        return (self.bits & QNAN) != QNAN;
    }

    pub fn isBool(self: NanBoxedValue) bool {
        return (self.bits | 1) == true_val.bits;
    }

    pub fn isNil(self: NanBoxedValue) bool {
        return self.bits == nil.bits;
    }

    pub fn isObj(self: NanBoxedValue) bool {
        return (self.bits & (QNAN | SIGN_BIT)) == (QNAN | SIGN_BIT);
    }

    pub inline fn is(self: NanBoxedValue, comptime t: @TypeOf(.enum_literal)) bool {
        if (comptime @hasField(Obj.ObjType, @tagName(t))) {
            return self.isObj() and self.asObj().obj_t == t;
        } else {
            return switch (t) {
                .number => self.isNumber(),
                .bool => self.isBool(),
                .nil => self.isNil(),
                .obj => self.isObj(),
                else => false,
            };
        }
    }

    // VALUE EXTRACTORS
    pub fn asNumber(self: NanBoxedValue) f64 {
        std.debug.assert(self.isNumber());
        return @bitCast(self.bits);
    }

    pub fn asBool(self: NanBoxedValue) bool {
        std.debug.assert(self.isBool());
        return self.bits == true_val.bits;
    }

    pub fn asObj(self: NanBoxedValue) *Obj {
        std.debug.assert(self.isObj());
        // NOT inverts bit, keeps the address data
        // & masks and extraxcts the address bits
        const pointer_int = self.bits & ~(SIGN_BIT | QNAN);
        return @ptrFromInt(pointer_int);
    }

    // UTILITIES
    pub fn isFalsey(self: NanBoxedValue) bool {
        if (self.isBool()) return !self.asBool();
        if (self.isNil()) return true;
        return false;
    }

    pub fn eq(a: NanBoxedValue, b: NanBoxedValue) bool {
        if (a.isNumber() and b.isNumber()) {
            return a.asNumber() == b.asNumber();
        }
        return a.bits == b.bits;
    }

    pub fn mark(self: NanBoxedValue, vm: *VM) !void {
        if (self.isObj()) {
            try self.asObj().mark(vm);
        }
    }

    pub fn format(self: NanBoxedValue, writer: anytype) !void {
        if (self.isNumber()) {
            try writer.print("{d}", .{self.asNumber()});
        } else if (self.isNil()) {
            try writer.print("nil", .{});
        } else if (self.isBool()) {
            try writer.print("{}", .{self.asBool()});
        } else if (self.isObj()) {
            try printObj(self.asObj(), writer);
        } else {
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

// UNION VALUE (16 bytes)
pub const UnionValue = union(Tag) {
    bool: bool,
    number: f64,
    obj: *Obj,
    nil,

    pub const Nil = UnionValue{ .nil = {} };
    pub const true_val = UnionValue{ .bool = true };
    pub const false_val = UnionValue{ .bool = false };

    // CONSTRUCTORS
    pub fn fromNumber(n: f64) UnionValue {
        return .{ .number = n };
    }

    pub fn fromBool(b: bool) UnionValue {
        return .{ .bool = b };
    }

    pub fn fromNil() UnionValue {
        return .nil;
    }

    pub fn fromObj(o: *Obj) UnionValue {
        return .{ .obj = o };
    }

    // TYPE CHECKS
    pub fn isNumber(self: UnionValue) bool {
        return self == .number;
    }

    pub fn isBool(self: UnionValue) bool {
        return self == .bool;
    }

    pub fn isNil(self: UnionValue) bool {
        return self == .nil;
    }

    pub fn isObj(self: UnionValue) bool {
        return self == .obj;
    }

    pub inline fn is(self: UnionValue, comptime t: @TypeOf(.enum_literal)) bool {
        if (comptime @hasField(Obj.ObjType, @tagName(t))) {
            return self == .obj and self.obj.obj_t == t;
        } else {
            return self == t;
        }
    }

    // VALUE EXTRACTORS
    pub fn asNumber(self: UnionValue) f64 {
        std.debug.assert(self == .number);
        return self.number;
    }

    pub fn asBool(self: UnionValue) bool {
        std.debug.assert(self == .bool);
        return self.bool;
    }

    pub fn asObj(self: UnionValue) *Obj {
        std.debug.assert(self == .obj);
        return self.obj;
    }

    // UTILITIES
    pub fn isFalsey(self: UnionValue) bool {
        return switch (self) {
            .bool => |b| !b,
            .nil => true,
            else => false,
        };
    }

    pub fn eq(a: UnionValue, b: UnionValue) bool {
        return std.meta.eql(a, b);
    }

    pub fn mark(self: UnionValue, vm: *VM) !void {
        switch (self) {
            .obj => |obj| try obj.mark(vm),
            else => {},
        }
    }

    pub fn format(self: UnionValue, writer: anytype) !void {
        switch (self) {
            .bool => |b| try writer.print("{}", .{b}),
            .number => |n| try writer.print("{d}", .{n}),
            .obj => |obj| try printObj(obj, writer),
            .nil => try writer.print("nil", .{}),
        }
    }
};

// SHARED UTILITIES
pub fn printObj(obj: *Obj, writer: anytype) !void {
    switch (obj.obj_t) {
        .string => try writer.print("{s}", .{obj.as(Obj.String).bytes}),
        .function => {
            const name = if (obj.as(Obj.Function).name) |str| str.bytes else "script";
            try writer.print("<fn {s}>", .{name});
        },
        .native => try writer.print("<native fn>", .{}),
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
    if (NAN_BOXING) {
        try std.testing.expect(@sizeOf(Value) == 8);
    } else {
        try std.testing.expect(@sizeOf(Value) == 16);
    }
}
