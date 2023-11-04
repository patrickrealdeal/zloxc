const std = @import("std");
const Obj = @import("./object.zig").Obj;

pub const ValueType = enum {
    Bool,
    Number,
    Obj,
    Nil,
};

pub const Value = union(ValueType) {
    Bool: bool,
    Number: f64,
    Obj: *Obj,
    Nil,

    pub fn isBool(self: Value) bool {
        return self == .Bool;
    }

    pub fn isNil(self: Value) bool {
        return self == .Nil;
    }

    pub fn isNumber(self: Value) bool {
        return self == .Number;
    }

    pub fn asBool(self: Value) bool {
        std.debug.assert(self.isBool());
        return self.Bool;
    }

    pub fn asNumber(self: Value) f64 {
        std.debug.assert(self.isNumber());
        return self.Number;
    }

    pub fn isObj(self: Value) bool {
        return self == .Obj;
    }

    pub fn isObjType(self: Value, objType: Obj.Type) bool {
        return self.isObj() and self.asObj().objType == objType;
    }

    pub fn asObj(self: Value) *Obj {
        std.debug.assert(self.isObj());
        return self.Obj;
    }

    pub fn asObjType(self: Value, comptime objType: Obj.Type) *Obj.ObjType(objType) {
        return self.asObj().asObjType(objType);
    }

    pub fn nil() Value {
        return .Nil;
    }

    pub fn fromBool(b: bool) Value {
        return Value{ .Bool = b };
    }

    pub fn fromNumber(n: f64) Value {
        return Value{ .Number = n };
    }

    pub fn fromObj(x: *Obj) Value {
        return Value{ .Obj = x };
    }

    pub fn equals(self: Value, other: Value) bool {
        return switch (self) {
            .Number => return if (other.isNumber()) self.asNumber() == other.asNumber() else false,
            .Bool => return if (other.isBool()) self.asBool() == other.asBool() else false,
            .Nil => return true,
            .Obj => |x| return switch (other) {
                .Obj => |y| x == y,
                else => false,
            },
        };
    }

    pub fn isFalsey(self: Value) bool {
        return switch (self) {
            .Bool => |x| !x,
            .Nil => true,
            .Number => false,
            .Obj => false,
        };
    }

    pub fn format(self: Value, comptime fmt: []const u8, options: std.fmt.FormatOptions, out_stream: anytype) !void {
        _ = fmt;
        _ = options;
        switch (self) {
            .Number => |value| try out_stream.print("'{d}'", .{value}),
            .Bool => |b| {
                const value = if (b) "true" else "false";
                try out_stream.print("'{s}'", .{value});
            },
            .Nil => |_| try out_stream.print("'nil'", .{}),
            .Obj => |o| switch (o.objType) {
                .String => try out_stream.print("{s}", .{o.toString()}),
                .Function => {
                    const name = if (o.asFunction().name) |str| str.bytes else "<script>";
                    try out_stream.print("<fn {s}>", .{name});
                },
            },
        }
    }
};
