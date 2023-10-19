const std = @import("std");

pub const ValueType = enum {
    Bool,
    Nil,
    Number,
};

pub const Value = union(ValueType) {
    Bool: bool,
    Number: f64,
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

    pub fn nil() Value {
        return .Nil;
    }

    pub fn fromBool(b: bool) Value {
        return Value{ .Bool = b };
    }

    pub fn fromNumber(n: f64) Value {
        return Value{ .Number = n };
    }

    pub fn equals(self: Value, other: Value) bool {
        switch (self) {
            .Number => return if (other.isNumber()) self.asNumber() == other.asNumber() else false,
            .Bool => return if (other.isBool()) self.asBool() == other.asBool() else false,
            .Nil => return true,
        }

        return false;
    }

    pub fn isFalsey(self: Value) bool {
        if (self.isBool()) return !self.asBool();
        if (self.isNil()) return true;
        return false;
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
        }
    }
};
