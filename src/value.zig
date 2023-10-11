const std = @import("std");

pub const Value = union(enum) {
    Number: f64,

    pub fn isNumber(self: Value) bool {
        return self == .Number;
    }

    pub fn asNumber(self: Value) f64 {
        std.debug.assert(self.isNumber());
        return self.Number;
    }

    pub fn format(self: Value, comptime fmt: []const u8, options: std.fmt.FormatOptions, out_stream: anytype) !void {
        _ = fmt;
        _ = options;
        switch (self) {
            .Number => |value| try out_stream.print("'{d}'\n", .{value}),
        }
    }
};
