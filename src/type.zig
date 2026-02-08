const std = @import("std");

/// Type information for the language
pub const Type = union(enum) {
    int,
    float,
    bool,
    string,
    nil,
    function: FunctionType,
    inferred, // For type inference with :=
    unknown, // Error recovery

    pub const FunctionType = struct {
        params: []Type,
        return_type: *Type,
    };

    /// Check if two types are equal
    pub fn eql(a: Type, b: Type) bool {
        if (@as(std.meta.Tag(Type), a) != @as(std.meta.Tag(Type), b)) return false;

        return switch (a) {
            .int, .float, .bool, .string, .nil, .inferred, .unknown => true,
            .function => |a_fn| {
                const b_fn = b.function;
                if (a_fn.params.len != b_fn.params.len) return false;
                if (!a_fn.return_type.eql(b_fn.return_type.*)) return false;

                for (a_fn.params, b_fn.params) |a_param, b_param| {
                    if (!a_param.eql(b_param)) return false;
                }
                return true;
            },
        };
    }

    /// Check if a value can be assigned to this type
    pub fn isAssignableFrom(self: Type, other: Type) bool {
        // Unknown for error recovery
        if (self == .unknown or other == .unknown) return true;

        // Nil can be assigned to any type (like null in many languages)
        if (other == .nil) return true;

        // Allow implicit int to float conversion
        if (self == .float and other == .int) return true;

        return self.eql(other);
    }

    /// Convert type to string for error messages
    pub fn toString(self: Type, allocator: std.mem.Allocator) ![]const u8 {
        return switch (self) {
            .int => try allocator.dupe(u8, "int"),
            .float => try allocator.dupe(u8, "float"),
            .bool => try allocator.dupe(u8, "bool"),
            .string => try allocator.dupe(u8, "string"),
            .nil => try allocator.dupe(u8, "nil"),
            .inferred => try allocator.dupe(u8, "<inferred>"),
            .unknown => try allocator.dupe(u8, "<unknown>"),
            .function => |f| {
                var buf: std.ArrayList(u8) = .empty;
                try buf.appendSlice(allocator, "fn(");
                for (f.params, 0..) |param, i| {
                    const param_str = try param.toString(allocator);
                    defer allocator.free(param_str);
                    try buf.appendSlice(allocator, param_str);
                    if (i < f.params.len - 1) try buf.appendSlice(allocator, ", ");
                }
                try buf.appendSlice(allocator, ") -> ");
                const ret_str = try f.return_type.toString(allocator);
                defer allocator.free(ret_str);
                try buf.appendSlice(allocator, ret_str);
                return buf.toOwnedSlice(allocator);
            },
        };
    }
};

/// Type annotation in source code
pub const TypeAnnotation = struct {
    type_kind: Type,
    is_mutable: bool, // true for 'var', false for 'let'
};

