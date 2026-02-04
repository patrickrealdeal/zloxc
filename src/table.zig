const std = @import("std");
const Value = @import("value.zig").Value;
const String = @import("object.zig").String;
const VM = @import("vm.zig");

/// Generic table for any key/value pair with GC integration
/// Replaces your old Table(KeyType, ValueType) generic
pub fn Table(comptime K: type, comptime V: type) type {
    return struct {
        map: std.AutoHashMapUnmanaged(K, V),
        allocator: std.mem.Allocator,

        const Self = @This();

        pub fn init(allocator: std.mem.Allocator) Self {
            return .{
                .map = .{},
                .allocator = allocator,
            };
        }

        pub fn deinit(self: *Self) void {
            self.map.deinit(self.allocator);
        }

        pub fn set(self: *Self, key: K, value: V) !bool {
            const result = try self.map.fetchPut(self.allocator, key, value);
            return result == null; // true if new key was added
        }

        pub fn get(self: *Self, key: K) ?V {
            return self.map.get(key);
        }

        pub fn delete(self: *Self, key: K) bool {
            return self.map.remove(key);
        }

        /// Mark all entries for GC (only called on tables with Obj pointers)
        pub fn mark(self: *Self, vm: *VM) !void {
            var it = self.map.iterator();
            while (it.next()) |entry| {
                // Mark both key and value if they're objects
                if (@typeInfo(K) == .pointer) {
                    const key_ptr: *const anyopaque = entry.key_ptr.*;
                    if (comptime isObjType(K)) {
                        const obj = @as(*String, @ptrCast(@alignCast(@constCast(key_ptr))));
                        try obj.obj.mark(vm);
                    }
                }
                if (@typeInfo(V) == .pointer or @TypeOf(entry.value_ptr.*) == Value) {
                    if (comptime isValueType(V)) {
                        try entry.value_ptr.mark(vm);
                    }
                }
            }
        }

        /// Remove unmarked entries (only for GC)
        pub fn removeWhite(self: *Self) void {
            var to_remove: std.ArrayList(K) = .empty;
            defer to_remove.deinit(self.allocator);

            var it = self.map.iterator();
            while (it.next()) |entry| {
                var should_remove = false;

                // Check if key is unmarked (for String keys)
                if (comptime isObjType(K)) {
                    const key_obj = entry.key_ptr.*;
                    if (!key_obj.obj.is_marked) {
                        should_remove = true;
                    }
                }

                // Check if value is unmarked (for Value or Obj values)
                if (!should_remove and comptime isValueType(V)) {
                    const val = entry.value_ptr.*;
                    if (val.isObj() and !val.asObj().is_marked) {
                        should_remove = true;
                    }
                }

                if (should_remove) {
                    to_remove.append(self.allocator, entry.key_ptr.*) catch continue;
                }
            }

            for (to_remove.items) |key| {
                _ = self.map.remove(key);
            }
        }

        /// Find an interned string (for string table only)
        pub fn findString(self: *Self, bytes: []const u8, hash: u32) ?*String {
            if (comptime K != *String) {
                @compileError("findString only works with Table(*String, V)");
            }

            var it = self.map.keyIterator();
            while (it.next()) |key_ptr| {
                const str = key_ptr.*;
                if (str.hash == hash and std.mem.eql(u8, str.bytes, bytes)) {
                    return str;
                }
            }
            return null;
        }

        // Helper compile-time checks
        fn isObjType(comptime T: type) bool {
            return T == *String; // Extend this if you have other object types as keys
        }

        fn isValueType(comptime T: type) bool {
            return T == Value;
        }
    };
}

// Type aliases for convenience - use these in your code!
pub const StringTable = Table(*String, Value); // For interning strings
pub const GlobalsTable = Table(*String, Value); // For global variables

// Helper to create tables
pub fn createStringTable(allocator: std.mem.Allocator) StringTable {
    return StringTable.init(allocator);
}

pub fn createGlobalsTable(allocator: std.mem.Allocator) GlobalsTable {
    return GlobalsTable.init(allocator);
}

