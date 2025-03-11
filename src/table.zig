const std = @import("std");
const Value = @import("value.zig").Value;
const String = @import("object.zig").String;
const VM = @import("vm.zig");

const table_max_load = 75;
const Self = @This();

pub fn Table(comptime KeyType: type, comptime ValueType: type) type {
    return struct {
        hm: std.HashMap(KeyType, ValueType, Ctx, table_max_load),

        const Ctx = struct {
            pub fn hash(_: Ctx, key: KeyType) u64 {
                return @intCast(key.getHash());
            }

            pub fn eql(_: Ctx, a: KeyType, b: KeyType) bool {
                return a.getHash() == b.getHash() and a.equal(b);
            }
        };

        pub fn init(allocator: std.mem.Allocator) @This() {
            return .{ .hm = std.HashMap(KeyType, ValueType, Ctx, table_max_load).init(allocator) };
        }

        pub fn deinit(self: *@This()) void {
            self.hm.deinit();
        }

        pub fn findString(self: *@This(), chars: []const u8, hash: u32) ?*String {
            var s = String{ .obj = undefined, .hash = hash, .bytes = chars };
            return self.hm.getKey(&s);
        }

        pub fn set(self: *@This(), key: KeyType, value: ValueType) !bool {
            return if (self.hm.fetchPut(key, value) catch {
                std.debug.print("OOME: can't put the key to a table", .{});
                return VM.VmError.OutOfMemory;
            }) |_|
                false // key is not new
            else
                true;
        }

        pub fn get(self: *@This(), key: KeyType) ?ValueType {
            return self.hm.get(key);
        }

        pub fn delete(self: *@This(), key: KeyType) bool {
            return self.hm.remove(key);
        }

        pub fn removeWhite(self: *@This()) void {
            var it = self.hm.iterator();
            while (it.next()) |kv| {
                if (!kv.key_ptr.*.obj.is_marked) {
                    std.debug.print("!!!removed: {s}\n", .{kv.key_ptr.*.bytes});
                    _ = self.delete(kv.key_ptr.*);
                }
            }
        }

        pub fn mark(self: *@This(), vm: *VM) !void {
            var it = self.hm.iterator();
            while (it.next()) |kv| {
                try kv.key_ptr.*.mark(vm);
                try kv.value_ptr.*.mark(vm);
            }
        }
    };
}
