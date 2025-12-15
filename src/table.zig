const std = @import("std");
const Value = @import("value.zig").Value;
const String = @import("object.zig").String;
const VM = @import("vm.zig");
const debug = @import("debug.zig");

const table_max_load = 75;

pub fn Table(comptime KeyType: type, comptime ValueType: type) type {
    return struct {
        hm: std.HashMapUnmanaged(KeyType, ValueType, Ctx, table_max_load),
        allocator: std.mem.Allocator,

        const HT = *@This();

        const Ctx = struct {
            pub fn hash(_: Ctx, key: KeyType) u64 {
                return @intCast(key.getHash());
            }

            pub fn eql(_: Ctx, a: KeyType, b: KeyType) bool {
                return a.getHash() == b.getHash() and a.equal(b);
            }
        };

        pub fn init(allocator: std.mem.Allocator) @This() {
            var hm = std.HashMapUnmanaged(KeyType, ValueType, Ctx, table_max_load).empty;
            hm.ensureTotalCapacity(allocator, 8) catch unreachable;

            return .{ .hm = hm, .allocator = allocator };
        }

        pub fn deinit(table: HT) void {
            table.hm.deinit(table.allocator);
        }

        pub fn findString(table: HT, chars: []const u8, hash: u32) ?*String {
            var s = String{ .obj = undefined, .hash = hash, .bytes = chars };
            return table.hm.getKey(&s);
        }

        pub fn set(table: HT, key: KeyType, value: ValueType) !bool {
            return if (table.hm.fetchPut(table.allocator, key, value) catch {
                std.debug.print("OOME: can't put the key to a table", .{});
                return error.VmOutOfMemory;
            }) |_| false else true;
        }

        pub fn get(table: HT, key: KeyType) ?ValueType {
            return table.hm.get(key);
        }

        pub fn delete(table: HT, key: KeyType) bool {
            return table.hm.remove(key);
        }

        pub fn removeWhite(table: HT) void {
            var it = table.hm.iterator();
            while (it.next()) |kv| {
                // We check the key, as string tables often have nil values.
                if (!kv.key_ptr.*.isMarked()) {
                    // This removes the entry from the HashMap's internal array,
                    // but does NOT free the memory for the key or value objects themselves.
                    _ = table.delete(kv.key_ptr.*);
                }
            }
        }

        pub fn mark(table: HT, vm: *VM) !void {
            var it = table.hm.iterator();
            while (it.next()) |kv| {
                try kv.key_ptr.*.mark(vm);
                try kv.value_ptr.*.mark(vm);
            }
        }
    };
}
