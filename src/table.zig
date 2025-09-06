const std = @import("std");
const Value = @import("value.zig").Value;
const String = @import("object.zig").String;
const VM = @import("vm.zig");

const table_max_load = 75;

pub fn Table(comptime KeyType: type, comptime ValueType: type) type {
    return struct {
        hm: std.HashMap(KeyType, ValueType, Ctx, table_max_load),
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
            return .{ .hm = std.HashMap(KeyType, ValueType, Ctx, table_max_load).init(allocator) };
        }

        pub fn deinit(table: HT) void {
            table.hm.deinit();
        }

        pub fn findString(table: HT, chars: []const u8, hash: u32) ?*String {
            var s = String{ .obj = undefined, .hash = hash, .bytes = chars };
            return table.hm.getKey(&s);
        }

        pub fn set(table: HT, key: KeyType, value: ValueType) !bool {
            return if (table.hm.fetchPut(key, value) catch {
                std.debug.print("OOME: can't put the key to a table", .{});
                return VM.VMError.OutOfMemory;
            }) |_|
                false // key is not new
            else
                true;
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
                if (!kv.key_ptr.*.isMarked()) {
                    std.debug.print("!!!removed: {s}\n", .{kv.key_ptr.*.bytes});
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
