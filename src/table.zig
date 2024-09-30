const std = @import("std");
const Value = @import("value.zig").Value;
const String = @import("object.zig").String;
const VM = @import("vm.zig").VM;

const table_max_load = 0.75;
const Self = @This();

count: usize,
entries: []Entry,
allocator: std.mem.Allocator,

pub fn init(allocator: std.mem.Allocator) Self {
    return .{
        .count = 0,
        .entries = &[_]Entry{},
        .allocator = allocator,
    };
}
pub fn deinit(self: *Self) void {
    self.allocator.free(self.entries);
    self.count = 0;
}

/// This function adds the given key/value pair to the given hash table.
/// If an entry for that key is already present, the new value overwrites the old value.
/// The function returns true if a new entry was added.
pub fn set(self: *Self, key: *String, value: Value) bool {
    const cap = self.entries.len;
    if (@as(f64, @floatFromInt(self.count + 1)) > @as(f64, @floatFromInt(cap)) * table_max_load) {
        const new_cap = if (cap < 8) 8 else 2 * cap;
        self.adjustCapacity(new_cap);
    }

    const entry = findEntry(self.entries, key);
    const isNewKey = entry.key == null;
    if (isNewKey and entry.value == .nil) self.count += 1;

    entry.key = key;
    entry.value = value;
    return isNewKey;
}

pub fn get(self: *Self, key: *String) ?*Value {
    if (self.count == 0) return null;

    const entry = findEntry(self.entries, key);
    if (entry.key == null) return null;

    return &entry.value.?;
}

pub fn delete(self: *Self, key: *String) bool {
    if (self.count == 0) return false;

    const entry = findEntry(self.entries, key);
    if (entry.key == null) return false;

    // place tombstone in entry
    entry.key = null;
    entry.value = Value{ .bool = true };
    return true;
}

pub fn findString(self: *Self, bytes: []const u8, hash: u32) ?*String {
    if (self.count == 0) return null;
    var index = hash % self.entries.len;
    while (true) {
        const entry = &self.entries[index];
        if (entry.key == null and entry.value == .nil) return null;
        if (hash == entry.key.?.hash and std.mem.eql(u8, entry.key.?.bytes, bytes)) return entry.key.?;

        index = (index + 1) % self.count;
    }
}

fn adjustCapacity(self: *Self, new_cap: usize) void {
    const entries = self.allocator.alloc(Entry, new_cap) catch unreachable;
    for (entries) |*e| {
        e.* = Entry{ .key = null, .value = Value.nil };
    }

    self.count = 0;
    for (self.entries) |entry| {
        if (entry.key == null) continue;

        const dest = findEntry(entries, entry.key.?);
        dest.key = entry.key;
        dest.value = entry.value;
        self.count += 1;
    }
    self.allocator.free(self.entries);
    self.entries = entries;
}

fn findEntry(entries: []Entry, key: *String) *Entry {
    var index = key.hash % entries.len;
    var tombstone: ?*Entry = null;
    while (true) {
        const entry = &entries[index];
        if (entry.key == null) {
            if (entry.value == .nil) {
                // emptry entry
                return tombstone orelse entry;
            } else {
                // we found a tombstone
                if (tombstone == null) tombstone = entry;
            }
        } else if (entry.key == key) {
            // we found the key
            return entry;
        }

        index = (index + 1) % entries.len;
    }
}

fn addALl(from: *Self, to: *Self) void {
    for (from.entries) |entry| {
        if (entry.key != null) {
            set(to, entry.key, entry.value);
        }
    }
}

const Entry = struct {
    key: ?*String,
    value: Value,
};

test "table" {
    var vm = VM.init(std.testing.allocator);
    defer vm.deinit();
    const str = try String.take(&vm, "hello");
    _ = vm.strings.set(str, Value{ .number = 24 });
    const entry = findEntry(vm.strings.entries, str);
    // const res = vm.strings.get(str);

    //try std.testing.expect(std.mem.eql(u8, res, "hello"));
    try std.testing.expect(entry.key == str);
}

test "entry" {
    const entry = Entry{ .key = null, .value = Value.nil };
    try std.testing.expect(entry.key == null);
}
