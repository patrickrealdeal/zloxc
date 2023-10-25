const std = @import("std");
const Allocator = std.mem.Allocator;
const Obj = @import("./object.zig").Obj;
const Value = @import("./value.zig").Value;

const Entry = struct {
    key: ?*Obj.String,
    value: Value,

    pub fn isTombstone(self: *Entry) bool {
        if (self.key != null) {
            return false;
        } else {
            return (!self.value.isNil());
        }
    }
};

pub const Table = struct {
    allocator: Allocator,
    count: usize,
    entries: []Entry,

    pub fn init(allocator: Allocator) Table {
        return Table{
            .allocator = allocator,
            .count = 0,
            .entries = &[_]Entry{},
        };
    }

    pub fn deinit(self: *Table) void {
        self.allocator.free(self.entries);
    }

    // adds the given key/value pair to the given hash table.
    // If an entry for that key is already present, the new value overwrites the old value.
    pub fn set(self: *Table, key: *Obj.String, value: Value) !bool {
        // Encodes a 75% capacity
        if (4 * (self.count + 1) > 3 * self.entries.len) {
            try self.adjustCapacity();
        }

        const entry = findEntry(self.entries, key);
        const isNewKey = entry.key == null;

        if (isNewKey) {
            if (!entry.isTombstone()) self.count += 1;
        }

        entry.key = key;
        entry.value = value;

        return isNewKey;
    }

    // We take the Entry’s value and copy it to the output parameter so the caller can get it.
    pub fn get(self: *Table, key: *Obj.String, value: *Value) bool {
        if (self.entries.len == 0) return false;

        const entry = findEntry(self.entries, key);
        if (entry.key == null) return false;
        value.* = entry.value;

        return true;
    }

    pub fn delete(self: *Table, key: *Obj.String) !bool {
        if (self.entries.len == 0) return false;

        // Find entry
        const entry = findEntry(self.entries, key);
        if (entry.key == null) return false;

        // Place a Tombstone in the entry to keep the collisin chain intact
        // as a null key with a true value
        entry.key = null;
        entry.value = Value.fromBool(true);
        return true;
    }

    pub fn findString(self: *Table, bytes: []const u8, hash: u32) ?*Obj.String {
        const entries = self.entries;
        if (entries.len == 0) return null;

        var index = hash & (entries.len - 1); // mod operation with bits

        while (true) {
            var entry = entries[index];

            if (entry.key) |entrykey| {
                if (entrykey.hash == hash and std.mem.eql(u8, entrykey.bytes, bytes)) {
                    return entrykey;
                }
            } else if (!entry.isTombstone()) {
                // Stop if we find and empty non-tombstone entry
                return null;
            }

            index = (index + 1) & (entries.len - 1);
        }
    }

    fn findEntry(entries: []Entry, key: *Obj.String) *Entry {
        var index = key.hash & (entries.len - 1);
        var tombstone: ?*Entry = null;

        // Linear probing
        while (true) {
            const entry = &entries[index];
            if (entry.key) |entrykey| {
                if (entrykey == key) return entry;
            } else {
                if (entry.isTombstone()) {
                    if (tombstone == null) tombstone = entry; // found and recicle tombstone
                } else {
                    return tombstone orelse entry;
                }
            }

            index = (index + 1) & (entries.len - 1);
        }
    }

    fn adjustCapacity(self: *Table) !void {
        const capacity = if (self.entries.len < 8) 8 else self.entries.len * 2;
        const entries = try self.allocator.alloc(Entry, capacity);

        // init new entries
        for (entries) |*entry| {
            entry.key = null;
            entry.value = Value.nil();
        }

        // copy old entries in new entries
        self.count = 0; // reset count to not count tombstones in
        for (self.entries) |entry| {
            if (entry.key) |key| {
                var dest = findEntry(entries, key);
                dest.key = key;
                dest.value = entry.value;
                self.count += 1;
            }
        }

        // release mem of old array
        self.allocator.free(self.entries);

        self.entries = entries;
    }

    fn copy(self: *Table, to: *Table) !void {
        for (self.entries) |entry| {
            if (entry.key) |key| {
                to.set(key, entry.value);
            }
        }
    }
};
