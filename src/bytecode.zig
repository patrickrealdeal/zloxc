const std = @import("std");
const Chunk = @import("chunk.zig");
const Value = @import("value.zig").Value;
const Obj = @import("object.zig");
const VM = @import("vm.zig");

const MAGIC: [4]u8 = .{ 'Z', 'L', 'O', 'X' };
const VERSION: u32 = 2; // Increment for new optimized format

pub const BytecodeSerializer = struct {
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator) BytecodeSerializer {
        return .{ .allocator = allocator };
    }

    pub fn serialize(self: *BytecodeSerializer, function: *Obj.Function, path: []const u8) !void {
        const file = try std.fs.cwd().createFile(path, .{});
        defer file.close();

        // Use larger buffer for better I/O performance
        var write_buffer: [16384]u8 = undefined;
        var file_writer = file.writer(&write_buffer);
        const writer = &file_writer.interface;

        try writer.writeAll(&MAGIC);
        try self.writeU32(writer, VERSION);
        try self.writeFunction(writer, function);
        try writer.flush();
    }

    pub fn deserialize(self: *BytecodeSerializer, vm: *VM, path: []const u8) !*Obj.Function {
        const file = try std.fs.cwd().openFile(path, .{});
        defer file.close();

        const file_size = (try file.stat()).size;

        // Align allocation to 8 bytes for better performance with f64 reads
        // Use @alignOf(f64) which is 8 on most platforms
        const aligned_size = std.mem.alignForward(usize, file_size, @alignOf(f64));
        const file_contents = try self.allocator.alignedAlloc(u8, .@"8", aligned_size);
        defer self.allocator.free(file_contents);

        const bytes_read = try file.readAll(file_contents[0..file_size]);
        if (bytes_read != file_size) return error.UnexpectedEof;

        var pos: usize = 0;

        // Read and validate magic (bounds check once)
        if (file_size < 8) return error.UnexpectedEof;
        if (!std.mem.eql(u8, file_contents[0..4], &MAGIC)) return error.InvalidBytecode;
        pos = 4;

        // Read version
        const version = std.mem.readInt(u32, file_contents[4..8], .little);
        if (version != VERSION) return error.IncompatibleVersion;
        pos = 8;

        return try self.readFunctionFromBuffer(file_contents[0..file_size], &pos, vm);
    }

    // Inline helpers for better performance
    inline fn writeU32(_: *BytecodeSerializer, writer: *std.Io.Writer, value: u32) !void {
        var buf: [4]u8 = undefined;
        std.mem.writeInt(u32, &buf, value, .little);
        try writer.writeAll(&buf);
    }

    inline fn writeU8(_: *BytecodeSerializer, writer: *std.Io.Writer, value: u8) !void {
        try writer.writeAll(&[1]u8{value});
    }

    // Use VarInt encoding for smaller integers (line numbers, small constants)
    fn writeVarInt(_: *BytecodeSerializer, writer: *std.Io.Writer, value: usize) !void {
        var v = value;
        while (v >= 0x80) {
            try writer.writeAll(&[1]u8{@as(u8, @truncate(v)) | 0x80});
            v >>= 7;
        }
        try writer.writeAll(&[1]u8{@as(u8, @truncate(v))});
    }

    fn writeFunction(self: *BytecodeSerializer, writer: *std.Io.Writer, function: *Obj.Function) anyerror!void {
        // Write name
        if (function.name) |name| {
            try self.writeVarInt(writer, name.bytes.len);
            try writer.writeAll(name.bytes);
        } else {
            try self.writeVarInt(writer, 0);
        }

        try self.writeU8(writer, @intCast(function.arity));
        try self.writeU8(writer, function.upvalue_count);
        try self.writeChunk(writer, &function.chunk);
    }

    fn writeChunk(self: *BytecodeSerializer, writer: *std.Io.Writer, chunk: *Chunk) !void {
        // Write bytecode (no compression - it's already dense)
        try self.writeVarInt(writer, chunk.code.items.len);
        try writer.writeAll(chunk.code.items);

        // Write line numbers with RLE compression
        try self.writeLineNumbers(writer, chunk.lines.items);

        // Write constants
        try self.writeVarInt(writer, chunk.constants.items.len);
        for (chunk.constants.items) |value| {
            try self.writeValue(writer, value);
        }
    }

    // Run-Length Encode line numbers (they repeat a lot)
    fn writeLineNumbers(self: *BytecodeSerializer, writer: *std.Io.Writer, lines: []const usize) !void {
        if (lines.len == 0) {
            try self.writeVarInt(writer, 0);
            return;
        }

        try self.writeVarInt(writer, lines.len);

        var i: usize = 0;
        while (i < lines.len) {
            const current_line = lines[i];
            var count: usize = 1;

            // Count consecutive identical lines
            while (i + count < lines.len and lines[i + count] == current_line) {
                count += 1;
            }

            // Write: line_number, repeat_count
            try self.writeVarInt(writer, current_line);
            try self.writeVarInt(writer, count);
            i += count;
        }
    }

    fn writeValue(self: *BytecodeSerializer, writer: *std.Io.Writer, value: Value) !void {
        if (value.isNumber()) {
            const num = value.asNumber();

            // Optimize for common small integers
            if (@trunc(num) == num and num >= -128 and num <= 127) {
                try self.writeU8(writer, 5); // Small int tag
                try self.writeU8(writer, @bitCast(@as(i8, @intFromFloat(num))));
            } else {
                try self.writeU8(writer, 0); // f64 tag
                const bytes = std.mem.asBytes(&num);
                try writer.writeAll(bytes);
            }
        } else if (value.isBool()) {
            try self.writeU8(writer, if (value.asBool()) 1 else 6); // true=1, false=6
        } else if (value.isNil()) {
            try self.writeU8(writer, 2);
        } else if (value.isObj()) {
            const obj = value.asObj();
            switch (obj.obj_t) {
                .string => {
                    try self.writeU8(writer, 3);
                    const str = obj.as(Obj.String);
                    try self.writeVarInt(writer, str.bytes.len);
                    try writer.writeAll(str.bytes);
                },
                .function => {
                    try self.writeU8(writer, 4);
                    try self.writeFunction(writer, obj.as(Obj.Function));
                },
                else => try self.writeU8(writer, 2), // nil for unsupported types
            }
        }
    }

    fn readFunctionFromBuffer(self: *BytecodeSerializer, buffer: []const u8, pos: *usize, vm: *VM) !*Obj.Function {
        const function = try Obj.Function.allocate(vm);

        vm.push(Value.fromObj(&function.obj));
        errdefer _ = vm.pop();

        // Read name
        const name_len = try self.readVarInt(buffer, pos);
        if (name_len > 0) {
            if (pos.* + name_len > buffer.len) return error.UnexpectedEof;
            const name_bytes = buffer[pos.*..][0..name_len];
            pos.* += name_len;
            function.name = try Obj.String.copy(vm, name_bytes);
        }

        // Read arity and upvalue count
        if (pos.* + 2 > buffer.len) return error.UnexpectedEof;
        function.arity = buffer[pos.*];
        function.upvalue_count = buffer[pos.* + 1];
        pos.* += 2;

        try self.readChunkFromBuffer(buffer, pos, vm, &function.chunk);

        _ = vm.pop();
        return function;
    }

    fn readChunkFromBuffer(self: *BytecodeSerializer, buffer: []const u8, pos: *usize, vm: *VM, chunk: *Chunk) !void {
        // Read bytecode
        const code_len = try self.readVarInt(buffer, pos);
        if (pos.* + code_len > buffer.len) return error.UnexpectedEof;

        try chunk.code.ensureTotalCapacity(chunk.allocator, code_len);
        chunk.code.appendSliceAssumeCapacity(buffer[pos.*..][0..code_len]);
        pos.* += code_len;

        // Read line numbers with RLE decompression
        try self.readLineNumbers(buffer, pos, chunk);

        // Read constants
        const constants_len = try self.readVarInt(buffer, pos);
        try chunk.constants.ensureTotalCapacity(chunk.allocator, constants_len);

        var i: usize = 0;
        while (i < constants_len) : (i += 1) {
            const value = try self.readValueFromBuffer(buffer, pos, vm);
            chunk.constants.appendAssumeCapacity(value);
        }
    }

    fn readLineNumbers(self: *BytecodeSerializer, buffer: []const u8, pos: *usize, chunk: *Chunk) !void {
        const total_lines = try self.readVarInt(buffer, pos);
        if (total_lines == 0) return;

        try chunk.lines.ensureTotalCapacity(chunk.allocator, total_lines);

        var decoded: usize = 0;
        while (decoded < total_lines) {
            const line_num = try self.readVarInt(buffer, pos);
            const count = try self.readVarInt(buffer, pos);

            var i: usize = 0;
            while (i < count) : (i += 1) {
                chunk.lines.appendAssumeCapacity(line_num);
            }
            decoded += count;
        }
    }

    fn readValueFromBuffer(self: *BytecodeSerializer, buffer: []const u8, pos: *usize, vm: *VM) anyerror!Value {
        if (pos.* >= buffer.len) return error.UnexpectedEof;
        const type_tag = buffer[pos.*];
        pos.* += 1;

        return switch (type_tag) {
            0 => blk: { // f64
                if (pos.* + 8 > buffer.len) return error.UnexpectedEof;
                const num_bits = std.mem.readInt(u64, buffer[pos.*..][0..8], .little);
                const num: f64 = @bitCast(num_bits);
                pos.* += 8;
                break :blk Value.fromNumber(num);
            },
            1 => Value.fromBool(true),
            2 => Value.fromNil(),
            3 => blk: { // string
                const str_len = try self.readVarInt(buffer, pos);
                if (pos.* + str_len > buffer.len) return error.UnexpectedEof;
                const str_bytes = buffer[pos.*..][0..str_len];
                pos.* += str_len;
                const str_obj = try Obj.String.copy(vm, str_bytes);
                break :blk Value.fromObj(&str_obj.obj);
            },
            4 => Value.fromObj(&(try self.readFunctionFromBuffer(buffer, pos, vm)).obj),
            5 => blk: { // small int
                if (pos.* >= buffer.len) return error.UnexpectedEof;
                const byte: i8 = @bitCast(buffer[pos.*]);
                pos.* += 1;
                break :blk Value.fromNumber(@floatFromInt(byte));
            },
            6 => Value.fromBool(false),
            else => error.InvalidValueType,
        };
    }

    // VarInt decoder - inline for performance
    inline fn readVarInt(_: *BytecodeSerializer, buffer: []const u8, pos: *usize) !usize {
        var result: usize = 0;
        var shift: u6 = 0;

        while (true) {
            if (pos.* >= buffer.len) return error.UnexpectedEof;
            const byte = buffer[pos.*];
            pos.* += 1;

            result |= @as(usize, byte & 0x7F) << shift;
            if ((byte & 0x80) == 0) break;
            shift += 7;

            if (shift >= 64) return error.VarIntOverflow;
        }

        return result;
    }
};

