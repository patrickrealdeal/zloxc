const std = @import("std");
const Chunk = @import("./chunk.zig").Chunk;

const InterpretResult = enum {
    INTERPRET_OK,
    INTERPRET_COMPILE_ERROR,
    INTERPRET_RUNTIME_ERROR,
};

pub const VM = struct {
    chunk: *Chunk,
    ip: u8, // instruction pointer

    pub fn init() VM {}

    pub fn deinit(self: *VM) void {
        _ = self;
    }

    pub fn interpret(self: *VM, chunk: Chunk) !InterpretResult {
        self.chunk = chunk;
        self.ip = 0;
        return self.run();
    }

    fn run(self: *VM) !void {
        const instruction = self.readByte();
        switch (instruction) {
            .OP_RETURN => {
                return .INTERPRET_OK;
            },
        }
    }

    fn readByte(self: *VM) u8 {
        const byte = self.chunk.code.items[self.ip];
        return byte;
    }
};
