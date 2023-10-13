const std = @import("std");
const VM = @import("./vm.zig").VM;
const Scanner = @import("./scanner.zig").Scanner;
const Token = @import("./scanner.zig").Token;
const Allocator = std.mem.Allocator;
const InterpretResult = @import("./vm.zig").InterpretResult;

pub fn compile(allocator: Allocator, source: []const u8) !InterpretResult {
    var scanner = Scanner.init(source);
    var token: Token = undefined;
    var line: usize = 0;
    while (!scanner.isAtEnd()) {
        token = scanner.scanToken();
        if (token.line != line) {
            std.debug.print("{d:4} ", .{token.line});
            line = token.line;
        } else {
            std.debug.print("   | ", .{});
        }
        const stoken = try token.toString(allocator);
        defer allocator.free(stoken);
        std.debug.print("{s}\n", .{stoken});
    }

    return .INTERPRET_OK;
}

const Parser = struct {
    vm: *VM,
    scanner: Scanner,

    pub fn init(vm: *VM, source: []const u8) !Parser {
        return Parser{
            .vm = vm,
            .scanner = Scanner.init(source),
        };
    }
};
