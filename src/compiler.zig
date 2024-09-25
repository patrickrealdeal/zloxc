const std = @import("std");
const Scanner = @import("scanner.zig").Scanner;
const Chunk = @import("chunk.zig").Chunk;
const Token = @import("scanner.zig").Token;
const OpCode = @import("chunk.zig").OpCode;
const Value = @import("value.zig").Value;
const TokenType = @import("scanner.zig").TokenType;

const debug_print_code = true;
const CompilerError = error{Compiler_Error};

pub fn compile(source: []const u8, chunk: *Chunk) !void {
    var scanner = Scanner.init(source);
    var parser = Parser.init(&scanner, chunk);
    try parser.advance();
    try parser.expression();

    if (scanner.scanToken()) |_| {
        parser.errorAtCurrent("Expected end of expression");
        return CompilerError.Compiler_Error;
    }

    parser.endCompiler();
}

const Precedence = enum {
    prec_none,
    prec_assignment, // =
    prec_or, // or
    prec_and, // and
    prec_equality, // == !=
    prec_comparison, // < > <= >=
    prec_term, // + -
    prec_factor, // * /
    prec_unary, // ! -
    prec_call, // . ()
    prec_primary,
};

const ParseFn = fn (parser: *Parser) anyerror!void;

const ParseRule = struct {
    prefix: ?*const ParseFn,
    infix: ?*const ParseFn,
    precedence: Precedence,

    pub fn init(prefix: ?*const ParseFn, infix: ?*const ParseFn, precedence: Precedence) ParseRule {
        return .{
            .prefix = prefix,
            .infix = infix,
            .precedence = precedence,
        };
    }
};

fn getRule(ttype: TokenType) ParseRule {
    return switch (ttype) {
        .left_paren => ParseRule.init(Parser.grouping, null, .prec_none),
        .right_paren => ParseRule.init(null, null, .prec_none),
        .left_brace => ParseRule.init(null, null, .prec_none),
        .right_brace => ParseRule.init(null, null, .prec_none),
        .comma => ParseRule.init(null, null, .prec_none),
        .dot => ParseRule.init(null, null, .prec_none),
        .minus => ParseRule.init(Parser.unary, Parser.binary, .prec_term),
        .plus => ParseRule.init(null, Parser.binary, .prec_term),
        .semicolon => ParseRule.init(null, null, .prec_none),
        .slash => ParseRule.init(null, Parser.binary, .prec_factor),
        .star => ParseRule.init(null, Parser.binary, .prec_factor),
        .bang => ParseRule.init(Parser.unary, null, .prec_none),
        .bang_equal => ParseRule.init(null, Parser.binary, .prec_equality),
        .equal => ParseRule.init(null, null, .prec_none),
        .equal_equal => ParseRule.init(null, Parser.binary, .prec_equality),
        .greater => ParseRule.init(null, Parser.binary, .prec_comparison),
        .greater_equal => ParseRule.init(null, Parser.binary, .prec_comparison),
        .less => ParseRule.init(null, Parser.binary, .prec_comparison),
        .less_equal => ParseRule.init(null, Parser.binary, .prec_comparison),
        .identifier => ParseRule.init(null, null, .prec_none),
        .string => ParseRule.init(null, null, .prec_none),
        .number => ParseRule.init(Parser.number, null, .prec_none),
        .keyword_and => ParseRule.init(null, null, .prec_none),
        .keyword_class => ParseRule.init(null, null, .prec_none),
        .keyword_else => ParseRule.init(null, null, .prec_none),
        .keyword_false => ParseRule.init(Parser.literal, null, .prec_none),
        .keyword_for => ParseRule.init(null, null, .prec_none),
        .keyword_fun => ParseRule.init(null, null, .prec_none),
        .keyword_if => ParseRule.init(null, null, .prec_none),
        .keyword_nil => ParseRule.init(Parser.literal, null, .prec_none),
        .keyword_or => ParseRule.init(null, null, .prec_none),
        .keyword_print => ParseRule.init(null, null, .prec_none),
        .keyword_return => ParseRule.init(null, null, .prec_none),
        .keyword_super => ParseRule.init(null, null, .prec_none),
        .keyword_this => ParseRule.init(null, null, .prec_none),
        .keyword_true => ParseRule.init(Parser.literal, null, .prec_none),
        .keyword_var => ParseRule.init(null, null, .prec_none),
        .keyword_while => ParseRule.init(null, null, .prec_none),
        .err => ParseRule.init(null, null, .prec_none),
        .eof => ParseRule.init(null, null, .prec_none),
    };
}

const Parser = struct {
    current: Token,
    previous: Token,
    scanner: *Scanner,
    compiling_chunk: *Chunk,
    panic_mode: bool,

    pub fn init(scanner: *Scanner, chunk: *Chunk) Parser {
        return .{
            .current = undefined,
            .previous = undefined,
            .scanner = scanner,
            .compiling_chunk = chunk,
            .panic_mode = false,
        };
    }

    pub fn advance(self: *Parser) !void {
        self.previous = self.current;
        while (self.scanner.scanToken()) |tok| {
            self.current = tok;
            if (self.current.ttype != .err) break;

            self.errorAtCurrent(self.current.lexeme);
            return CompilerError.Compiler_Error;
        }
    }

    fn consume(self: *Parser, ttype: TokenType, message: []const u8) !void {
        if (self.current.ttype == ttype) {
            try self.advance();
            return;
        }

        self.errorAtCurrent(message);
        return CompilerError.Compiler_Error;
    }

    fn parsePrecedence(self: *Parser, precedence: Precedence) !void {
        try self.advance();
        const prefixRule = getRule(self.previous.ttype).prefix orelse {
            self.err("Expected expression");
            return CompilerError.Compiler_Error;
        };

        try prefixRule(self);

        while (@intFromEnum(precedence) <= @intFromEnum(getRule(self.current.ttype).precedence)) {
            try self.advance();
            const rule = getRule(self.previous.ttype);
            std.debug.print("{any}\n", .{rule});
            const infixRule = rule.infix orelse {
                self.err("Unreachable");
                return CompilerError.Compiler_Error;
            };

            try infixRule(self);
        }
    }

    fn expression(self: *Parser) !void {
        try self.parsePrecedence(.prec_assignment);
    }

    fn number(self: *Parser) !void {
        const value = std.fmt.parseFloat(f64, self.previous.lexeme) catch unreachable;
        try self.emitConstant(Value{ .number = value });
    }

    fn grouping(self: *Parser) !void {
        try self.expression();
        try self.consume(.right_paren, "expected ')' after expression");
    }

    fn literal(self: *Parser) !void {
        switch (self.previous.ttype) {
            .keyword_false => self.emitByte(@intFromEnum(OpCode.false)),
            .keyword_true => self.emitByte(@intFromEnum(OpCode.true)),
            .keyword_nil => self.emitByte(@intFromEnum(OpCode.nil)),
            else => unreachable,
        }
    }

    fn unary(self: *Parser) !void {
        const optype = self.previous.ttype;

        // compile opperand
        try self.parsePrecedence(.prec_unary);

        switch (optype) {
            .minus => self.emitByte(@intFromEnum(OpCode.negate)),
            .bang => self.emitByte(@intFromEnum(OpCode.not)),
            else => unreachable,
        }
    }

    fn binary(self: *Parser) !void {
        const optype = self.previous.ttype;
        const rule = getRule(optype);
        try self.parsePrecedence(@enumFromInt(@intFromEnum(rule.precedence) + 1));

        switch (optype) {
            .plus => self.emitByte(@intFromEnum(OpCode.add)),
            .minus => self.emitByte(@intFromEnum(OpCode.sub)),
            .star => self.emitByte(@intFromEnum(OpCode.mul)),
            .slash => self.emitByte(@intFromEnum(OpCode.div)),
            .bang_equal => self.emitBytes(@intFromEnum(OpCode.equal), @intFromEnum(OpCode.not)),
            .equal_equal => self.emitByte(@intFromEnum(OpCode.equal)),
            .greater => self.emitByte(@intFromEnum(OpCode.greater)),
            .greater_equal => self.emitBytes(@intFromEnum(OpCode.less), @intFromEnum(OpCode.not)),
            .less => self.emitByte(@intFromEnum(OpCode.less)),
            .less_equal => self.emitBytes(@intFromEnum(OpCode.greater), @intFromEnum(OpCode.not)),
            else => unreachable,
        }
    }

    fn emitConstant(self: *Parser, value: Value) !void {
        self.emitBytes(@intFromEnum(OpCode.constant), try self.makeConstant(value));
    }

    fn makeConstant(self: *Parser, value: Value) !u8 {
        const constant = self.currentChunk().addConstant(value) catch {
            self.err("Err adding constant");
            return CompilerError.Compiler_Error;
        };

        if (constant > std.math.maxInt(u8)) {
            self.err("Too many constants in a chunk");
            return CompilerError.Compiler_Error;
        }

        return @as(u8, @intCast(constant));
    }

    fn emitByte(self: *Parser, byte: u8) void {
        self.currentChunk().write(byte, self.previous.line) catch |e| {
            std.log.err("Error {any} trying to emit byte\n", .{e});
            std.process.exit(1);
        };
    }

    fn emitBytes(self: *Parser, byte1: u8, byte2: u8) void {
        self.emitByte(byte1);
        self.emitByte(byte2);
    }

    fn currentChunk(self: *Parser) *Chunk {
        return self.compiling_chunk;
    }

    fn endCompiler(self: *Parser) void {
        self.emitReturn();

        if (comptime debug_print_code) {
            Chunk.disassemble(self.currentChunk(), "code");
        }
    }

    fn emitReturn(self: *Parser) void {
        self.emitByte(@intFromEnum(OpCode.ret));
    }

    fn errorAtCurrent(self: *Parser, message: []const u8) void {
        self.errorAt(self.current, message);
    }

    fn err(self: *Parser, message: []const u8) void {
        self.errorAt(self.previous, message);
    }

    fn errorAt(self: *Parser, token: Token, message: []const u8) void {
        if (self.panic_mode) return;

        self.panic_mode = false;
        const errWriter = std.io.getStdErr().writer();
        errWriter.print("[line {d}] Error", .{token.line}) catch unreachable;

        switch (token.ttype) {
            .eof => errWriter.writeAll(" at end") catch unreachable,
            .err => {},
            else => errWriter.print(" at '{s}'", .{token.lexeme}) catch unreachable,
        }
        errWriter.print(": {s}\n", .{message}) catch unreachable;
    }
};
