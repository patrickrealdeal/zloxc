const std = @import("std");
const VM = @import("./vm.zig").VM;
const Scanner = @import("./scanner.zig").Scanner;
const Token = @import("./scanner.zig").Token;
const TokenType = @import("./scanner.zig").TokenType;
const Allocator = std.mem.Allocator;
const InterpretResult = @import("./vm.zig").InterpretResult;
const Chunk = @import("./chunk.zig").Chunk;
const OpCode = @import("./chunk.zig").OpCode;
const Value = @import("./value.zig").Value;

pub fn compile(vm: *VM, source: []const u8, chunk: *Chunk) !bool {
    var parser = try Parser.init(vm, chunk, source);

    var scanner = Scanner.init(source);
    parser.hadError = false;
    parser.panicMode = false;
    parser.scanner = scanner;

    try parser.advance();
    try parser.expression();
    try parser.consume(.EOF, "Expect end of expression");

    return !parser.hadError;
}

//const Compiler = struct {
//    chunk: Chunk,

//    pub fn init(chunk: *Chunk) Compiler {
//       return Compiler{
//            .chunk = chunk,
//        };
//    }
//};

// Ordered from lowest to higher
const Precedence = enum {
    None,
    Assignment, // =
    Or, // or
    And, // and
    Equality, // == !=
    Comparison, // < > <= >=
    Term, // + -
    Factor, // * /
    Unary, // ! -
    Call, // . ()
    Primary,

    fn next(self: Precedence) Precedence {
        return @enumFromInt(@intFromEnum(self) + 1);
    }
};

fn getPrecedence(ttype: TokenType) Precedence {
    return switch (ttype) {
        // Single char tokens
        .LEFT_PAREN => .Call,
        .RIGHT_PAREN, .LEFT_BRACE, .RIGHT_BRACE, .COMMA => .None,
        .DOT => .Call,
        .MINUS, .PLUS => .Term,
        .SEMICOLON => .None,
        .SLASH, .STAR => .Factor,

        // One or two char tokens
        .BANG_EQUAL, .EQUAL_EQUAL => .Equality,
        .GREATER, .GREATER_EQUAL, .LESS, .LESS_EQUAL => .Comparison,
        .BANG, .EQUAL => .None,

        // Literals
        .IDENTIFIER, .STRING, .NUMBER => .None,

        // Keywords
        .AND => .And,
        .OR => .Or,
        .CLASS, .ELSE, .FALSE, .FOR, .FUN, .IF, .NIL => .None,
        .PRINT, .RETURN, .SUPER, .THIS, .VAR, .WHILE, .ERROR => .None,
        .EOF => .None,
        else => .None,
    };
}

const CompilerErrors = error{OutOfMemory} || std.os.WriteError;

const Parser = struct {
    vm: *VM,
    scanner: Scanner,
    current: Token,
    previous: Token,
    hadError: bool,
    // compiler: *Compiler,
    panicMode: bool,
    chunk: *Chunk,

    pub fn init(vm: *VM, chunk: *Chunk, source: []const u8) !Parser {
        return Parser{
            .vm = vm,
            .scanner = Scanner.init(source),
            .current = undefined,
            .previous = undefined,
            .hadError = false,
            .panicMode = false,
            .chunk = chunk,
            //.compiler = compiler,
        };
    }

    fn currentChunk(self: *Parser) *Chunk {
        return self.chunk;
    }

    fn advance(self: *Parser) !void {
        self.previous = self.current;

        while (true) {
            self.current = self.scanner.scanToken();
            if (!self.check(.ERROR)) break;

            try self.errorAtCurrent(self.current.lexeme);
        }
    }

    fn check(self: *Parser, ttype: TokenType) bool {
        return self.current.ttype == ttype;
    }

    fn expression(self: *Parser) !void {
        try self.parsePrecedence(.Assignment);
    }

    fn endCompiler(self: *Parser) !void {
        try self.emitReturn();
        if (!self.hadError) {
            self.currentChunk().disassemble("code");
        }
    }

    fn consume(self: *Parser, ttype: TokenType, message: []const u8) !void {
        if (self.check(ttype)) {
            try self.advance();
        } else {
            try self.errorAtCurrent(message);
        }
    }

    fn errorAtCurrent(self: *Parser, message: []const u8) !void {
        try self.errorAt(&self.current, message);
    }

    fn err(self: *Parser, message: []const u8) !void {
        try self.errorAt(&self.current, message);
    }

    fn prefixError(self: *Parser) !void {
        try self.err("Expect prefix expression.");
    }

    fn infixError(self: *Parser) !void {
        try self.err("Expect infix expression.");
    }

    fn errorAt(self: *Parser, token: *Token, message: []const u8) !void {
        if (self.panicMode) return;
        self.panicMode = true;

        try self.vm.errWriter.print("[line {}] Error", .{token.line});

        switch (token.ttype) {
            .EOF => {
                try self.vm.errWriter.print(" at end", .{});
            },
            .ERROR => {
                try self.vm.errWriter.print(" at '{s}'", .{token.lexeme});
            },
            else => {
                try self.vm.errWriter.print(" at '{s}'", .{token.lexeme});
            },
        }
        try self.vm.errWriter.print(": {s}\n", .{message});
        self.hadError = true;
    }

    fn emitByte(self: *Parser, byte: u8) !void {
        try self.currentChunk().write(byte, self.previous.line);
    }

    fn emitOp(self: *Parser, op: OpCode) !void {
        try self.currentChunk().writeOp(op, self.previous.line);
    }

    fn emitReturn(self: *Parser) !void {
        try self.emitOp(.RETURN);
    }

    fn emitUnaryOp(self: *Parser, op: OpCode, byte: u8) !void {
        try self.emitOp(op);
        try self.emitByte(byte);
    }

    fn emitConstant(self: *Parser, value: Value) !void {
        try self.emitUnaryOp(.OP_CONSTANT, try self.makeConstant(value));
    }

    fn match(self: *Parser, ttype: TokenType) !bool {
        if (!self.check(ttype)) return false;
        try self.advance();
        return true;
    }

    fn makeConstant(self: *Parser, value: Value) !u8 {
        self.vm.push(value);
        const constant = try self.currentChunk().addConstant(value);

        if (constant > std.math.maxInt(u8)) {
            try self.err("Too many constants in one chunk.");
            return 0;
        }

        const byte: u8 = @intCast(constant);
        return byte;
    }

    fn parsePrecedence(self: *Parser, precedence: Precedence) CompilerErrors!void {
        try self.advance();

        const canAssign = @intFromEnum(precedence) <= @intFromEnum(Precedence.Assignment);
        try self.prefix(self.previous.ttype, canAssign);

        while (@intFromEnum(precedence) <= @intFromEnum(self.current.ttype)) {
            try self.advance();
            try self.infix(self.previous.ttype, canAssign);
        }

        if (canAssign and try self.match(.EQUAL)) {
            try self.err("Invalid assignment target");
        }
    }

    // EXPRESSIONS
    /// converts token to f64 then generates code to load the value
    fn number(self: *Parser) !void {
        if (std.fmt.parseFloat(f64, self.previous.lexeme)) |value| {
            try self.emitConstant(Value.fromNumber(value));
        } else |e| switch (e) {
            error.InvalidCharacter => {
                try self.err("Could not parse Number");
                return;
            },
        }
    }

    fn grouping(self: *Parser) !void {
        try self.expression();
        try self.consume(.RIGHT_PAREN, "Expect ')' after expression.");
    }

    fn unary(self: *Parser) !void {
        const opType = self.previous.ttype;

        // Compile the operand
        try self.expression();

        // Emit Op instructions
        switch (opType) {
            .MINUS => try self.emitOp(.OP_NEGATE),
            else => return,
        }
    }

    fn binary(self: *Parser) !void {
        var opType = self.previous.ttype;

        try self.parsePrecedence(getPrecedence(opType).next());

        switch (opType) {
            .PLUS => try self.emitOp(.OP_ADD),
            .MINUS => try self.emitOp(.OP_SUBTRACT),
            .STAR => try self.emitOp(.OP_MULTIPLY),
            .SLASH => try self.emitOp(.OP_DIVIDE),
            else => try self.err("Unexpected binary operator"),
        }
    }

    fn prefix(self: *Parser, ttype: TokenType, canAssign: bool) !void {
        _ = canAssign;
        switch (ttype) {
            // Single-character tokens.
            .LEFT_PAREN => try self.grouping(),
            .MINUS => try self.unary(),
            .RIGHT_PAREN, .LEFT_BRACE, .RIGHT_BRACE, .COMMA, .DOT => try self.prefixError(),
            .PLUS, .SEMICOLON, .SLASH, .STAR => try self.prefixError(),

            // One or two character tokens.
            .BANG => try self.unary(),
            .EQUAL, .BANG_EQUAL, .EQUAL_EQUAL, .GREATER, .GREATER_EQUAL => try self.prefixError(),
            .LESS, .LESS_EQUAL => try self.prefixError(),

            // Literals.
            //.Identifier => try self.variable(canAssign),
            //.String => try self.string(),
            .NUMBER => try self.number(),

            // Keywords.
            //.Nil, .True, .False => try self.literal(),
            //.This => try self.this(),
            //.Super => try self.super(),
            .AND, .CLASS, .ELSE, .FOR, .FUN, .IF, .OR => try self.prefixError(),
            .PRINT, .RETURN, .VAR, .WHILE, .ERROR, .EOF => try self.prefixError(),
            else => return,
        }
    }

    fn infix(self: *Parser, ttype: TokenType, canAssign: bool) !void {
        _ = canAssign;
        switch (ttype) {
            // Single-character tokens.
            .MINUS, .PLUS, .SLASH, .STAR => try self.binary(),
            //.LeftParen => try self.call(),
            //.Dot => try self.dot(canAssign),
            .RIGHT_PAREN, .LEFT_BRACE, .RIGHT_BRACE, .COMMA, .SEMICOLON => try self.infixError(),

            // One or two character tokens.
            .BANG_EQUAL, .EQUAL_EQUAL, .GREATER, .GREATER_EQUAL => try self.binary(),
            .LESS, .LESS_EQUAL => try self.binary(),

            .BANG, .EQUAL => try self.infixError(),

            // Literals.
            .IDENTIFIER, .STRING, .NUMBER => try self.infixError(),

            // Keywords.
            //.And => try self.and_(),
            //.Or => try self.or_(),
            .CLASS, .ELSE, .FALSE, .FOR, .FUN, .IF, .NIL => try self.infixError(),
            .PRINT, .RETURN, .SUPER, .THIS, .TRUE, .VAR, .WHILE, .ERROR, .EOF => try self.infixError(),
            else => return,
        }
    }
};
