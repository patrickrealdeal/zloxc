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
const debug = @import("./debug.zig");
const Obj = @import("./object.zig").Obj;

pub fn compile(vm: *VM, source: []const u8, chunk: *Chunk) bool {
    var parser = try Parser.init(vm, chunk, source);
    parser.advance();
    parser.expression();
    parser.consume(.EOF, "Expect end of expression");

    if (parser.hadError) {
        return false;
    }

    parser.endCompiler();
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
const Precedence = enum(u8) {
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

    /// Asks the Scanner for next Token and stores it for later use
    fn advance(self: *Parser) void {
        self.previous = self.current;

        while (true) {
            self.current = self.scanner.scanToken();
            if (!self.check(.ERROR)) break;

            self.errorAtCurrent(self.current.lexeme);
        }
    }

    fn check(self: *Parser, ttype: TokenType) bool {
        return self.current.ttype == ttype;
    }

    fn expression(self: *Parser) void {
        self.parsePrecedence(.Assignment);
    }

    fn endCompiler(self: *Parser) void {
        self.emitReturn();
        if (!self.hadError and debug.trace_parser) {
            self.currentChunk().disassemble("code") catch unreachable;
        }
    }

    /// Validates that Token has the expected type
    fn consume(self: *Parser, ttype: TokenType, message: []const u8) void {
        if (self.current.ttype == ttype) {
            self.advance();
            return;
        }

        self.errorAtCurrent(message);
    }

    fn errorAtCurrent(self: *Parser, message: []const u8) void {
        self.errorAt(&self.current, message);
    }

    fn err(self: *Parser, message: []const u8) void {
        self.errorAt(&self.current, message);
    }

    fn errorAt(self: *Parser, token: *Token, message: []const u8) void {
        if (self.panicMode) return;
        self.hadError = true;

        std.debug.print("[line {}] Error", .{token.line});

        if (token.ttype == .EOF) {
            std.debug.print(" at end", .{});
        } else if (token.ttype == .ERROR) {
            // Nothing.
        } else {
            std.debug.print(" at '{s}'", .{token.lexeme});
        }

        std.debug.print(": {s}\n", .{message});
        self.hadError = true;
    }

    fn emitByte(self: *Parser, byte: u8) void {
        self.currentChunk().write(byte, self.previous.line) catch unreachable;
    }

    fn emitOp(self: *Parser, op: OpCode) void {
        self.currentChunk().writeOp(op, self.previous.line) catch unreachable;
    }

    fn emitReturn(self: *Parser) void {
        self.emitOp(.RETURN);
    }

    fn emitUnaryOp(self: *Parser, op: OpCode, byte: u8) void {
        self.emitOp(op);
        self.emitByte(byte);
    }

    fn emitConstant(self: *Parser, value: Value) void {
        self.emitUnaryOp(.CONSTANT, self.makeConstant(value));
    }

    fn match(self: *Parser, ttype: TokenType) bool {
        if (!self.check(ttype)) return false;
        self.advance();
        return true;
    }

    fn makeConstant(self: *Parser, value: Value) u8 {
        const constant = self.currentChunk().addConstant(value) catch unreachable;

        if (constant > std.math.maxInt(u8)) {
            self.err("Too many constants in one chunk.");
            return 0;
        }

        // const byte: u8 = @intCast(constant);
        return constant;
    }

    /// Starts at the current token and parses any expression at the given precedence level or higher
    fn parsePrecedence(self: *Parser, precedence: Precedence) void {
        self.advance();

        const prefixRule = getRule(self.previous.ttype).prefix;
        if (debug.trace_parser) {
            std.debug.print("P | {s}: {s}\n", .{
                @tagName(self.previous.ttype),
                @tagName(precedence),
            });
        }
        if (prefixRule == null) {
            self.err("Expect expression");
            return;
        }

        prefixRule.?(self);

        while (@intFromEnum(precedence) <= @intFromEnum(getRule(self.current.ttype).precedence)) {
            self.advance();
            const infixRule = getRule(self.previous.ttype).infix;
            if (debug.trace_parser) {
                std.debug.print("P | {s}: {s}\n", .{
                    @tagName(self.previous.ttype),
                    @tagName(precedence),
                });
            }

            if (infixRule == null) {
                self.err("Expected expression");
                return;
            }
            infixRule.?(self);
        }
    }

    // EXPRESSIONS
    /// converts token to f64 then generates code to load the value
    fn number(self: *Parser) void {
        const value = std.fmt.parseFloat(f64, self.previous.lexeme) catch unreachable;
        self.emitConstant(Value.fromNumber(value));
    }

    fn grouping(self: *Parser) void {
        self.expression();
        self.consume(TokenType.RIGHT_PAREN, "Expect ')' after expression.");
    }

    fn unary(self: *Parser) void {
        const opType = self.previous.ttype;

        // Compile the operand
        self.parsePrecedence(Precedence.Unary);

        // Emit Op instructions
        switch (opType) {
            .MINUS => self.emitOp(.NEGATE),
            .BANG => self.emitOp(.NOT),
            else => return,
        }
    }

    fn binary(self: *Parser) void {
        var opType = self.previous.ttype;

        const rule = getRule(opType);
        self.parsePrecedence(rule.precedence.next());

        switch (opType) {
            .PLUS => self.emitOp(.ADD),
            .MINUS => self.emitOp(.SUBTRACT),
            .STAR => self.emitOp(.MULTIPLY),
            .SLASH => self.emitOp(.DIVIDE),
            .BANG_EQUAL => {
                self.emitOp(.EQUAL);
                self.emitOp(.NOT);
            },
            .EQUAL_EQUAL => self.emitOp(.EQUAL),
            .GREATER => self.emitOp(.GREATER),
            .GREATER_EQUAL => {
                self.emitOp(.LESS);
                self.emitOp(.NOT);
            },
            .LESS => self.emitOp(.LESS),
            .LESS_EQUAL => {
                self.emitOp(.GREATER);
                self.emitOp(.NOT);
            },
            else => self.err("Unexpected binary operator"),
        }
    }

    fn literal(self: *Parser) void {
        switch (self.previous.ttype) {
            .NIL => self.emitOp(.NIL),
            .FALSE => self.emitOp(.FALSE),
            .TRUE => self.emitOp(.TRUE),
            else => unreachable,
        }
    }

    fn string(self: *Parser) void {
        const lexeme = self.previous.lexeme;
        const str = Obj.String.copy(self.vm, lexeme[0..]) catch unreachable;
        self.emitConstant(str.obj.value());
    }
};

const ParseRule = struct {
    prefix: ?*const ParseFn,
    infix: ?*const ParseFn,
    precedence: Precedence,
};

pub const ParseFn = fn (self: *Parser) void;

fn makeRule(comptime prefix: ?*const ParseFn, comptime infix: ?*const ParseFn, comptime precedence: Precedence) ParseRule {
    return ParseRule{
        .prefix = prefix,
        .infix = infix,
        .precedence = precedence,
    };
}

fn getRule(ttype: TokenType) ParseRule {
    return switch (ttype) {
        .LEFT_PAREN => makeRule(Parser.grouping, null, Precedence.None),
        .RIGHT_PAREN => makeRule(null, null, Precedence.None),
        .LEFT_BRACE => makeRule(null, null, Precedence.None),
        .RIGHT_BRACE => makeRule(null, null, Precedence.None),
        .COMMA => makeRule(null, null, Precedence.None),
        .DOT => makeRule(null, null, Precedence.None),
        .MINUS => makeRule(Parser.unary, Parser.binary, Precedence.Term),
        .PLUS => makeRule(null, Parser.binary, Precedence.Term),
        .SEMICOLON => makeRule(null, null, Precedence.None),
        .SLASH => makeRule(null, Parser.binary, Precedence.Factor),
        .STAR => makeRule(null, Parser.binary, Precedence.Factor),
        .BANG => makeRule(Parser.unary, null, Precedence.None),
        .BANG_EQUAL => makeRule(null, Parser.binary, Precedence.Equality),
        .EQUAL => makeRule(null, null, Precedence.None),
        .EQUAL_EQUAL => makeRule(null, Parser.binary, Precedence.Equality),
        .GREATER => makeRule(null, Parser.binary, Precedence.Comparison),
        .GREATER_EQUAL => makeRule(null, Parser.binary, Precedence.Comparison),
        .LESS => makeRule(null, Parser.binary, Precedence.Comparison),
        .LESS_EQUAL => makeRule(null, Parser.binary, Precedence.Comparison),
        .IDENTIFIER => makeRule(null, null, Precedence.None),
        .STRING => makeRule(Parser.string, null, Precedence.None),
        .NUMBER => makeRule(Parser.number, null, Precedence.None),
        .AND => makeRule(null, null, Precedence.None),
        .CLASS => makeRule(null, null, Precedence.None),
        .ELSE => makeRule(null, null, Precedence.None),
        .FALSE => makeRule(Parser.literal, null, Precedence.None),
        .FOR => makeRule(null, null, Precedence.None),
        .FUN => makeRule(null, null, Precedence.None),
        .IF => makeRule(null, null, Precedence.None),
        .NIL => makeRule(Parser.literal, null, Precedence.None),
        .OR => makeRule(null, null, Precedence.None),
        .PRINT => makeRule(null, null, Precedence.None),
        .RETURN => makeRule(null, null, Precedence.None),
        .SUPER => makeRule(null, null, Precedence.None),
        .THIS => makeRule(null, null, Precedence.None),
        .TRUE => makeRule(Parser.literal, null, Precedence.None),
        .VAR => makeRule(null, null, Precedence.None),
        .WHILE => makeRule(null, null, Precedence.None),
        .ERROR => makeRule(null, null, Precedence.None),
        .EOF => makeRule(null, null, Precedence.None),
    };
}
