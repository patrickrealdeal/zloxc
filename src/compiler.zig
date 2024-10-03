const std = @import("std");
const Scanner = @import("scanner.zig").Scanner;
const Chunk = @import("chunk.zig").Chunk;
const Token = @import("scanner.zig").Token;
const OpCode = @import("chunk.zig").OpCode;
const Value = @import("value.zig").Value;
const TokenType = @import("scanner.zig").TokenType;
const Obj = @import("object.zig");
const VM = @import("vm.zig").VM;

const debug_print_code = true;
const u8_count = std.math.maxInt(u8) + 1;
const CompilerError = error{ Compiler_Error, TooManyLocalVariables, VarAlreadyDeclared };

const Compiler = struct {
    locals: [u8_count]Local,
    local_count: u8,
    scope_depth: u32,

    pub fn init() Compiler {
        return .{
            .locals = undefined,
            .local_count = 0,
            .scope_depth = 0,
        };
    }

    pub fn addLocal(self: *Compiler, name: Token) !void {
        if (current.local_count == u8_count) {
            return CompilerError.TooManyLocalVariables;
        }

        self.locals[self.local_count] = Local{ .name = name, .depth = null };
        self.local_count += 1;
    }
};

const Local = struct {
    name: Token,
    depth: ?u32,
};

var current: *Compiler = undefined;

pub fn compile(source: []const u8, chunk: *Chunk, vm: *VM) !void {
    var scanner = Scanner.init(source);
    var parser = Parser.init(&scanner, chunk, vm);
    try parser.advance();
    var compiler = Compiler.init();
    current = &compiler;

    while (!try parser.match(.eof)) {
        try parser.declaration();
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

const ParseFn = fn (parser: *Parser, can_assign: bool) anyerror!void;

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

inline fn getRule(ttype: TokenType) ParseRule {
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
        .identifier => ParseRule.init(Parser.variable, null, .prec_none),
        .string => ParseRule.init(Parser.string, null, .prec_none),
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
    vm: *VM,

    pub fn init(scanner: *Scanner, chunk: *Chunk, vm: *VM) Parser {
        return .{
            .current = undefined,
            .previous = undefined,
            .scanner = scanner,
            .compiling_chunk = chunk,
            .panic_mode = false,
            .vm = vm,
        };
    }

    pub fn advance(self: *Parser) !void {
        self.previous = self.current;
        while (true) {
            self.current = self.scanner.scanToken();
            if (self.current.ttype != .err) break;

            self.errorAtCurrent(self.current.lexeme);
            return CompilerError.Compiler_Error;
        }
    }

    fn consume(self: *Parser, ttype: TokenType, comptime message: []const u8) !void {
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

        const can_assign = @intFromEnum(precedence) <= @intFromEnum(Precedence.prec_assignment);
        try prefixRule(self, can_assign);

        while (@intFromEnum(precedence) <= @intFromEnum(getRule(self.current.ttype).precedence)) {
            try self.advance();
            const rule = getRule(self.previous.ttype);
            std.debug.print("{any}\n", .{rule});
            const infixRule = rule.infix orelse unreachable;
            try infixRule(self, can_assign);
        }

        if (can_assign and try self.match(.equal)) {
            self.err("Invalid assignment target");
        }
    }

    fn parseVariable(self: *Parser, comptime message: []const u8) !u8 {
        try self.consume(.identifier, message);

        try self.declareVariable();
        if (current.scope_depth > 0) return 0;

        return try self.identifierConstant(self.previous);
    }

    fn defineVariable(self: *Parser, global: u8) !void {
        if (current.scope_depth > 0) {
            self.markInitialized();
            return;
        }
        self.emitBytes(@intFromEnum(OpCode.define_global), global);
    }

    fn markInitialized(self: *Parser) void {
        _ = self;
        current.locals[current.local_count - 1].depth = current.scope_depth;
    }

    fn identifierConstant(self: *Parser, name: Token) !u8 {
        return try self.makeConstant(Value{ .obj = &(try Obj.String.copy(self.vm, name.lexeme)).obj });
    }

    fn declareVariable(self: *Parser) !void {
        if (current.scope_depth == 0) return;
        const name = self.previous;
        var i: usize = current.local_count;
        while (i > 0) {
            i -= 1;
            const local = current.locals[i];
            if (local.depth != null and local.depth.? < current.scope_depth) {
                break;
            }

            if (std.mem.eql(u8, local.name.lexeme, name.lexeme)) {
                return CompilerError.VarAlreadyDeclared;
            }
        }

        try current.addLocal(name);
    }

    fn expression(self: *Parser) !void {
        try self.parsePrecedence(.prec_assignment);
    }

    fn block(self: *Parser) !void {
        while (!self.check(.right_brace) and !self.check(.eof)) {
            try self.declaration();
        }

        try self.consume(.right_brace, "Expect '}' after block.");
    }

    fn varDeclaration(self: *Parser) !void {
        const global = try self.parseVariable("Expected variable name.");

        if (try self.match(.equal)) {
            try self.expression();
        } else {
            self.emitByte(@intFromEnum(OpCode.nil));
        }

        try self.consume(.semicolon, "Expected ';' after variable declaration,");
        try self.defineVariable(global);
    }

    fn declaration(self: *Parser) anyerror!void {
        if (try self.match(.keyword_var)) {
            try self.varDeclaration();
        } else {
            try self.statement();
        }

        if (self.panic_mode) try self.synchronize();
    }

    fn statement(self: *Parser) !void {
        if (try self.match(.keyword_print)) {
            try self.printStatement();
        } else if (try self.match(.left_brace)) {
            self.beginScope();
            try self.block();
            self.endScope();
        } else {
            try self.expressionStatement();
        }
    }

    fn expressionStatement(self: *Parser) !void {
        try self.expression();
        try self.consume(.semicolon, "Expected ';' after expression.");
        self.emitByte(@intFromEnum(OpCode.pop));
    }

    fn printStatement(self: *Parser) !void {
        try self.expression();
        try self.consume(.semicolon, "Expected ';' after value.");
        self.emitByte(@intFromEnum(OpCode.print));
    }

    fn synchronize(self: *Parser) !void {
        self.panic_mode = false;

        while (!try self.match(.eof)) {
            if (self.previous.ttype == .semicolon) return;
            switch (self.current.ttype) {
                .keyword_class, .keyword_fun, .keyword_var, .keyword_for, .keyword_if, .keyword_while, .keyword_print, .keyword_return => return,
                else => try self.advance(),
            }
        }
    }

    fn number(self: *Parser, can_assign: bool) !void {
        _ = can_assign;
        const value = std.fmt.parseFloat(f64, self.previous.lexeme) catch unreachable;
        try self.emitConstant(Value{ .number = value });
    }

    fn match(self: *Parser, ttype: TokenType) !bool {
        if (!(self.current.ttype == ttype)) return false;
        try self.advance();
        return true;
    }

    fn check(self: *Parser, ttype: TokenType) bool {
        return self.current.ttype == ttype;
    }

    fn grouping(self: *Parser, can_assign: bool) !void {
        _ = can_assign;
        try self.expression();
        try self.consume(.right_paren, "expected ')' after expression");
    }

    fn string(self: *Parser, can_assign: bool) !void {
        _ = can_assign;
        const bytes_len = self.previous.lexeme.len;
        // discards '""'
        const str = try Obj.String.copy(self.vm, self.previous.lexeme[1 .. bytes_len - 1]);
        try self.emitConstant(Value{ .obj = &str.obj });
    }

    fn literal(self: *Parser, can_assign: bool) !void {
        _ = can_assign;
        switch (self.previous.ttype) {
            .keyword_false => self.emitByte(@intFromEnum(OpCode.false)),
            .keyword_true => self.emitByte(@intFromEnum(OpCode.true)),
            .keyword_nil => self.emitByte(@intFromEnum(OpCode.nil)),
            else => unreachable,
        }
    }

    fn variable(self: *Parser, can_assign: bool) !void {
        try self.namedVariable(self.previous, can_assign);
    }

    fn namedVariable(self: *Parser, name: Token, can_assign: bool) !void {
        var get_op: OpCode = undefined;
        var set_op: OpCode = undefined;

        var arg: ?u8 = self.resolveLocal(current, name);
        if (arg) |_| {
            get_op = .get_local;
            set_op = .set_local;
        } else {
            arg = try self.identifierConstant(name);
            get_op = .get_global;
            set_op = .set_global;
        }

        if (can_assign and try self.match(.equal)) {
            try self.expression();
            self.emitBytes(@as(u8, @intCast(@intFromEnum(set_op))), arg.?);
        } else {
            self.emitBytes(@as(u8, @intCast(@intFromEnum(get_op))), arg.?);
        }
    }

    fn resolveLocal(self: *Parser, compiler: *Compiler, name: Token) ?u8 {
        var i: usize = compiler.local_count;
        while (i > 0) {
            i -= 1;
            const local = compiler.locals[i];
            if (std.mem.eql(u8, name.lexeme, local.name.lexeme)) {
                if (local.depth == null) {
                    self.err("Can't read local variable in its own initializer.");
                }
                return @as(u8, @intCast(i));
            }
        }

        return null;
    }

    fn unary(self: *Parser, can_assign: bool) !void {
        _ = can_assign;
        const optype = self.previous.ttype;

        // compile opperand
        try self.parsePrecedence(.prec_unary);

        switch (optype) {
            .minus => self.emitByte(@intFromEnum(OpCode.negate)),
            .bang => self.emitByte(@intFromEnum(OpCode.not)),
            else => unreachable,
        }
    }

    fn binary(self: *Parser, can_assign: bool) !void {
        _ = can_assign;
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
            std.debug.print("-----------------\n", .{});
        }
    }

    fn beginScope(self: *Parser) void {
        _ = self;
        current.scope_depth += 1;
    }

    fn endScope(self: *Parser) void {
        current.scope_depth -= 1;
        while (current.local_count > 0 and
            current.locals[current.local_count - 1].depth.? > current.scope_depth)
        {
            self.emitByte(@intFromEnum(OpCode.pop));
            current.local_count -= 1;
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
