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
    var compiler = try Compiler.init(vm);
    defer compiler.deinit();

    var parser = try Parser.init(vm, &compiler, chunk, source);
    parser.advance();

    if (parser.hadError) {
        return false;
    }

    while (!parser.match(.EOF)) {
        parser.declaration();
    }

    parser.endCompiler();
    return !parser.hadError;
}

pub const Compiler = struct {
    locals: std.ArrayList(Local),
    scopeDepth: usize,

    pub fn init(vm: *VM) !Compiler {
        return Compiler{
            .locals = std.ArrayList(Local).init(vm.allocator),
            .scopeDepth = 0,
        };
    }

    pub fn deinit(self: *Compiler) void {
        self.locals.deinit();
    }
};

const Local = struct {
    name: Token,
    depth: i32,
};

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
    compiler: *Compiler,
    panicMode: bool,
    chunk: *Chunk,

    pub fn init(vm: *VM, compiler: *Compiler, chunk: *Chunk, source: []const u8) !Parser {
        return Parser{
            .vm = vm,
            .scanner = Scanner.init(source),
            .current = undefined,
            .previous = undefined,
            .hadError = false,
            .panicMode = false,
            .chunk = chunk,
            .compiler = compiler,
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

    // Expressions and Statements
    fn expression(self: *Parser) void {
        self.parsePrecedence(.Assignment);
    }

    fn declaration(self: *Parser) void {
        if (self.match(.VAR)) {
            self.varDeclaration();
        } else {
            self.statement();
        }

        if (self.panicMode) {
            self.synchronize();
        }
    }

    fn statement(self: *Parser) void {
        if (self.match(.PRINT)) {
            self.printStatement();
        } else if (self.match(.LEFT_BRACE)) {
            self.beginScope();
            self.block();
            self.endScope();
        } else if (self.match(.IF)) {
            self.ifStatement();
        } else if (self.match(.WHILE)) {
            self.whileStatement();
        } else if (self.match(.FOR)) {
            self.forStatement();
        } else {
            self.expressionStatement();
        }
    }

    // Semantically, an expression statement evaluates the expression and discards the result.
    fn expressionStatement(self: *Parser) void {
        self.expression();
        self.consume(.SEMICOLON, "Expect ';' after expression.");
        self.emitOp(.POP);
    }

    fn ifStatement(self: *Parser) void {
        self.consume(.LEFT_PAREN, "Expect '(' after 'if'.");
        self.expression();
        self.consume(.RIGHT_PAREN, "Expect ')' after condition.");

        const thenJump = self.emitJump(.JUMP_IF_FALSE);
        self.emitOp(.POP); // pop the condition value
        self.statement();

        const elseJump = self.emitJump(.JUMP);

        self.patchJump(thenJump);
        self.emitOp(.POP);

        if (self.match(.ELSE)) {
            self.statement();
        }

        self.patchJump(elseJump);
    }

    fn whileStatement(self: *Parser) void {
        const loopStart = self.currentChunk().code.items.len;
        self.consume(.LEFT_PAREN, "Expected '(' after 'while'.");
        self.expression();
        self.consume(.RIGHT_PAREN, "Expected ')' after condition");

        const exitJump = self.emitJump(.JUMP_IF_FALSE);
        self.emitOp(.POP); // Pop condition Value
        self.statement();
        // After executing the body of a while loop,
        // we jump all the way back to before the condition.
        self.emitLoop(loopStart);

        self.patchJump(exitJump);
        self.emitOp(.POP);
    }

    fn forStatement(self: *Parser) void {
        self.beginScope(); // varaibles should be scoped to loop body

        // Initializer clause
        self.consume(.LEFT_PAREN, "Expect '(' after 'for'.");
        if (self.match(.SEMICOLON)) {
            // No initializer
        } else if (self.match(.VAR)) {
            self.varDeclaration();
        } else {
            self.expression();
        }

        var loopStart = self.currentChunk().code.items.len;

        // Condition clause
        var exitJump: ?usize = null;
        if (!self.match(.SEMICOLON)) {
            self.expression();
            self.consume(.SEMICOLON, "Expect ';' after loop condition.");

            // Jump out the loop if condition is false
            exitJump = self.emitJump(.JUMP_IF_FALSE);
            self.emitOp(.POP);
        }

        // Increment clause
        if (!self.match(.RIGHT_PAREN)) {
            const bodyJump = self.emitJump(.JUMP);
            const incrementStart = self.currentChunk().code.items.len;
            self.expression();
            self.emitOp(.POP);
            self.consume(.RIGHT_PAREN, "Expected ')' after for clause.");

            self.emitLoop(loopStart);
            loopStart = incrementStart;
            self.patchJump(bodyJump);
        }

        self.statement();
        self.emitLoop(loopStart);

        if (exitJump) |exit| {
            self.patchJump(exit);
            self.emitOp(.POP);
        }
        self.endScope();
    }

    fn printStatement(self: *Parser) void {
        self.expression();
        self.consume(.SEMICOLON, "Expect ';' after value.");
        self.emitOp(.PRINT);
    }

    // Initializes 'var a;' as 'var a = nil'.
    fn varDeclaration(self: *Parser) void {
        const global = self.parseVariable("Expect variable name.") catch unreachable;

        if (self.match(.EQUAL)) {
            self.expression();
        } else {
            self.emitOp(.NIL);
        }

        self.consume(.SEMICOLON, "Expect ';' after variable declaration.");

        self.defineVariable(global);
    }

    fn parseVariable(self: *Parser, message: []const u8) !u8 {
        self.consume(.IDENTIFIER, message);

        try self.declareVariable();
        if (self.compiler.scopeDepth > 0) return 0;

        return self.identifierConstant(&self.previous);
    }

    fn defineVariable(self: *Parser, global: u8) void {
        if (self.compiler.scopeDepth > 0) {
            self.markInitialized();
            return;
        }

        self.emitUnaryOp(.DEFINE_GLOBAL, global);
    }

    // “Declaring” is when the variable is added to the scope, and “defining” is when it becomes available for use.
    fn markInitialized(self: *Parser) void {
        var locals = &self.compiler.locals;
        locals.items[locals.items.len - 1].depth = @intCast(self.compiler.scopeDepth);
    }

    fn declareVariable(self: *Parser) !void {
        if (self.compiler.scopeDepth == 0) return;

        const name = self.previous;
        // detect vars with same name in same local scope
        var i: i32 = @as(i32, @intCast(self.compiler.locals.items.len)) - 1;
        while (i >= 0) : (i -= 1) {
            const local_index = @as(u8, @intCast(i));
            const local = self.compiler.locals.items[local_index];
            if (local.depth != 1 and local.depth < self.compiler.scopeDepth) break;

            if (std.mem.eql(u8, name.lexeme, local.name.lexeme)) {
                self.err("Already a variable with this name in this scope.");
            }
        }

        try self.addLocal(name);
    }

    fn addLocal(self: *Parser, name: Token) !void {
        if (self.compiler.locals.items.len > std.math.maxInt(u8)) {
            self.err("Too many local variables in function.");
            return;
        }

        const local = Local{
            .name = name,
            .depth = -1,
        };

        try self.compiler.locals.append(local);
    }

    // Takes the given token and adds its lexeme to the chunk’s constant table as a string.
    fn identifierConstant(self: *Parser, name: *Token) !u8 {
        const string_obj = try Obj.String.copy(self.vm, name.lexeme);
        return self.makeConstant(string_obj.obj.value());
    }

    fn block(self: *Parser) void {
        while (!self.check(.RIGHT_BRACE) and !self.check(.EOF)) {
            self.declaration();
        }

        self.consume(.RIGHT_BRACE, "Expect '}' after block.");
    }

    fn beginScope(self: *Parser) void {
        self.compiler.scopeDepth += 1;
    }

    fn endScope(self: *Parser) void {
        self.compiler.scopeDepth -= 1;
        var locals = &self.compiler.locals;
        while (locals.items.len > 0 and locals.items[locals.items.len - 1].depth > self.compiler.scopeDepth) {
            self.emitOp(.POP);

            _ = locals.pop();
        }
    }

    // We skip tokens indiscriminately until we reach something
    // that looks like a statement boundary
    fn synchronize(self: *Parser) void {
        self.panicMode = false;

        while (self.current.ttype != .EOF) {
            if (self.previous.ttype == .SEMICOLON) return;
            switch (self.current.ttype) {
                .CLASS, .FUN, .VAR, .FOR, .IF, .WHILE, .PRINT, .RETURN => return,
                else => self.advance(),
            }
        }
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
        self.panicMode = true;

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

    fn emitJump(self: *Parser, op: OpCode) usize {
        self.emitOp(op);
        // Backpatching, We emit the jump instruction
        // first with a placeholder offset operand
        self.emitByte(0xff); // We use two bytes for the jump offset
        self.emitByte(0xff); // 16-bit offset lets us jump over up to 65,535 bytes of code
        return self.currentChunk().code.items.len - 2;
    }

    fn emitLoop(self: *Parser, loopStart: usize) void {
        self.emitOp(.LOOP);

        const offset: u16 = @intCast(self.currentChunk().code.items.len - loopStart + 2);
        if (offset > std.math.maxInt(u16)) self.err("Loop body too large.");

        self.emitByte(@as(u8, @truncate((offset >> 8) & 0xff)));
        self.emitByte(@as(u8, @truncate(offset & 0xff)));
    }

    fn patchJump(self: *Parser, offset: usize) void {
        // -2 to adjust for the bytecode for the jump offset itself.
        const jump = self.currentChunk().code.items.len - offset - 2;

        if (jump > std.math.maxInt(u16)) {
            self.err("Too much code to jump over.");
        }

        // replaces the operand at the given location
        // with the calculated jump offset
        self.currentChunk().code.items[offset] = @as(u8, @intCast(jump >> 8 & 0xff));
        self.currentChunk().code.items[offset + 1] = @as(u8, @intCast(jump & 0xff));
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

        const canAssign = @intFromEnum(precedence) <= @intFromEnum(Precedence.Assignment);
        prefixRule.?(self, canAssign);

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
            infixRule.?(self, canAssign);
        }

        if (canAssign and self.match(.EQUAL)) {
            self.err("Invalid assignment target.");
        }
    }

    // EXPRESSIONS
    /// converts token to f64 then generates code to load the value
    fn number(self: *Parser, _: bool) void {
        const value = std.fmt.parseFloat(f64, self.previous.lexeme) catch unreachable;
        self.emitConstant(Value.fromNumber(value));
    }

    fn grouping(self: *Parser, _: bool) void {
        self.expression();
        self.consume(TokenType.RIGHT_PAREN, "Expect ')' after expression.");
    }

    fn unary(self: *Parser, _: bool) void {
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

    fn binary(self: *Parser, _: bool) void {
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

    fn literal(self: *Parser, _: bool) void {
        switch (self.previous.ttype) {
            .NIL => self.emitOp(.NIL),
            .FALSE => self.emitOp(.FALSE),
            .TRUE => self.emitOp(.TRUE),
            else => unreachable,
        }
    }

    fn string(self: *Parser, _: bool) void {
        const lexeme = self.previous.lexeme;
        const lexLen = lexeme.len;
        const str = Obj.String.copy(self.vm, lexeme[1 .. lexLen - 1]) catch unreachable; // Trim ""
        self.emitConstant(str.obj.value());
    }

    fn variable(self: *Parser, canAssign: bool) void {
        self.namedVariable(&self.previous, canAssign);
    }

    fn and_(self: *Parser, canAssign: bool) void {
        _ = canAssign;
        const endJump = self.emitJump(.JUMP_IF_FALSE);
        self.emitOp(.POP);
        self.parsePrecedence(.And);
        self.patchJump(endJump);
    }

    fn or_(self: *Parser, canAssign: bool) void {
        _ = canAssign;
        const elseJump = self.emitJump(.JUMP_IF_FALSE);
        const endJump = self.emitJump(.JUMP);

        self.patchJump(elseJump);
        self.emitOp(.POP);

        self.parsePrecedence(.Or);
        self.patchJump(endJump);
    }

    fn namedVariable(self: *Parser, name: *Token, canAssign: bool) void {
        var arg: u8 = undefined;
        var getOp: OpCode = undefined;
        var setOp: OpCode = undefined;

        if (self.resolveLocal(name)) |v| {
            getOp = .GET_LOCAL;
            setOp = .SET_LOCAL;
            arg = v;
        } else {
            arg = self.identifierConstant(name) catch unreachable;
            getOp = .GET_GLOBAL;
            setOp = .SET_GLOBAL;
        }

        if (canAssign and self.match(.EQUAL)) {
            self.expression();
            self.emitUnaryOp(setOp, arg);
        } else {
            self.emitUnaryOp(getOp, arg);
        }
    }

    fn resolveLocal(self: *Parser, name: *Token) ?u8 {
        var locals = self.compiler.locals;

        var i: i32 = @as(i32, @intCast(locals.items.len)) - 1;
        while (i >= 0) : (i -= 1) {
            const local_index = @as(u8, @intCast(i));
            const local = locals.items[local_index];
            if (std.mem.eql(u8, name.lexeme, local.name.lexeme)) {
                // Check if local is fully defined
                if (local.depth == -1) {
                    self.err("Can't read local variable in its own initializer.");
                }

                return @as(u8, @intCast(i));
            }
        }

        return null;
    }
};

const ParseRule = struct {
    prefix: ?*const ParseFn,
    infix: ?*const ParseFn,
    precedence: Precedence,
};

pub const ParseFn = fn (self: *Parser, canAssign: bool) void;

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
        .IDENTIFIER => makeRule(Parser.variable, null, Precedence.None),
        .STRING => makeRule(Parser.string, null, Precedence.None),
        .NUMBER => makeRule(Parser.number, null, Precedence.None),
        .AND => makeRule(null, Parser.and_, Precedence.None),
        .CLASS => makeRule(null, null, Precedence.None),
        .ELSE => makeRule(null, null, Precedence.None),
        .FALSE => makeRule(Parser.literal, null, Precedence.None),
        .FOR => makeRule(null, null, Precedence.None),
        .FUN => makeRule(null, null, Precedence.None),
        .IF => makeRule(null, null, Precedence.None),
        .NIL => makeRule(Parser.literal, null, Precedence.None),
        .OR => makeRule(null, Parser.or_, Precedence.None),
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
