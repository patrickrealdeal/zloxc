const std = @import("std");
const Scanner = @import("scanner.zig");
const Chunk = @import("chunk.zig");
const Token = @import("token.zig");
const Value = @import("value.zig").Value;
const Obj = @import("object.zig");
const VM = @import("vm.zig");
const debug = @import("debug.zig");
const TokenType = Token.TokenType;
const OpCode = Chunk.OpCode;

const u8_max = std.math.maxInt(u8) + 1;
const CompilerError = error{ CompilerError, TooManyLocalVariables, VarAlreadyDeclared };

pub const Compiler = struct {
    enclosing: ?*Compiler,
    func: *Obj.Function,
    func_t: FunctionType,
    locals: [u8_max]Local,
    upvalues: std.ArrayList(Upvalue),
    local_count: u8,
    scope_depth: u32,
    allocator: *std.mem.Allocator,

    pub fn init(vm: *VM, func_t: FunctionType, enclosing: ?*Compiler) !Compiler {
        return .{
            .enclosing = enclosing,
            .func = try Obj.Function.allocate(vm),
            .func_t = func_t,
            .locals = [_]Local{Local{ .name = "" }} ** u8_max,
            .upvalues = std.ArrayList(Upvalue).init(vm.allocator),
            .local_count = 1, // claim slot zero for VM internal use
            .scope_depth = 0,
            .allocator = &vm.allocator,
        };
    }

    pub fn deinit(self: *Compiler) void {
        self.upvalues.deinit();
    }

    pub fn addLocal(self: *Compiler, name: Token) !void {
        if (self.local_count == u8_max) {
            return CompilerError.TooManyLocalVariables;
        }

        self.locals[self.local_count] = Local{ .name = name.lexeme, .depth = null };
        self.local_count += 1;
    }

    fn addUpvalue(self: *Compiler, index: u8, is_local: bool) !u8 {
        for (self.upvalues.items, 0..) |upv, i| {
            if (upv.index == index and upv.is_local == is_local) {
                return @as(u8, @intCast(i));
            }
        }

        if (self.upvalues.items.len == u8_max) {
            return CompilerError.TooManyLocalVariables;
        }

        try self.upvalues.append(Upvalue{ .is_local = is_local, .index = index });
        self.func.upvalue_count += 1;
        return @as(u8, @intCast(self.upvalues.items.len - 1));
    }
};

const Local = struct {
    name: []const u8,
    depth: ?u32 = null,
    is_captured: bool = false,
};

const Upvalue = struct {
    is_local: bool,
    index: u8,
};

const FunctionType = enum {
    function,
    script,
};

pub fn compile(source: []const u8, vm: *VM) !?*Obj.Function {
    var scanner: Scanner = .init(source);
    var compiler: Compiler = try .init(vm, .script, null);
    defer compiler.deinit();
    var parser: Parser = .init(&scanner, vm, &compiler);
    vm.parser = &parser;
    defer vm.parser = null;
    try parser.advance();

    while (!try parser.match(.eof)) {
        try parser.declaration();
    }

    const func = parser.endCompiler();
    return func;
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
        .left_paren => .init(Parser.grouping, Parser.call, .prec_call),
        .right_paren => .init(null, null, .prec_none),
        .left_brace => .init(null, null, .prec_none),
        .right_brace => .init(null, null, .prec_none),
        .comma => .init(null, null, .prec_none),
        .dot => .init(null, null, .prec_none),
        .minus => .init(Parser.unary, Parser.binary, .prec_term),
        .plus => .init(null, Parser.binary, .prec_term),
        .semicolon => .init(null, null, .prec_none),
        .slash => .init(null, Parser.binary, .prec_factor),
        .star => .init(null, Parser.binary, .prec_factor),
        .bang => .init(Parser.unary, null, .prec_none),
        .bang_equal => .init(null, Parser.binary, .prec_equality),
        .equal => .init(null, null, .prec_none),
        .equal_equal => .init(null, Parser.binary, .prec_equality),
        .greater => .init(null, Parser.binary, .prec_comparison),
        .greater_equal => .init(null, Parser.binary, .prec_comparison),
        .less => .init(null, Parser.binary, .prec_comparison),
        .less_equal => .init(null, Parser.binary, .prec_comparison),
        .identifier => .init(Parser.variable, null, .prec_none),
        .string => .init(Parser.string, null, .prec_none),
        .number => .init(Parser.number, null, .prec_none),
        .class => .init(null, null, .prec_none),
        .@"else" => .init(null, null, .prec_none),
        .false => .init(Parser.literal, null, .prec_none),
        .true => .init(Parser.literal, null, .prec_none),
        .@"for" => .init(null, null, .prec_none),
        .fun => .init(null, null, .prec_none),
        .@"if" => .init(null, null, .prec_none),
        .nil => .init(Parser.literal, null, .prec_none),
        .@"and" => .init(null, Parser.and_, .prec_and),
        .@"or" => .init(null, Parser.or_, .prec_or),
        .print => .init(null, null, .prec_none),
        .@"return" => .init(null, null, .prec_none),
        .super => .init(null, null, .prec_none),
        .this => .init(null, null, .prec_none),
        .@"var" => .init(null, null, .prec_none),
        .@"while" => .init(null, null, .prec_none),
        .err => .init(null, null, .prec_none),
        .eof => .init(null, null, .prec_none),
    };
}

pub const Parser = struct {
    current: Token,
    previous: Token,
    scanner: *Scanner,
    panic_mode: bool,
    vm: *VM,
    compiler: *Compiler,

    pub fn init(scanner: *Scanner, vm: *VM, compiler: *Compiler) Parser {
        return .{
            .current = undefined,
            .previous = undefined,
            .scanner = scanner,
            .panic_mode = false,
            .vm = vm,
            .compiler = compiler,
        };
    }

    pub fn advance(self: *Parser) !void {
        self.previous = self.current;
        while (true) {
            self.current = self.scanner.scanToken();
            if (self.current.ttype != .err) break;

            self.errorAtCurrent(self.current.lexeme);
            return CompilerError.CompilerError;
        }
    }

    fn consume(self: *Parser, ttype: TokenType, comptime message: []const u8) !void {
        if (self.current.ttype == ttype) {
            try self.advance();
            return;
        }

        self.errorAtCurrent(message);
        return CompilerError.CompilerError;
    }

    fn parsePrecedence(self: *Parser, precedence: Precedence) !void {
        try self.advance();
        const prefixRule = getRule(self.previous.ttype).prefix orelse {
            self.err("Expected expression");
            return CompilerError.CompilerError;
        };

        const can_assign = @intFromEnum(precedence) <= @intFromEnum(Precedence.prec_assignment);
        try prefixRule(self, can_assign);

        while (@intFromEnum(precedence) <= @intFromEnum(getRule(self.current.ttype).precedence)) {
            try self.advance();
            const rule = getRule(self.previous.ttype);
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
        if (self.compiler.scope_depth > 0) return 0;

        return try self.identifierConstant(self.previous);
    }

    fn defineVariable(self: *Parser, global: u8) !void {
        if (self.compiler.scope_depth > 0) {
            self.markInitialized();
            return;
        }
        self.emitBytes(@intFromEnum(OpCode.define_global), global);
    }

    fn argumentList(self: *Parser) !u8 {
        var arg_count: u8 = 0;
        if (!self.check(.right_paren)) {
            while (true) {
                try self.expression();

                if (arg_count == 255) {
                    self.err("Can't have more than 255 arguments.");
                    break;
                }

                arg_count += 1;
                if (!try self.match(.comma)) break;
            }
        }

        try self.consume(.right_paren, "Expect ')' after arguments.");
        return arg_count;
    }

    fn and_(self: *Parser, can_assign: bool) !void {
        _ = can_assign;
        const end_jump = self.emitJump(@intFromEnum(OpCode.jump_if_false));

        self.emitByte(@intFromEnum(OpCode.pop));
        try self.parsePrecedence(.prec_and);

        self.patchJump(end_jump);
    }

    fn or_(self: *Parser, can_assign: bool) !void {
        _ = can_assign;
        const else_jump = self.emitJump(@intFromEnum(OpCode.jump_if_false));
        const end_jump = self.emitJump(@intFromEnum(OpCode.jump));

        self.patchJump(else_jump);
        self.emitByte(@intFromEnum(OpCode.pop));

        try self.parsePrecedence(.prec_or);
        self.patchJump(end_jump);
    }

    fn markInitialized(self: *Parser) void {
        if (self.compiler.scope_depth == 0) return;
        self.compiler.locals[self.compiler.local_count - 1].depth = self.compiler.scope_depth;
    }

    fn identifierConstant(self: *Parser, name: Token) !u8 {
        const str = try Obj.String.copy(self.vm, name.lexeme);
        return try self.makeConstant(Value{ .obj = &str.obj });
    }

    fn declareVariable(self: *Parser) !void {
        if (self.compiler.scope_depth == 0) return;
        const name = self.previous;
        var i: usize = self.compiler.local_count;
        while (i > 0) {
            i -= 1;
            const local = self.compiler.locals[i];
            if (local.depth != null and local.depth.? < self.compiler.scope_depth) {
                break;
            }

            if (std.mem.eql(u8, local.name, name.lexeme)) {
                return CompilerError.VarAlreadyDeclared;
            }
        }

        try self.compiler.addLocal(name);
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

    fn function(self: *Parser, fun_t: FunctionType) !void {
        var compiler = try Compiler.init(self.vm, fun_t, self.compiler);
        defer compiler.deinit();
        self.compiler = &compiler;
        self.compiler.func.name = try Obj.String.copy(self.vm, self.previous.lexeme);
        self.beginScope();

        try self.consume(.left_paren, "Expect '(' after function name.");

        // Function parameters
        if (!self.check(.right_paren)) {
            while (true) {
                if (self.compiler.func.arity == 255) {
                    self.errorAtCurrent("Can't have more than 255 parameters.");
                    break;
                }

                self.compiler.func.arity += 1;
                const constant = try self.parseVariable("Expect parameter name.");
                try self.defineVariable(constant);
                if (!try self.match(.comma)) break;
            }
        }

        try self.consume(.right_paren, "Expect ')' after function name.");

        // Body
        try self.consume(.left_brace, "Expect '{' after function name.");
        try self.block();

        const func = self.endCompiler();
        self.emitBytes(@intFromEnum(OpCode.closure), try self.makeConstant(Value{ .obj = &func.obj }));

        for (compiler.upvalues.items) |upvalue| {
            self.emitByte(if (upvalue.is_local) 1 else 0);
            self.emitByte(upvalue.index);
        }
    }

    fn funDeclaration(self: *Parser) !void {
        const global = try self.parseVariable("Expect function name.");
        self.markInitialized();
        try self.function(.function);
        try self.defineVariable(global);
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
        if (try self.match(.@"var")) {
            try self.varDeclaration();
        } else if (try self.match(.fun)) {
            try self.funDeclaration();
        } else {
            try self.statement();
        }

        if (self.panic_mode) try self.synchronize();
    }

    fn statement(self: *Parser) !void {
        if (try self.match(.print)) {
            try self.printStatement();
        } else if (try self.match(.@"if")) {
            try self.ifStatement();
        } else if (try self.match(.@"return")) {
            try self.retStatement();
        } else if (try self.match(.@"while")) {
            try self.whileStatement();
        } else if (try self.match(.@"for")) {
            try self.forStatement();
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

    fn ifStatement(self: *Parser) anyerror!void {
        try self.consume(.left_paren, "Expected '(' after 'if'.");
        try self.expression();
        try self.consume(.right_paren, "Expected ')' after condition.");

        const then_jump = self.emitJump(@intFromEnum(OpCode.jump_if_false));
        self.emitByte(@intFromEnum(OpCode.pop));
        try self.statement();

        const elseJump = self.emitJump(@intFromEnum(OpCode.jump));
        self.patchJump(then_jump);
        self.emitByte(@intFromEnum(OpCode.pop));

        if (try self.match(.@"else")) {
            try self.statement();
        }

        self.patchJump(elseJump);
    }

    fn whileStatement(self: *Parser) anyerror!void {
        const loop_start = self.currentChunk().code.items.len;
        try self.consume(.left_paren, "Expected '(' after 'while'.");
        try self.expression();
        try self.consume(.right_paren, "Expected ')' after condition.");

        const exit_jump = self.emitJump(@intFromEnum(OpCode.jump_if_false));
        self.emitByte(@intFromEnum(OpCode.pop));
        try self.statement();
        self.emitLoop(loop_start);

        self.patchJump(exit_jump);
        self.emitByte(@intFromEnum(OpCode.pop));
    }

    fn forStatement(self: *Parser) anyerror!void {
        self.beginScope();

        try self.consume(.left_paren, "Expected '(' after 'for'.");
        if (try self.match(.semicolon)) {
            // no initializer
        } else if (try self.match(.@"var")) {
            try self.varDeclaration();
        } else {
            try self.expressionStatement();
        }

        var loop_start = self.currentChunk().code.items.len;

        // condition clause
        var exit_jump: ?usize = null;
        if (!try self.match(.semicolon)) {
            try self.expression();
            try self.consume(.semicolon, "Expected ';' after loop condition.");

            // jump out of loop if condition is false
            exit_jump = self.emitJump(@intFromEnum(OpCode.jump_if_false));
            self.emitByte(@intFromEnum(OpCode.pop)); // Condition
        }

        // Increment Clause
        if (!try self.match(.right_paren)) {
            const body_jump = self.emitJump(@intFromEnum(OpCode.jump));
            const increment_start = self.currentChunk().code.items.len;
            try self.expression();
            self.emitByte(@intFromEnum(OpCode.pop));
            try self.consume(.right_paren, "Expect ')' after for loop clauses.");

            self.emitLoop(loop_start);
            loop_start = increment_start;
            self.patchJump(body_jump);
        }

        try self.statement();
        self.emitLoop(loop_start);

        if (exit_jump) |jump| {
            self.patchJump(jump);
            self.emitByte(@intFromEnum(OpCode.pop)); // Condition
        }

        self.endScope();
    }

    fn printStatement(self: *Parser) !void {
        try self.expression();
        try self.consume(.semicolon, "Expected ';' after value.");
        self.emitByte(@intFromEnum(OpCode.print));
    }

    fn retStatement(self: *Parser) !void {
        if (self.compiler.func_t == .script) {
            self.err("Can't return from top-level code.");
        }

        if (try self.match(.semicolon)) {
            self.emitReturn();
        } else {
            try self.expression();
            try self.consume(.semicolon, "Expect ';' after value.");
            self.emitByte(@intFromEnum(OpCode.ret));
        }
    }

    fn synchronize(self: *Parser) !void {
        self.panic_mode = false;

        while (!try self.match(.eof)) {
            if (self.previous.ttype == .semicolon) return;
            switch (self.current.ttype) {
                .class,
                .fun,
                .@"var",
                .@"for",
                .@"if",
                .@"while",
                .print,
                .@"return",
                => return,
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
            .false => self.emitByte(@intFromEnum(OpCode.false)),
            .true => self.emitByte(@intFromEnum(OpCode.true)),
            .nil => self.emitByte(@intFromEnum(OpCode.nil)),
            else => unreachable,
        }
    }

    fn variable(self: *Parser, can_assign: bool) !void {
        try self.namedVariable(self.previous, can_assign);
    }

    fn namedVariable(self: *Parser, name: Token, can_assign: bool) !void {
        var get_op: ?OpCode = null;
        var set_op: ?OpCode = null;
        var arg: u8 = undefined;

        if (self.resolveLocal(self.compiler, name)) |local| {
            arg = local;
            get_op = .get_local;
            set_op = .set_local;
        } else if (try self.resolveUpvalue(self.compiler, name)) |upvalue| {
            arg = upvalue;
            get_op = .get_upvalue;
            set_op = .set_upvalue;
        } else {
            arg = try self.identifierConstant(name);
            get_op = .get_global;
            set_op = .set_global;
        }

        if (can_assign and try self.match(.equal)) {
            try self.expression();
            self.emitBytes(@as(u8, @intCast(@intFromEnum(set_op.?))), arg);
        } else {
            self.emitBytes(@as(u8, @intCast(@intFromEnum(get_op.?))), arg);
        }
    }

    fn resolveLocal(self: *Parser, compiler: *Compiler, name: Token) ?u8 {
        var i: usize = compiler.local_count;
        while (i > 0) {
            i -= 1;
            const local = compiler.locals[i];
            if (std.mem.eql(u8, name.lexeme, local.name)) {
                if (local.depth == null) {
                    self.err("Can't read local variable in its own initializer.");
                }
                return @as(u8, @intCast(i));
            }
        }

        return null;
    }

    fn resolveUpvalue(self: *Parser, compiler: *Compiler, name: Token) !?u8 {
        if (compiler.enclosing) |enclosing| {
            if (self.resolveLocal(enclosing, name)) |local| {
                enclosing.locals[local].is_captured = true;
                return try compiler.addUpvalue(local, true);
            } else if (try self.resolveUpvalue(enclosing, name)) |upvalue| {
                return try compiler.addUpvalue(upvalue, false);
            }
        }

        return null;
    }

    fn unary(self: *Parser, can_assign: bool) !void {
        _ = can_assign;
        const optype = self.previous.ttype;

        // compile operand
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

    fn call(self: *Parser, can_assign: bool) !void {
        _ = can_assign;
        const arg_count = try self.argumentList();
        self.emitBytes(@intFromEnum(OpCode.call), arg_count);
    }

    fn emitConstant(self: *Parser, value: Value) !void {
        self.emitBytes(@intFromEnum(OpCode.constant), try self.makeConstant(value));
    }

    fn makeConstant(self: *Parser, value: Value) !u8 {
        self.vm.push(value);
        const constant = self.currentChunk().addConstant(value) catch {
            self.err("Err adding constant");
            return CompilerError.CompilerError;
        };
        _ = self.vm.pop();

        if (constant > std.math.maxInt(u8)) {
            self.err("Too many constants in a chunk");
            return CompilerError.CompilerError;
        }

        return @as(u8, @intCast(constant));
    }

    fn patchJump(self: *Parser, offset: usize) void {
        // -2 to adjust for the bytecode for the jump offset itself.
        const jump = self.currentChunk().code.items.len - offset - 2;

        if (jump > std.math.maxInt(u16)) {
            self.err("Too much code to jump over.");
        }

        self.currentChunk().code.items[offset] = @as(u8, @truncate((jump >> 8))) & 0xff;
        self.currentChunk().code.items[offset + 1] = @as(u8, @truncate(jump & 0xff));
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

    fn emitLoop(self: *Parser, loop_start: usize) void {
        self.emitByte(@intFromEnum(OpCode.loop));

        const offset = self.currentChunk().code.items.len - loop_start + 2;
        if (offset > std.math.maxInt(u16)) self.err("Loop body too large.");

        self.emitByte(@as(u8, @truncate((offset >> 8) & 0xff)));
        self.emitByte(@truncate(offset & 0xff));
    }

    fn emitJump(self: *Parser, byte: u8) usize {
        self.emitByte(byte);
        // 16-bit placeholder operand to calculate the jump
        self.emitBytes(0xff, 0xff);
        return self.currentChunk().code.items.len - 2;
    }

    fn currentChunk(self: *Parser) *Chunk {
        return &self.compiler.func.chunk;
    }

    fn endCompiler(self: *Parser) *Obj.Function {
        self.emitReturn();
        const func = self.compiler.func;

        if (self.compiler.enclosing) |compiler| {
            self.compiler = compiler;
        }

        if (comptime debug.print_code) {
            Chunk.disassemble(&func.chunk, if (func.name) |name| name.bytes else "<script>");
            std.debug.print("-----------------\n", .{});
        }

        return func;
    }

    fn beginScope(self: *Parser) void {
        self.compiler.scope_depth += 1;
    }

    fn endScope(self: *Parser) void {
        self.compiler.scope_depth -= 1;
        while (self.compiler.local_count > 1 and
            self.compiler.locals[self.compiler.local_count - 1].depth.? > self.compiler.scope_depth)
        {
            if (self.compiler.locals[self.compiler.local_count - 1].is_captured) {
                self.emitByte(@intFromEnum(OpCode.close_upvalue));
            } else {
                self.emitByte(@intFromEnum(OpCode.pop));
            }
            self.compiler.local_count -= 1;
        }
    }

    fn emitReturn(self: *Parser) void {
        self.emitByte(@intFromEnum(OpCode.nil));
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
