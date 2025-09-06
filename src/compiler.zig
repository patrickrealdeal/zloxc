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

const u8_max = std.math.maxInt(u8);
const CompilerError = error{ CompilerError, TooManyLocalVariables, VarAlreadyDeclared };

pub const Compiler = struct {
    enclosing: ?*Compiler,
    func: *Obj.Function,
    func_t: FunctionType,
    locals: [u8_max]Local,
    upvalues: std.ArrayList(Upvalue),
    local_count: u8,
    scope_depth: u32,
    allocator: std.mem.Allocator,

    pub fn init(vm: *VM, func_t: FunctionType, enclosing: ?*Compiler) !Compiler {
        return .{
            .enclosing = enclosing,
            .func = try Obj.Function.allocate(vm),
            .func_t = func_t,
            .locals = [_]Local{.empty} ** u8_max,
            .upvalues = .empty,
            .local_count = 1, // claim slot zero for VM internal use
            .scope_depth = 0,
            .allocator = vm.allocator,
        };
    }

    pub fn deinit(parser: *Compiler) void {
        parser.upvalues.deinit(parser.allocator);
    }

    pub fn addLocal(compiler: *Compiler, name: Token) !void {
        if (compiler.local_count == u8_max) {
            return CompilerError.TooManyLocalVariables;
        }

        compiler.locals[compiler.local_count] = Local{ .name = name.lexeme, .depth = null };
        compiler.local_count += 1;
    }

    fn addUpvalue(parser: *Compiler, index: u8, upv_source: UpvalueSource) !u8 {
        for (parser.upvalues.items, 0..) |upv, i| {
            if (upv.index == index and upv.source == .local) {
                return @as(u8, @intCast(i));
            }
        }

        if (parser.upvalues.items.len == u8_max) {
            return CompilerError.TooManyLocalVariables;
        }

        try parser.upvalues.append(parser.allocator, Upvalue{ .source = upv_source, .index = index });
        parser.func.upvalue_count += 1;
        return @as(u8, @intCast(parser.upvalues.items.len - 1));
    }
};

const Local = struct {
    name: []const u8,
    depth: ?u32 = null,
    is_captured: bool = false,

    pub const empty: Local = .{ .name = "" };
};

const UpvalueSource = enum { local, enclosing };
const Upvalue = struct {
    source: UpvalueSource,
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

    pub fn advance(parser: *Parser) !void {
        parser.previous = parser.current;
        while (true) {
            parser.current = parser.scanner.scanToken();
            if (parser.current.ttype != .err) break;

            parser.errorAtCurrent(parser.current.lexeme);
            return CompilerError.CompilerError;
        }
    }

    fn consume(parser: *Parser, ttype: TokenType, comptime message: []const u8) !void {
        if (parser.current.ttype == ttype) {
            try parser.advance();
            return;
        }

        parser.errorAtCurrent(message);
        return CompilerError.CompilerError;
    }

    fn parsePrecedence(parser: *Parser, precedence: Precedence) !void {
        try parser.advance();
        const prefixRule = getRule(parser.previous.ttype).prefix orelse {
            parser.err("Expected expression");
            return CompilerError.CompilerError;
        };

        const can_assign = @intFromEnum(precedence) <= @intFromEnum(Precedence.prec_assignment);
        try prefixRule(parser, can_assign);

        while (@intFromEnum(precedence) <= @intFromEnum(getRule(parser.current.ttype).precedence)) {
            try parser.advance();
            const rule = getRule(parser.previous.ttype);
            const infixRule = rule.infix orelse unreachable;
            try infixRule(parser, can_assign);
        }

        if (can_assign and try parser.match(.equal)) {
            parser.err("Invalid assignment target");
        }
    }

    fn parseVariable(parser: *Parser, comptime message: []const u8) !u8 {
        try parser.consume(.identifier, message);

        try parser.declareVariable();
        if (parser.compiler.scope_depth > 0) return 0;

        return try parser.identifierConstant(parser.previous);
    }

    fn defineVariable(parser: *Parser, global: u8) !void {
        if (parser.compiler.scope_depth > 0) {
            parser.markInitialized();
            return;
        }
        parser.emitBytes(@intFromEnum(OpCode.define_global), global);
    }

    fn argumentList(parser: *Parser) !u8 {
        var arg_count: u8 = 0;
        if (!parser.check(.right_paren)) {
            while (true) {
                try parser.expression();

                if (arg_count == 255) {
                    parser.err("Can't have more than 255 arguments.");
                    break;
                }

                arg_count += 1;
                if (!try parser.match(.comma)) break;
            }
        }

        try parser.consume(.right_paren, "Expect ')' after arguments.");
        return arg_count;
    }

    fn and_(parser: *Parser, can_assign: bool) !void {
        _ = can_assign;
        const end_jump = parser.emitJump(@intFromEnum(OpCode.jump_if_false));

        parser.emitByte(@intFromEnum(OpCode.pop));
        try parser.parsePrecedence(.prec_and);

        parser.patchJump(end_jump);
    }

    fn or_(parser: *Parser, can_assign: bool) !void {
        _ = can_assign;
        const else_jump = parser.emitJump(@intFromEnum(OpCode.jump_if_false));
        const end_jump = parser.emitJump(@intFromEnum(OpCode.jump));

        parser.patchJump(else_jump);
        parser.emitByte(@intFromEnum(OpCode.pop));

        try parser.parsePrecedence(.prec_or);
        parser.patchJump(end_jump);
    }

    fn markInitialized(parser: *Parser) void {
        if (parser.compiler.scope_depth == 0) return;
        parser.compiler.locals[parser.compiler.local_count - 1].depth = parser.compiler.scope_depth;
    }

    fn identifierConstant(parser: *Parser, name: Token) !u8 {
        const str = try Obj.String.copy(parser.vm, name.lexeme);
        return try parser.makeConstant(Value{ .obj = &str.obj });
    }

    fn declareVariable(parser: *Parser) !void {
        if (parser.compiler.scope_depth == 0) return;
        const name = parser.previous;
        var i: usize = parser.compiler.local_count;
        while (i > 0) {
            i -= 1;
            const local = parser.compiler.locals[i];
            if (local.depth != null and local.depth.? < parser.compiler.scope_depth) {
                break;
            }

            if (std.mem.eql(u8, local.name, name.lexeme)) {
                return CompilerError.VarAlreadyDeclared;
            }
        }

        try parser.compiler.addLocal(name);
    }

    fn expression(parser: *Parser) !void {
        try parser.parsePrecedence(.prec_assignment);
    }

    fn block(parser: *Parser) !void {
        while (!parser.check(.right_brace) and !parser.check(.eof)) {
            try parser.declaration();
        }

        try parser.consume(.right_brace, "Expect '}' after block.");
    }

    fn function(parser: *Parser, fun_t: FunctionType) !void {
        var compiler: Compiler = try .init(parser.vm, fun_t, parser.compiler);
        defer compiler.deinit();
        parser.compiler = &compiler;
        parser.compiler.func.name = try Obj.String.copy(parser.vm, parser.previous.lexeme);
        parser.beginScope();

        try parser.consume(.left_paren, "Expect '(' after function name.");

        // Function parameters
        if (!parser.check(.right_paren)) {
            while (true) {
                if (parser.compiler.func.arity == 255) {
                    parser.errorAtCurrent("Can't have more than 255 parameters.");
                    break;
                }

                parser.compiler.func.arity += 1;
                const constant = try parser.parseVariable("Expect parameter name.");
                try parser.defineVariable(constant);
                if (!try parser.match(.comma)) break;
            }
        }

        try parser.consume(.right_paren, "Expect ')' after function name.");

        // Body
        try parser.consume(.left_brace, "Expect '{' after function name.");
        try parser.block();

        const func = parser.endCompiler();
        parser.emitBytes(@intFromEnum(OpCode.closure), try parser.makeConstant(Value{ .obj = &func.obj }));

        for (compiler.upvalues.items) |upvalue| {
            parser.emitByte(if (upvalue.source == .local) 1 else 0);
            parser.emitByte(upvalue.index);
        }
    }

    fn funDeclaration(parser: *Parser) !void {
        const global = try parser.parseVariable("Expect function name.");
        parser.markInitialized();
        try parser.function(.function);
        try parser.defineVariable(global);
    }

    fn varDeclaration(parser: *Parser) !void {
        const global = try parser.parseVariable("Expected variable name.");

        if (try parser.match(.equal)) {
            try parser.expression();
        } else {
            parser.emitByte(@intFromEnum(OpCode.nil));
        }

        try parser.consume(.semicolon, "Expected ';' after variable declaration,");
        try parser.defineVariable(global);
    }

    fn declaration(parser: *Parser) anyerror!void {
        if (try parser.match(.@"var")) {
            try parser.varDeclaration();
        } else if (try parser.match(.fun)) {
            try parser.funDeclaration();
        } else {
            try parser.statement();
        }

        if (parser.panic_mode) try parser.synchronize();
    }

    fn statement(parser: *Parser) !void {
        if (try parser.match(.print)) {
            try parser.printStatement();
        } else if (try parser.match(.@"if")) {
            try parser.ifStatement();
        } else if (try parser.match(.@"return")) {
            try parser.retStatement();
        } else if (try parser.match(.@"while")) {
            try parser.whileStatement();
        } else if (try parser.match(.@"for")) {
            try parser.forStatement();
        } else if (try parser.match(.left_brace)) {
            parser.beginScope();
            try parser.block();
            parser.endScope();
        } else {
            try parser.expressionStatement();
        }
    }

    fn expressionStatement(parser: *Parser) !void {
        try parser.expression();
        try parser.consume(.semicolon, "Expected ';' after expression.");
        parser.emitByte(@intFromEnum(OpCode.pop));
    }

    fn ifStatement(parser: *Parser) anyerror!void {
        try parser.consume(.left_paren, "Expected '(' after 'if'.");
        try parser.expression();
        try parser.consume(.right_paren, "Expected ')' after condition.");

        const then_jump = parser.emitJump(@intFromEnum(OpCode.jump_if_false));
        parser.emitByte(@intFromEnum(OpCode.pop));
        try parser.statement();

        const elseJump = parser.emitJump(@intFromEnum(OpCode.jump));
        parser.patchJump(then_jump);
        parser.emitByte(@intFromEnum(OpCode.pop));

        if (try parser.match(.@"else")) {
            try parser.statement();
        }

        parser.patchJump(elseJump);
    }

    fn whileStatement(parser: *Parser) anyerror!void {
        const loop_start = parser.currentChunk().code.items.len;
        try parser.consume(.left_paren, "Expected '(' after 'while'.");
        try parser.expression();
        try parser.consume(.right_paren, "Expected ')' after condition.");

        const exit_jump = parser.emitJump(@intFromEnum(OpCode.jump_if_false));
        parser.emitByte(@intFromEnum(OpCode.pop));
        try parser.statement();
        parser.emitLoop(loop_start);

        parser.patchJump(exit_jump);
        parser.emitByte(@intFromEnum(OpCode.pop));
    }

    fn forStatement(parser: *Parser) anyerror!void {
        parser.beginScope();

        try parser.consume(.left_paren, "Expected '(' after 'for'.");
        if (try parser.match(.semicolon)) {
            // no initializer
        } else if (try parser.match(.@"var")) {
            try parser.varDeclaration();
        } else {
            try parser.expressionStatement();
        }

        var loop_start = parser.currentChunk().code.items.len;

        // condition clause
        var exit_jump: ?usize = null;
        if (!try parser.match(.semicolon)) {
            try parser.expression();
            try parser.consume(.semicolon, "Expected ';' after loop condition.");

            // jump out of loop if condition is false
            exit_jump = parser.emitJump(@intFromEnum(OpCode.jump_if_false));
            parser.emitByte(@intFromEnum(OpCode.pop)); // Condition
        }

        // Increment Clause
        if (!try parser.match(.right_paren)) {
            const body_jump = parser.emitJump(@intFromEnum(OpCode.jump));
            const increment_start = parser.currentChunk().code.items.len;
            try parser.expression();
            parser.emitByte(@intFromEnum(OpCode.pop));
            try parser.consume(.right_paren, "Expect ')' after for loop clauses.");

            parser.emitLoop(loop_start);
            loop_start = increment_start;
            parser.patchJump(body_jump);
        }

        try parser.statement();
        parser.emitLoop(loop_start);

        if (exit_jump) |jump| {
            parser.patchJump(jump);
            parser.emitByte(@intFromEnum(OpCode.pop)); // Condition
        }

        parser.endScope();
    }

    fn printStatement(parser: *Parser) !void {
        try parser.expression();
        try parser.consume(.semicolon, "Expected ';' after value.");
        parser.emitByte(@intFromEnum(OpCode.print));
    }

    fn retStatement(parser: *Parser) !void {
        if (parser.compiler.func_t == .script) {
            parser.err("Can't return from top-level code.");
        }

        if (try parser.match(.semicolon)) {
            parser.emitReturn();
        } else {
            try parser.expression();
            try parser.consume(.semicolon, "Expect ';' after value.");
            parser.emitByte(@intFromEnum(OpCode.ret));
        }
    }

    fn synchronize(parser: *Parser) !void {
        parser.panic_mode = false;

        while (!try parser.match(.eof)) {
            if (parser.previous.ttype == .semicolon) return;
            switch (parser.current.ttype) {
                .class,
                .fun,
                .@"var",
                .@"for",
                .@"if",
                .@"while",
                .print,
                .@"return",
                => return,
                else => try parser.advance(),
            }
        }
    }

    fn number(parser: *Parser, can_assign: bool) !void {
        _ = can_assign;
        const value = try std.fmt.parseFloat(f64, parser.previous.lexeme);
        try parser.emitConstant(Value{ .number = value });
    }

    fn match(parser: *Parser, ttype: TokenType) !bool {
        if (!(parser.current.ttype == ttype)) return false;
        try parser.advance();
        return true;
    }

    fn check(parser: *Parser, ttype: TokenType) bool {
        return parser.current.ttype == ttype;
    }

    fn grouping(parser: *Parser, can_assign: bool) !void {
        _ = can_assign;
        try parser.expression();
        try parser.consume(.right_paren, "expected ')' after expression");
    }

    fn string(parser: *Parser, can_assign: bool) !void {
        _ = can_assign;
        const bytes_len = parser.previous.lexeme.len;
        // discards '""'
        const str = try Obj.String.copy(parser.vm, parser.previous.lexeme[1 .. bytes_len - 1]);
        try parser.emitConstant(Value{ .obj = &str.obj });
    }

    fn literal(parser: *Parser, can_assign: bool) !void {
        _ = can_assign;
        switch (parser.previous.ttype) {
            .false => parser.emitByte(@intFromEnum(OpCode.false)),
            .true => parser.emitByte(@intFromEnum(OpCode.true)),
            .nil => parser.emitByte(@intFromEnum(OpCode.nil)),
            else => unreachable,
        }
    }

    fn variable(parser: *Parser, can_assign: bool) !void {
        try parser.namedVariable(parser.previous, can_assign);
    }

    fn namedVariable(parser: *Parser, name: Token, can_assign: bool) !void {
        var get_op: ?OpCode = null;
        var set_op: ?OpCode = null;
        var arg: u8 = undefined;

        if (parser.resolveLocal(parser.compiler, name)) |local| {
            arg = local;
            get_op = .get_local;
            set_op = .set_local;
        } else if (try parser.resolveUpvalue(parser.compiler, name)) |upvalue| {
            arg = upvalue;
            get_op = .get_upvalue;
            set_op = .set_upvalue;
        } else {
            arg = try parser.identifierConstant(name);
            get_op = .get_global;
            set_op = .set_global;
        }

        if (can_assign and try parser.match(.equal)) {
            try parser.expression();
            parser.emitBytes(@as(u8, @intCast(@intFromEnum(set_op.?))), arg);
        } else {
            parser.emitBytes(@as(u8, @intCast(@intFromEnum(get_op.?))), arg);
        }
    }

    fn resolveLocal(parser: *Parser, compiler: *Compiler, name: Token) ?u8 {
        var i: usize = compiler.local_count;
        while (i > 0) {
            i -= 1;
            const local = compiler.locals[i];
            if (std.mem.eql(u8, name.lexeme, local.name)) {
                if (local.depth == null) {
                    parser.err("Can't read local variable in its own initializer.");
                }
                return @as(u8, @intCast(i));
            }
        }

        return null;
    }

    fn resolveUpvalue(parser: *Parser, compiler: *Compiler, name: Token) !?u8 {
        if (compiler.enclosing) |enclosing| {
            if (parser.resolveLocal(enclosing, name)) |local| {
                enclosing.locals[local].is_captured = true;
                return try compiler.addUpvalue(local, .local);
            } else if (try parser.resolveUpvalue(enclosing, name)) |upvalue| {
                return try compiler.addUpvalue(upvalue, .enclosing);
            }
        }

        return null;
    }

    fn unary(parser: *Parser, can_assign: bool) !void {
        _ = can_assign;
        const optype = parser.previous.ttype;

        // compile operand
        try parser.parsePrecedence(.prec_unary);

        switch (optype) {
            .minus => parser.emitByte(@intFromEnum(OpCode.negate)),
            .bang => parser.emitByte(@intFromEnum(OpCode.not)),
            else => unreachable,
        }
    }

    fn binary(parser: *Parser, can_assign: bool) !void {
        _ = can_assign;
        const optype = parser.previous.ttype;
        const rule = getRule(optype);
        try parser.parsePrecedence(@enumFromInt(@intFromEnum(rule.precedence) + 1));

        switch (optype) {
            .plus => parser.emitByte(@intFromEnum(OpCode.add)),
            .minus => parser.emitByte(@intFromEnum(OpCode.sub)),
            .star => parser.emitByte(@intFromEnum(OpCode.mul)),
            .slash => parser.emitByte(@intFromEnum(OpCode.div)),
            .bang_equal => parser.emitBytes(@intFromEnum(OpCode.equal), @intFromEnum(OpCode.not)),
            .equal_equal => parser.emitByte(@intFromEnum(OpCode.equal)),
            .greater => parser.emitByte(@intFromEnum(OpCode.greater)),
            .greater_equal => parser.emitBytes(@intFromEnum(OpCode.less), @intFromEnum(OpCode.not)),
            .less => parser.emitByte(@intFromEnum(OpCode.less)),
            .less_equal => parser.emitBytes(@intFromEnum(OpCode.greater), @intFromEnum(OpCode.not)),
            else => unreachable,
        }
    }

    fn call(parser: *Parser, can_assign: bool) !void {
        _ = can_assign;
        const arg_count = try parser.argumentList();
        parser.emitBytes(@intFromEnum(OpCode.call), arg_count);
    }

    fn emitConstant(parser: *Parser, value: Value) !void {
        parser.emitBytes(@intFromEnum(OpCode.constant), try parser.makeConstant(value));
    }

    fn makeConstant(parser: *Parser, value: Value) !u8 {
        parser.vm.push(value);
        const constant = parser.currentChunk().addConstant(value) catch {
            parser.err("Err adding constant");
            return CompilerError.CompilerError;
        };
        _ = parser.vm.pop();

        if (constant > std.math.maxInt(u8)) {
            parser.err("Too many constants in a chunk");
            return CompilerError.CompilerError;
        }

        return @as(u8, @intCast(constant));
    }

    fn patchJump(parser: *Parser, offset: usize) void {
        // -2 to adjust for the bytecode for the jump offset itparser.
        const jump = parser.currentChunk().code.items.len - offset - 2;

        if (jump > std.math.maxInt(u16)) {
            parser.err("Too much code to jump over.");
        }

        parser.currentChunk().code.items[offset] = @as(u8, @truncate((jump >> 8))) & 0xff;
        parser.currentChunk().code.items[offset + 1] = @as(u8, @truncate(jump & 0xff));
    }

    fn emitByte(parser: *Parser, byte: u8) void {
        parser.currentChunk().write(byte, parser.previous.line) catch |e| {
            std.log.err("Error {any} trying to emit byte\n", .{e});
            std.process.exit(1);
        };
    }

    fn emitBytes(parser: *Parser, byte1: u8, byte2: u8) void {
        parser.emitByte(byte1);
        parser.emitByte(byte2);
    }

    fn emitLoop(parser: *Parser, loop_start: usize) void {
        parser.emitByte(@intFromEnum(OpCode.loop));

        const offset = parser.currentChunk().code.items.len - loop_start + 2;
        if (offset > std.math.maxInt(u16)) parser.err("Loop body too large.");

        parser.emitByte(@as(u8, @truncate((offset >> 8) & 0xff)));
        parser.emitByte(@truncate(offset & 0xff));
    }

    fn emitJump(parser: *Parser, byte: u8) usize {
        parser.emitByte(byte);
        // 16-bit placeholder operand to calculate the jump
        parser.emitBytes(0xff, 0xff);
        return parser.currentChunk().code.items.len - 2;
    }

    fn currentChunk(parser: *Parser) *Chunk {
        return &parser.compiler.func.chunk;
    }

    fn endCompiler(parser: *Parser) *Obj.Function {
        parser.emitReturn();
        const func = parser.compiler.func;

        if (parser.compiler.enclosing) |compiler| {
            parser.compiler = compiler;
        }

        if (comptime debug.print_code) {
            Chunk.disassemble(&func.chunk, if (func.name) |name| name.bytes else "<script>");
            std.debug.print("-----------------\n", .{});
        }

        return func;
    }

    fn beginScope(parser: *Parser) void {
        parser.compiler.scope_depth += 1;
    }

    fn endScope(parser: *Parser) void {
        parser.compiler.scope_depth -= 1;
        while (parser.compiler.local_count > 1 and
            parser.compiler.locals[parser.compiler.local_count - 1].depth.? > parser.compiler.scope_depth)
        {
            if (parser.compiler.locals[parser.compiler.local_count - 1].is_captured) {
                parser.emitByte(@intFromEnum(OpCode.close_upvalue));
            } else {
                parser.emitByte(@intFromEnum(OpCode.pop));
            }
            parser.compiler.local_count -= 1;
        }
    }

    fn emitReturn(parser: *Parser) void {
        parser.emitByte(@intFromEnum(OpCode.nil));
        parser.emitByte(@intFromEnum(OpCode.ret));
    }

    fn errorAtCurrent(parser: *Parser, message: []const u8) void {
        parser.errorAt(parser.current, message);
    }

    fn err(parser: *Parser, message: []const u8) void {
        parser.errorAt(parser.previous, message);
    }

    fn errorAt(parser: *Parser, token: Token, message: []const u8) void {
        if (parser.panic_mode) return;

        parser.panic_mode = false;
        var stderr = std.fs.File.stderr().writer(&.{});
        const err_writer = &stderr.interface;
        err_writer.print("[line {d}] Error", .{token.line}) catch unreachable;

        switch (token.ttype) {
            .eof => err_writer.writeAll(" at end") catch unreachable,
            .err => {},
            else => err_writer.print(" at '{s}'", .{token.lexeme}) catch unreachable,
        }
        err_writer.print(": {s}\n", .{message}) catch unreachable;
    }
};
