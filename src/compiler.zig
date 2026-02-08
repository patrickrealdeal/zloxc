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
const ast = @import("ast.zig");
const Node = @import("ast.zig").Node;
const Codegen = @import("codegen.zig");
const GCAllocator = @import("memory.zig").GCAllocator;
const Type = @import("type.zig").Type;
const TypeAnnotation = @import("type.zig").TypeAnnotation;
const TypeChecker = @import("typechecker.zig").TypeChecker;
const builtin = @import("builtin");

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

    pub fn deinit(compiler: *Compiler) void {
        compiler.upvalues.deinit(compiler.allocator);
    }

    pub fn addLocal(compiler: *Compiler, name: []const u8) !void {
        if (compiler.local_count == u8_max) {
            return CompilerError.TooManyLocalVariables;
        }
        compiler.locals[compiler.local_count] = Local{ .name = name, .depth = null };
        compiler.local_count += 1;
    }

    pub fn addUpvalue(compiler: *Compiler, index: u8, upv_source: UpvalueSource) !u8 {
        for (compiler.upvalues.items, 0..) |upv, i| {
            if (upv.index == index and upv.source == upv_source) {
                return @intCast(i);
            }
        }

        if (compiler.upvalues.items.len == u8_max) {
            return CompilerError.TooManyLocalVariables;
        }

        try compiler.upvalues.append(compiler.allocator, Upvalue{ .source = upv_source, .index = index });
        compiler.func.upvalue_count += 1;
        return @intCast(compiler.upvalues.items.len - 1);
    }
};

const Local = struct {
    name: []const u8,
    depth: ?u32 = null,
    is_captured: bool = false,

    pub const empty: Local = .{ .name = "" };
};

pub const UpvalueSource = enum { local, enclosing };

pub const Upvalue = struct {
    source: UpvalueSource,
    index: u8,
};

const FunctionType = enum {
    function,
    script,
};

pub fn compile(source: []const u8, vm: *VM) !?*Obj.Function {
    const total_start = std.time.nanoTimestamp();

    const gc: *GCAllocator = @ptrCast(@alignCast(vm.allocator.ptr));
    gc.disable_gc = true;
    defer gc.disable_gc = false;

    // PHASE 1: SCANNING
    const scan_start = std.time.nanoTimestamp();
    var scanner: Scanner = .init(source);
    var compiler: Compiler = try .init(vm, .script, null);
    defer compiler.deinit();
    var parser: Parser = .init(&scanner, vm, &compiler);
    vm.parser = &parser;
    defer vm.parser = null;

    try parser.advance();
    const scan_end = std.time.nanoTimestamp();

    // PHASE 2: PARSING (Build AST)
    const parse_start = std.time.nanoTimestamp();
    var _ast: std.ArrayList(*Node) = .empty;
    defer _ast.deinit(parser.allocator);

    const stack_before = vm.stack.items.len;

    while (!try parser.match(.eof)) {
        const stmt = try parser.declaration();
        try _ast.append(parser.allocator, stmt);
    }
    const parse_end = std.time.nanoTimestamp();

    // PHASE 3: TYPE CHECKING
    const typecheck_start = std.time.nanoTimestamp();
    if (!vm.is_repl) {
        vm.type_checker.reset();
        try vm.type_checker.registerNative("clock", &[_]Type{}, Type.float);
        try vm.type_checker.registerNative("sqrt", &[_]Type{Type.float}, Type.float);
        try vm.type_checker.registerNative("cos", &[_]Type{Type.float}, Type.float);
        try vm.type_checker.registerNative("sin", &[_]Type{Type.float}, Type.float);
        try vm.type_checker.registerNative("str", &[_]Type{Type.float}, Type.string);
    }

    const type_check_passed = try vm.type_checker.checkProgram(_ast.items);
    if (!type_check_passed) {
        std.debug.print("\nType checking failed with {} error(s)\n", .{vm.type_checker.errors.items.len});
        return error.TypeError;
    }
    const typecheck_end = std.time.nanoTimestamp();

    // PHASE 4: CODE GENERATION
    const codegen_start = std.time.nanoTimestamp();
    var codegen = Codegen.init(&compiler, vm);
    for (_ast.items) |stmt| {
        try codegen.emitFromAst(stmt);
    }

    const func = codegen.endCompilation();
    const codegen_end = std.time.nanoTimestamp();

    // PHASE 5: CLEANUP
    const cleanup_start = std.time.nanoTimestamp();
    const stack_after = vm.stack.items.len;
    var i: usize = stack_after;
    while (i > stack_before) : (i -= 1) {
        _ = vm.pop();
    }
    const cleanup_end = std.time.nanoTimestamp();

    const total_end = std.time.nanoTimestamp();

    // PRINT TIMING BREAKDOWN
    const to_ms = 1_000_000.0;
    const scan_ms = @as(f64, @floatFromInt(scan_end - scan_start)) / to_ms;
    const parse_ms = @as(f64, @floatFromInt(parse_end - parse_start)) / to_ms;
    const typecheck_ms = @as(f64, @floatFromInt(typecheck_end - typecheck_start)) / to_ms;
    const codegen_ms = @as(f64, @floatFromInt(codegen_end - codegen_start)) / to_ms;
    const cleanup_ms = @as(f64, @floatFromInt(cleanup_end - cleanup_start)) / to_ms;
    const total_ms = @as(f64, @floatFromInt(total_end - total_start)) / to_ms;

    std.debug.print("\n╔════════════════════════════════╗\n", .{});
    std.debug.print("║   COMPILATION BREAKDOWN        ║\n", .{});
    std.debug.print("╠════════════════════════════════╣\n", .{});
    std.debug.print("║ Scan:       {d:>7.3}ms ({d:>5.1}%) ║\n", .{ scan_ms, scan_ms / total_ms * 100 });
    std.debug.print("║ Parse:      {d:>7.3}ms ({d:>5.1}%) ║\n", .{ parse_ms, parse_ms / total_ms * 100 });
    std.debug.print("║ Type Check: {d:>7.3}ms ({d:>5.1}%) ║\n", .{ typecheck_ms, typecheck_ms / total_ms * 100 });
    std.debug.print("║ Code Gen:   {d:>7.3}ms ({d:>5.1}%) ║\n", .{ codegen_ms, codegen_ms / total_ms * 100 });
    std.debug.print("║ Cleanup:    {d:>7.3}ms ({d:>5.1}%) ║\n", .{ cleanup_ms, cleanup_ms / total_ms * 100 });
    std.debug.print("╠════════════════════════════════╣\n", .{});
    std.debug.print("║ TOTAL:      {d:>7.3}ms         ║\n", .{total_ms});
    std.debug.print("╚════════════════════════════════╝\n\n", .{});

    std.debug.print("Generated {} bytecode instructions\n", .{func.chunk.code.items.len});
    std.debug.print("Generated {} constants\n", .{func.chunk.constants.items.len});
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

const ParseFn = fn (parser: *Parser, can_assign: bool, left_node: ?*Node) anyerror!*Node;

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
        .let, .colon_equal, .colon, .arrow, .type_int, .type_float, .type_bool, .type_string, .type_void => .init(null, null, .prec_none),
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
    allocator: std.mem.Allocator,

    pub fn init(scanner: *Scanner, vm: *VM, compiler: *Compiler) Parser {
        return .{
            .current = undefined,
            .previous = undefined,
            .scanner = scanner,
            .panic_mode = false,
            .vm = vm,
            .compiler = compiler,
            .allocator = compiler.allocator,
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

    fn parsePrecedence(parser: *Parser, precedence: Precedence) !*Node {
        try parser.advance();
        const prefixRule = getRule(parser.previous.ttype).prefix orelse {
            parser.err("Expected expression");
            return CompilerError.CompilerError;
        };

        const can_assign = @intFromEnum(precedence) <= @intFromEnum(Precedence.prec_assignment);
        var left_node: *Node = try prefixRule(parser, can_assign, null);

        while (@intFromEnum(precedence) <= @intFromEnum(getRule(parser.current.ttype).precedence)) {
            try parser.advance();
            const rule = getRule(parser.previous.ttype);
            const infixRule = rule.infix orelse unreachable;
            left_node = try infixRule(parser, can_assign, left_node);
        }

        if (can_assign and try parser.match(.equal)) {
            parser.err("Invalid assignment target");
        }

        return left_node;
    }

    /// Parse a type annotation: ': int' or ': fn(int, bool) -> string'
    fn parseTypeAnnotation(parser: *Parser) !Type {
        return switch (parser.current.ttype) {
            .type_int => blk: {
                try parser.advance();
                break :blk Type.int;
            },
            .type_float => blk: {
                try parser.advance();
                break :blk Type.float;
            },
            .type_bool => blk: {
                try parser.advance();
                break :blk Type.bool;
            },
            .type_string => blk: {
                try parser.advance();
                break :blk Type.string;
            },
            .type_void => blk: {
                try parser.advance();
                break :blk Type.nil;
            },
            .fun => try parser.parseFunctionType(),
            else => {
                parser.err("Expected type annotation.");
                return Type.unknown;
            },
        };
    }

    /// Parse function type: 'fn(int, bool) -> string'
    fn parseFunctionType(parser: *Parser) anyerror!Type {
        try parser.consume(.fun, "Expect 'fn'.");
        try parser.consume(.left_paren, "Expect '(' after 'fn'.");

        var params: std.ArrayList(Type) = .empty;

        if (!parser.check(.right_paren)) {
            while (true) {
                const param_type = try parser.parseTypeAnnotation();
                try params.append(parser.allocator, param_type);
                if (!try parser.match(.comma)) break;
            }
        }

        try parser.consume(.right_paren, "Expect ')' after parameters.");
        try parser.consume(.colon, "Expect ':' before return type.");

        const return_type = try parser.allocator.create(Type);
        return_type.* = try parser.parseTypeAnnotation();

        return Type{
            .function = .{
                .params = try params.toOwnedSlice(parser.allocator),
                .return_type = return_type,
            },
        };
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

        //try parser.compiler.addLocal(name.lexeme);
    }

    fn expression(parser: *Parser) !*Node {
        return try parser.parsePrecedence(.prec_assignment);
    }

    fn and_(parser: *Parser, can_assign: bool, left_node: ?*Node) !*Node {
        _ = can_assign;
        const left = left_node orelse unreachable;

        const right = try parser.parsePrecedence(.prec_and);

        const node = try parser.allocator.create(Node);
        node.* = .{
            .logical = .{
                .left = left,
                .op = .@"and",
                .right = right,
            },
        };
        return node;
    }

    fn or_(parser: *Parser, can_assign: bool, left_node: ?*Node) !*Node {
        _ = can_assign;
        const left = left_node orelse unreachable;

        const right = try parser.parsePrecedence(.prec_or);

        const node = try parser.allocator.create(Node);
        node.* = .{
            .logical = .{
                .left = left,
                .op = .@"or",
                .right = right,
            },
        };
        return node;
    }

    fn blockStatement(parser: *Parser) !*Node {
        parser.compiler.scope_depth += 1;

        var statements: std.ArrayList(*Node) = .empty;

        while (!parser.check(.right_brace) and !parser.check(.eof)) {
            const stmt = try parser.declaration();
            try statements.append(parser.allocator, stmt);
        }

        try parser.consume(.right_brace, "Expect '}' after block.");

        parser.compiler.scope_depth -= 1;

        const node = try parser.allocator.create(Node);
        const owned_slice = try statements.toOwnedSlice(parser.allocator);
        node.* = .{ .block = .{ .statements = owned_slice } };
        return node;
    }

    /// Updated function declaration with typed parameters
    fn funDeclaration(parser: *Parser) !*Node {
        try parser.consume(.identifier, "Expect function name.");
        const name = parser.previous.lexeme;

        try parser.declareVariable();
        const scope_depth_at_declaration = parser.compiler.scope_depth;

        try parser.consume(.left_paren, "Expect '(' after function name.");

        // Parse typed parameters
        var params: std.ArrayList(ast.Parameter) = .empty;

        if (!parser.check(.right_paren)) {
            while (true) {
                if (params.items.len >= 255) {
                    parser.err("Can't have more than 255 parameters.");
                    break;
                }

                try parser.consume(.identifier, "Expect parameter name.");
                const param_name = parser.previous.lexeme;

                // Parse parameter type (after colon)
                try parser.consume(.colon, "Expect ':' after parameter name.");
                const param_type = try parser.parseTypeAnnotation();

                try params.append(parser.allocator, .{
                    .name = param_name,
                    .param_type = param_type,
                });

                if (!try parser.match(.comma)) break;
            }
        }

        try parser.consume(.right_paren, "Expect ')' after parameters.");

        // Parse return type
        var return_type: Type = Type.nil; // Default to nil (void)
        if (try parser.match(.colon)) {
            return_type = try parser.parseTypeAnnotation();
        }

        // Parse body
        try parser.consume(.left_brace, "Expect '{' before function body.");
        const body = try parser.blockStatement();

        const params_slice = try params.toOwnedSlice(parser.allocator);

        const node = try parser.allocator.create(Node);
        node.* = .{
            .function_declaration = .{
                .name = name,
                .params = params_slice,
                .body = body,
                .arity = @intCast(params_slice.len),
                .scope_depth_at_declaration = scope_depth_at_declaration,
                .return_type = return_type,
            },
        };

        return node;
    }

    fn classDeclaration(parser: *Parser) !*Node {
        try parser.consume(.identifier, "Expect class name.");
        const name_token = parser.previous;

        try parser.consume(.left_brace, "Expect '{' before class body.");
        // TODO: Methods
        try parser.consume(.right_brace, "Expect '}' after class body.");

        const node = try parser.allocator.create(Node);
        node.* = .{ .class_declaration = .{
            .name = name_token.lexeme,
            .scope_depth_at_declaration = parser.compiler.scope_depth,
        } };
        return node;
    }

    /// Updated variable declaration with types
    fn varDeclaration(parser: *Parser) !*Node {
        const is_mutable = parser.previous.ttype == .@"var"; // var vs let

        try parser.consume(.identifier, "Expected variable name.");
        const name = parser.previous.lexeme;

        try parser.declareVariable();

        var type_annotation: ?TypeAnnotation = null;
        var initializer: ?*Node = null;

        if (try parser.match(.colon_equal)) {
            // Type inference: 'let x := 5'
            type_annotation = .{
                .type_kind = Type.inferred,
                .is_mutable = is_mutable,
            };
            initializer = try parser.expression();
        } else if (try parser.match(.colon)) {
            // Explicit type: 'let x: int = 5'
            const explicit_type = try parser.parseTypeAnnotation();
            type_annotation = .{
                .type_kind = explicit_type,
                .is_mutable = is_mutable,
            };

            if (try parser.match(.equal)) {
                initializer = try parser.expression();
            }
        } else {
            // Error: must use either := for inference or : type for explicit type
            parser.errorAtCurrent("Variable declaration must use ':=' for type inference or ': type' for explicit type annotation");
        }

        try parser.consume(.semicolon, "Expected ';'");

        const node = try parser.allocator.create(Node);
        node.* = .{
            .var_declaration = .{
                .name = name,
                .initializer = initializer,
                .scope_depth_at_declaration = parser.compiler.scope_depth,
                .type_annotation = type_annotation,
                .inferred_type = Type.unknown, // Will be filled by type checker
            },
        };
        return node;
    }

    fn declaration(parser: *Parser) anyerror!*Node {
        if (try parser.match(.@"var")) {
            return try parser.varDeclaration();
        } else if (try parser.match(.let)) {
            return try parser.varDeclaration();
        } else if (try parser.match(.class)) {
            return try parser.classDeclaration();
        } else if (try parser.match(.fun)) {
            return try parser.funDeclaration();
        } else {
            return try parser.statement();
        }

        if (parser.panic_mode) try parser.synchronize();
    }

    fn statement(parser: *Parser) anyerror!*Node {
        if (try parser.match(.print)) {
            return try parser.printStatement();
        } else if (try parser.match(.@"if")) {
            return try parser.ifStatement();
        } else if (try parser.match(.@"return")) {
            return try parser.retStatement();
        } else if (try parser.match(.@"while")) {
            return try parser.whileStatement();
        } else if (try parser.match(.@"for")) {
            return try parser.forStatement();
        } else if (try parser.match(.left_brace)) {
            return try parser.blockStatement();
        } else {
            return try parser.expressionStatement();
        }
    }

    fn expressionStatement(parser: *Parser) !*Node {
        const expr_node = try parser.expression();
        try parser.consume(.semicolon, "Expected ';' after expression.");

        const statement_node = try parser.allocator.create(Node);
        statement_node.* = .{ .expression_statement = expr_node };
        return statement_node;
    }

    fn ifStatement(parser: *Parser) !*Node {
        try parser.consume(.left_paren, "Expect '(' after 'if'.");
        const condition = try parser.expression();
        try parser.consume(.right_paren, "Expect ')' after condition.");

        const then_branch = try parser.statement();

        var else_branch: ?*Node = null;
        if (try parser.match(.@"else")) {
            else_branch = try parser.statement();
        }

        const node = try parser.allocator.create(Node);
        node.* = .{ .if_statement = .{
            .condition = condition,
            .then_branch = then_branch,
            .else_branch = else_branch,
        } };
        return node;
    }

    fn whileStatement(parser: *Parser) anyerror!*Node {
        try parser.consume(.left_paren, "Expect '(' after 'while'.");
        const condition = try parser.expression();
        try parser.consume(.right_paren, "Expect ')' after condition.");

        const body = try parser.statement();

        const node = try parser.allocator.create(Node);
        node.* = .{ .while_statement = .{
            .condition = condition,
            .body = body,
        } };
        return node;
    }

    fn forStatement(parser: *Parser) !*Node {
        //parser.compiler.scope_depth += 1;

        try parser.consume(.left_paren, "Expect '(' after 'for'.");

        // Initializer
        var initializer: ?*Node = null;
        if (try parser.match(.semicolon)) {
            // No initializer
        } else if (try parser.match(.@"var")) {
            initializer = try parser.varDeclaration();
        } else if (try parser.match(.let)) {
            initializer = try parser.varDeclaration();
        } else {
            initializer = try parser.expressionStatement();
        }

        // Condition
        var condition: ?*Node = null;
        if (!parser.check(.semicolon)) {
            condition = try parser.expression();
        }
        try parser.consume(.semicolon, "Expect ';' after loop condition.");

        // Increment
        var increment: ?*Node = null;
        if (!parser.check(.right_paren)) {
            increment = try parser.expression();
        }
        try parser.consume(.right_paren, "Expect ')' after for clauses.");

        const body = try parser.statement();

        //parser.compiler.scope_depth -= 1;

        const node = try parser.allocator.create(Node);
        node.* = .{ .for_statement = .{
            .initializer = initializer,
            .condition = condition,
            .increment = increment,
            .body = body,
        } };
        return node;
    }

    fn printStatement(parser: *Parser) !*Node {
        const value_to_print = try parser.expression();
        try parser.consume(.semicolon, "Expected ';' after value.");

        const statement_node = try parser.allocator.create(Node);
        statement_node.* = .{ .print_statement = value_to_print };

        return statement_node;
    }

    fn retStatement(parser: *Parser) !*Node {
        var value: ?*Node = null;
        if (!try parser.match(.semicolon)) {
            value = try parser.expression();
            try parser.consume(.semicolon, "Expect ';' after return value.");
        }

        const node = try parser.allocator.create(Node);
        node.* = .{ .ret_statement = .{ .value = value } };
        return node;
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

    fn number(parser: *Parser, can_assign: bool, left_node: ?*Node) !*Node {
        _ = can_assign;
        _ = left_node;
        const value = try std.fmt.parseFloat(f64, parser.previous.lexeme);

        const node = try parser.allocator.create(Node);
        node.* = .{
            .number_literal = .{
                .value = Value.fromNumber(value),
                .lexeme = parser.previous.lexeme,
            },
        };
        return node;
    }

    fn match(parser: *Parser, ttype: TokenType) !bool {
        if (!(parser.current.ttype == ttype)) return false;
        try parser.advance();
        return true;
    }

    fn check(parser: *Parser, ttype: TokenType) bool {
        return parser.current.ttype == ttype;
    }

    fn grouping(parser: *Parser, can_assign: bool, left_node: ?*Node) !*Node {
        _ = can_assign;
        _ = left_node;
        const inner_expr_node = try parser.expression();
        try parser.consume(.right_paren, "expected ')' after expression");
        return inner_expr_node;
    }

    fn string(parser: *Parser, can_assign: bool, left_node: ?*Node) !*Node {
        _ = can_assign;
        _ = left_node;

        const bytes_len = parser.previous.lexeme.len;
        const str = try Obj.String.copy(parser.vm, parser.previous.lexeme[1 .. bytes_len - 1]);

        // Push to VM stack to protect from GC during parsing
        parser.vm.push(Value.fromObj(&str.obj));

        const node = try parser.allocator.create(Node);
        node.* = .{ .literal = Value.fromObj(&str.obj) };
        return node;
    }

    fn literal(parser: *Parser, can_assign: bool, left_node: ?*Node) !*Node {
        _ = can_assign;
        _ = left_node;

        const value: Value = switch (parser.previous.ttype) {
            .false => Value.fromBool(false),
            .true => Value.fromBool(true),
            .nil => Value.fromNil(),
            else => unreachable,
        };

        const node = try parser.allocator.create(Node);
        node.* = .{
            .literal = value,
        };
        return node;
    }

    fn variable(parser: *Parser, can_assign: bool, left_node: ?*Node) !*Node {
        _ = left_node;

        const name = parser.previous.lexeme;
        const name_token = parser.previous; // Save token for location

        if (can_assign and try parser.match(.equal)) {
            const value = try parser.expression();
            const node = try parser.allocator.create(Node);
            node.* = .{
                .assignment = .{
                    .target = name,
                    .value = value,
                    .loc = .{
                        .line = name_token.line,
                        .column = 0, // We don't track columns in tokens yet
                        .length = name.len,
                    },
                },
            };
            return node;
        }

        // Otherwise it's a variable reference
        const node = try parser.allocator.create(Node);
        node.* = .{ .var_ref = .{ .name = name } };
        return node;
    }

    fn unary(parser: *Parser, can_assign: bool, left_node: ?*Node) !*Node {
        _ = can_assign;
        _ = left_node;
        const optype = parser.previous.ttype;

        // compile operand
        const right_node = try parser.parsePrecedence(.prec_unary);

        const node = try parser.allocator.create(Node);
        node.* = .{
            .unary = .{
                .op = optype,
                .right = right_node,
            },
        };
        return node;
    }

    fn binary(parser: *Parser, can_assign: bool, left_node: ?*Node) !*Node {
        _ = can_assign;
        const left = left_node orelse unreachable;
        const optype = parser.previous.ttype;
        const rule = getRule(optype);

        const right = try parser.parsePrecedence(@enumFromInt(@intFromEnum(rule.precedence) + 1));

        const node = try parser.allocator.create(Node);
        node.* = .{
            .binary = .{
                .left = left,
                .op = optype,
                .right = right,
                .result_type = .inferred, // TODO:
            },
        };

        return node;
    }

    fn call(parser: *Parser, can_assign: bool, left_node: ?*Node) !*Node {
        _ = can_assign;
        const callee = left_node orelse unreachable;

        // Parse arguments
        var arguments: std.ArrayList(*Node) = .empty;

        if (!parser.check(.right_paren)) {
            while (true) {
                if (arguments.items.len >= 255) {
                    parser.err("Can't have more than 255 arguments.");
                    break;
                }
                const arg = try parser.expression();
                try arguments.append(parser.allocator, arg);

                if (!try parser.match(.comma)) break;
            }
        }

        try parser.consume(.right_paren, "Expect ')' after arguments.");

        const node = try parser.allocator.create(Node);
        node.* = .{
            .call = .{
                .callee = callee,
                .arguments = try arguments.toOwnedSlice(parser.allocator),
            },
        };
        return node;
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
