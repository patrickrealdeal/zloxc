const std = @import("std");
const Token = @import("token.zig");
const TokenType = Token.TokenType;

pub const Self = @This();

source: []const u8,
start: usize,
current: usize,
line: usize,

pub fn init(source: []const u8) Self {
    return .{
        .source = source,
        .start = 0,
        .current = 0,
        .line = 1,
    };
}

pub fn scanToken(scanner: *Self) Token {
    scanner.skipWhitespace();
    if (scanner.isAtEnd()) return scanner.makeToken(.eof);
    scanner.start = scanner.current;

    const c = scanner.advance();
    if (std.ascii.isAlphabetic(c)) return scanner.identifier();
    if (std.ascii.isDigit(c)) return scanner.number();

    return switch (c) {
        '(' => scanner.makeToken(.left_paren),
        ')' => scanner.makeToken(.right_paren),
        '{' => scanner.makeToken(.left_brace),
        '}' => scanner.makeToken(.right_brace),
        ';' => scanner.makeToken(.semicolon),
        ',' => scanner.makeToken(.comma),
        ':' => if (scanner.match('=')) scanner.makeToken(.colon_equal) else scanner.makeToken(.colon),
        '.' => scanner.makeToken(.dot),
        '-' => if (scanner.match('>')) scanner.makeToken(.arrow) else scanner.makeToken(.minus),
        '+' => scanner.makeToken(.plus),
        '*' => scanner.makeToken(.star),
        '/' => scanner.makeToken(.slash),
        '!' => if (scanner.match('=')) scanner.makeToken(.bang_equal) else scanner.makeToken(.bang),
        '=' => if (scanner.match('=')) scanner.makeToken(.equal_equal) else scanner.makeToken(.equal),
        '<' => if (scanner.match('=')) scanner.makeToken(.less_equal) else scanner.makeToken(.less),
        '>' => if (scanner.match('=')) scanner.makeToken(.greater_equal) else scanner.makeToken(.greater),
        '"' => return scanner.string(),
        else => {
            std.debug.print("char {c}\n", .{c});
            return scanner.errorToken("Unexpected character ");
        },
    };
}

pub fn makeToken(scanner: *Self, ttype: TokenType) Token {
    return Token{
        .ttype = ttype,
        .lexeme = scanner.source[scanner.start..scanner.current],
        .line = scanner.line,
    };
}

fn match(scanner: *Self, char: u8) bool {
    if (scanner.isAtEnd()) {
        return false;
    }
    if (scanner.source[scanner.current] != char) {
        return false;
    }

    _ = scanner.advance();
    return true;
}

fn skipWhitespace(scanner: *Self) void {
    while (true) {
        switch (scanner.peek()) {
            ' ', '\t', '\r' => _ = scanner.advance(),
            '\n' => {
                scanner.line += 1;
                _ = scanner.advance();
            },
            '/' => {
                if (scanner.peekNext() == '/') {
                    while (scanner.peek() != '\n' and !scanner.isAtEnd()) {
                        _ = scanner.advance();
                    }
                } else {
                    return;
                }
            },
            else => return,
        }
    }
}

fn string(scanner: *Self) Token {
    while (scanner.peek() != '"' and !scanner.isAtEnd()) {
        if (scanner.peek() == '\n') {
            scanner.line += 1;
        }
        _ = scanner.advance();
    }

    if (scanner.isAtEnd()) return scanner.errorToken("Unterminated string");

    // The closing quote
    _ = scanner.advance();
    return scanner.makeToken(.string);
}

fn number(scanner: *Self) Token {
    while (std.ascii.isDigit(scanner.peek())) {
        _ = scanner.advance();
    }

    // Look for a fractional part
    if (scanner.peek() == '.' and std.ascii.isDigit(scanner.peekNext())) {
        // Consume the '.'
        _ = scanner.advance();

        while (std.ascii.isDigit(scanner.peek())) {
            _ = scanner.advance();
        }
    }

    return scanner.makeToken(.number);
}

fn identifier(scanner: *Self) Token {
    while (std.ascii.isAlphabetic(scanner.peek()) or std.ascii.isDigit(scanner.peek())) _ = scanner.advance();
    return scanner.makeToken(scanner.identifierType());
}

fn identifierType(scanner: *Self) TokenType {
    return switch (scanner.source[scanner.start]) {
        'l' => scanner.checkKeyword("let", .let),
        'a' => scanner.checkKeyword("and", .@"and"),
        'c' => scanner.checkKeyword("class", .class),
        'e' => scanner.checkKeyword("else", .@"else"),
        'i' => switch (scanner.source[scanner.start + 1]) {
            'f' => scanner.checkKeyword("if", .@"if"),
            'n' => scanner.checkKeyword("int", .type_int),
            else => .identifier,
        },
        'b' => scanner.checkKeyword("bool", .type_bool),
        'n' => scanner.checkKeyword("nil", .nil),
        'o' => scanner.checkKeyword("or", .@"or"),
        'p' => scanner.checkKeyword("print", .print),
        'r' => scanner.checkKeyword("return", .@"return"),
        's' => switch (scanner.source[scanner.start + 1]) {
            'u' => scanner.checkKeyword("super", .super),
            't' => scanner.checkKeyword("string", .type_string),
            else => .identifier,
        },
        'v' => switch (scanner.source[scanner.start + 1]) {
            'a' => scanner.checkKeyword("var", .@"var"),
            'o' => scanner.checkKeyword("void", .type_void),
            else => .identifier,
        },
        'w' => scanner.checkKeyword("while", .@"while"),
        'f' => switch (scanner.source[scanner.start + 1]) {
            'a' => scanner.checkKeyword("false", .false),
            'o' => scanner.checkKeyword("for", .@"for"),
            'u' => scanner.checkKeyword("fun", .fun),
            'l' => scanner.checkKeyword("float", .type_float),
            else => .identifier,
        },
        't' => switch (scanner.source[scanner.start + 1]) {
            'r' => scanner.checkKeyword("true", .true),
            'h' => scanner.checkKeyword("this", .this),
            else => .identifier,
        },
        else => .identifier,
    };
}

fn checkKeyword(scanner: *Self, keyword: []const u8, ttype: TokenType) TokenType {
    if (std.mem.eql(u8, keyword, scanner.source[scanner.start..scanner.current])) {
        return ttype;
    }
    return .identifier;
}

inline fn peek(scanner: *Self) u8 {
    if (scanner.isAtEnd()) return 0;
    return scanner.source[scanner.current];
}

inline fn peekNext(scanner: *Self) u8 {
    if (scanner.isAtEnd()) return 0;
    return scanner.source[scanner.current + 1];
}

fn errorToken(scanner: *Self, message: []const u8) Token {
    return Token{
        .ttype = .err,
        .lexeme = message,
        .line = scanner.line,
    };
}

fn advance(scanner: *Self) u8 {
    scanner.current += 1;
    return scanner.source[scanner.current - 1];
}

pub fn isAtEnd(scanner: *Self) bool {
    return scanner.current >= scanner.source.len;
}

