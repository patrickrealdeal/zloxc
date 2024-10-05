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

pub fn scanToken(self: *Self) Token {
    self.skipWhitespace();
    if (self.isAtEnd()) return self.makeToken(.eof);
    self.start = self.current;

    const c = self.advance();
    if (std.ascii.isAlphabetic(c)) return self.identifier();
    if (std.ascii.isDigit(c)) return self.number();

    return switch (c) {
        '(' => self.makeToken(.left_paren),
        ')' => self.makeToken(.right_paren),
        '{' => self.makeToken(.left_brace),
        '}' => self.makeToken(.right_brace),
        ';' => self.makeToken(.semicolon),
        ',' => self.makeToken(.comma),
        '.' => self.makeToken(.dot),
        '-' => self.makeToken(.minus),
        '+' => self.makeToken(.plus),
        '*' => self.makeToken(.star),
        '/' => self.makeToken(.slash),
        '!' => if (self.match('=')) self.makeToken(.bang_equal) else self.makeToken(.bang),
        '=' => if (self.match('=')) self.makeToken(.equal_equal) else self.makeToken(.equal),
        '<' => if (self.match('=')) self.makeToken(.less_equal) else self.makeToken(.less),
        '>' => if (self.match('=')) self.makeToken(.greater_equal) else self.makeToken(.greater),
        '"' => return self.string(),
        else => return self.errorToken("Unexpected character "),
    };
}

pub fn makeToken(self: *Self, ttype: TokenType) Token {
    return Token{
        .ttype = ttype,
        .lexeme = self.source[self.start..self.current],
        .line = self.line,
    };
}

fn match(self: *Self, char: u8) bool {
    if (self.isAtEnd()) {
        return false;
    }
    if (self.source[self.current] != char) {
        return false;
    }

    self.current += 1;
    return true;
}

fn skipWhitespace(self: *Self) void {
    while (true) {
        switch (self.peek()) {
            ' ', '\t', '\r' => self.current += 1,
            '\n' => {
                self.line += 1;
                self.current += 1;
            },
            '/' => {
                if (self.peekNext() == '/') {
                    while (self.peek() == '\n' and !self.isAtEnd()) {
                        self.current += 1;
                    }
                } else {
                    return;
                }
            },
            else => return,
        }
    }
}

fn string(self: *Self) Token {
    while (self.peek() != '"' and !self.isAtEnd()) {
        if (self.peek() == '\n') {
            self.line += 1;
        }
        self.current += 1;
    }

    if (self.isAtEnd()) return self.errorToken("Unterminated string");

    // The closing quote
    self.current += 1;
    return self.makeToken(.string);
}

fn number(self: *Self) Token {
    while (std.ascii.isDigit(self.peek())) {
        self.current += 1;
    }

    // Look for a fractional part
    if (self.peek() == '.' and std.ascii.isDigit(self.peekNext())) {
        // Consume the '.'
        self.current += 1;

        while (std.ascii.isDigit(self.peek())) {
            self.current += 1;
        }
    }

    return self.makeToken(.number);
}

fn identifier(self: *Self) Token {
    while (std.ascii.isAlphabetic(self.peek()) or std.ascii.isDigit(self.peek())) self.current += 1;
    return self.makeToken(self.identifierType());
}

fn identifierType(self: *Self) TokenType {
    return switch (self.source[self.start]) {
        'a' => self.checkKeyword("and", .keyword_and),
        'c' => self.checkKeyword("class", .keyword_class),
        'e' => self.checkKeyword("else", .keyword_else),
        'i' => self.checkKeyword("if", .keyword_if),
        'n' => self.checkKeyword("nil", .keyword_nil),
        'o' => self.checkKeyword("or", .keyword_or),
        'p' => self.checkKeyword("print", .keyword_print),
        'r' => self.checkKeyword("return", .keyword_return),
        's' => self.checkKeyword("super", .keyword_super),
        'v' => self.checkKeyword("var", .keyword_var),
        'w' => self.checkKeyword("while", .keyword_while),
        'f' => switch (self.source[self.start + 1]) {
            'a' => self.checkKeyword("false", .keyword_false),
            'o' => self.checkKeyword("for", .keyword_for),
            'u' => self.checkKeyword("fun", .keyword_fun),
            else => .identifier,
        },
        't' => switch (self.source[self.start + 1]) {
            'r' => self.checkKeyword("true", .keyword_true),
            'h' => self.checkKeyword("this", .keyword_this),
            else => .identifier,
        },
        else => .identifier,
    };
}

fn checkKeyword(self: *Self, keyword: []const u8, ttype: TokenType) TokenType {
    if (std.mem.eql(u8, keyword, self.source[self.start..self.current])) {
        return ttype;
    }
    return .identifier;
}

inline fn peek(self: *Self) u8 {
    if (self.isAtEnd()) return 0;
    return self.source[self.current];
}

inline fn peekNext(self: *Self) u8 {
    if (self.isAtEnd()) return 0;
    return self.source[self.current + 1];
}

fn errorToken(self: *Self, message: []const u8) Token {
    return Token{
        .ttype = .err,
        .lexeme = message,
        .line = self.line,
    };
}

fn advance(self: *Self) u8 {
    self.current += 1;
    return self.source[self.current - 1];
}

pub fn isAtEnd(self: *Self) bool {
    return self.current >= self.source.len;
}
