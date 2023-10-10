const std = @import("std");
const Allocator = std.mem.Allocator;

pub const TokenType = enum(usize) {
    // Single character tokens
    LEFT_PAREN,
    RIGHT_PAREN,
    LEFT_BRACE,
    RIGHT_BRACE,
    COMMA,
    DOT,
    MINUS,
    PLUS,
    SEMICOLON,
    SLASH,
    STAR,

    // One or two character tokens
    BANG,
    BANG_EQUAL,
    EQUAL,
    EQUAL_EQUAL,
    GREATER,
    GREATER_EQUAL,
    LESS,
    LESS_EQUAL,

    // Literal
    IDENTIFIER,
    STRING,
    NUMBER,

    // Keywords
    AND,
    CLASS,
    ELSE,
    FALSE,
    FUN,
    FOR,
    IF,
    NIL,
    OR,
    PRINT,
    RETURN,
    SUPER,
    THIS,
    TRUE,
    VAR,
    WHILE,

    ERROR,
    EOF,
};

pub const Token = struct {
    ttype: TokenType,
    lexeme: []const u8,
    line: usize,

    pub fn init(ttype: TokenType, lexeme: []const u8, line: usize) Token {
        return Token{
            .ttype = ttype,
            .lexeme = lexeme,
            .line = line,
        };
    }

    pub fn toString(self: *Token, allocator: Allocator) ![]const u8 {
        const string = try std.fmt.allocPrint(
            allocator,
            "Token: {s:} -- lexeme: {s:} -- source line: {d:>}",
            .{ @tagName(self.ttype), self.lexeme, self.line },
        );

        return string;
    }
};

pub const Scanner = struct {
    source: []const u8,
    start: usize,
    current: usize,
    line: usize,

    pub fn init(source: []const u8) Scanner {
        return Scanner{ .source = source, .start = 0, .current = 0, .line = 1 };
    }

    pub fn scanToken(self: *Scanner) Token {
        self.skipWhitespace();

        self.source = self.source[self.current..];
        self.current = 0;

        if (self.isAtEnd()) {
            return self.makeToken(.EOF);
        }

        const c = self.peek();
        self.advance();

        return switch (c) {
            '(' => self.makeToken(.LEFT_PAREN),
            ')' => self.makeToken(.RIGHT_PAREN),
            '{' => self.makeToken(.LEFT_BRACE),
            '}' => self.makeToken(.RIGHT_BRACE),
            ',' => self.makeToken(.COMMA),
            '.' => self.makeToken(.DOT),
            '-' => self.makeToken(.MINUS),
            '+' => self.makeToken(.PLUS),
            ';' => self.makeToken(.SEMICOLON),
            '/' => self.makeToken(.SLASH),
            '*' => self.makeToken(.STAR),
            '!' => self.makeToken(if (self.match('=')) .BANG_EQUAL else .BANG),
            '=' => self.makeToken(if (self.match('=')) .EQUAL_EQUAL else .EQUAL),
            '<' => self.makeToken(if (self.match('=')) .LESS_EQUAL else .LESS),
            '>' => self.makeToken(if (self.match('=')) .GREATER_EQUAL else .GREATER),
            '"' => self.scanString(),
            else => {
                if (isDigit(c)) return self.scanNumber();
                if (isAlpha(c)) return self.scanIdentifier();
                if (c == '0') return self.makeToken(.EOF);
                return self.makeError("Unexpected character.\n");
            },
        };
    }

    fn scanIdentifier(self: *Scanner) Token {
        while (isAlpha(self.peek()) or isDigit(self.peek())) self.advance();

        return self.makeToken(self.identifierType());
    }

    fn identifierType(self: *Scanner) TokenType {
        return switch (self.source[0]) {
            'a' => self.checkKeyword(1, "nd", .AND),
            'c' => self.checkKeyword(1, "lass", .CLASS),
            'e' => self.checkKeyword(1, "lse", .ELSE),
            'i' => self.checkKeyword(1, "f", .IF),
            'n' => self.checkKeyword(1, "il", .NIL),
            'o' => self.checkKeyword(1, "or", .OR),
            'p' => self.checkKeyword(1, "rint", .PRINT),
            'r' => self.checkKeyword(1, "eturn", .RETURN),
            's' => self.checkKeyword(1, "uper", .SUPER),
            'v' => self.checkKeyword(1, "ar", .VAR),
            'w' => self.checkKeyword(1, "hile", .WHILE),
            'f' => {
                return switch (self.source[1]) {
                    'a' => self.checkKeyword(2, "alse", .FALSE),
                    'o' => self.checkKeyword(2, "r", .FOR),
                    'u' => self.checkKeyword(2, "un", .FUN),
                    else => .IDENTIFIER,
                };
            },
            't' => {
                return switch (self.source[1]) {
                    'h' => self.checkKeyword(2, "is", .THIS),
                    'r' => self.checkKeyword(2, "ue", .TRUE),
                    else => .IDENTIFIER,
                };
            },
            else => .IDENTIFIER,
        };
    }

    fn checkKeyword(self: *Scanner, offset: usize, str: []const u8, tokenType: TokenType) TokenType {
        if (self.current != str.len + offset) return .IDENTIFIER;
        const sourceSlice = self.source[offset..self.current];
        std.debug.assert(sourceSlice.len == str.len);
        return if (std.mem.eql(u8, sourceSlice, str)) tokenType else .IDENTIFIER;
    }

    fn scanString(self: *Scanner) Token {
        while (self.peek() != '"' and !self.isAtEnd()) {
            if (self.peek() == '\n') {
                self.line += 1;
            }
            self.advance();
        }

        if (self.isAtEnd()) {
            return self.makeError("Unterminated string.");
        }

        // The closing quote
        self.advance();
        return self.makeToken(.STRING);
    }

    fn scanNumber(self: *Scanner) Token {
        while (isDigit(self.peek())) self.advance();

        // Look for a fractional part
        if (self.peek() == '.' and isDigit(self.peekNext())) {
            // Consume the '.'
            self.advance();

            while (isDigit(self.peek())) self.advance();
        }

        return self.makeToken(.NUMBER);
    }

    fn peek(self: *Scanner) u8 {
        if (self.isAtEnd()) return 0;
        return self.source[self.current];
    }

    fn peekNext(self: *Scanner) u8 {
        if (self.current + 1 >= self.source.len) return 0;
        return self.source[self.current + 1];
    }

    fn advance(self: *Scanner) void {
        self.current += 1;
    }

    fn match(self: *Scanner, char: u8) bool {
        if (self.isAtEnd()) return false;
        if (self.peek() != char) return false;

        self.current += 1;

        return true;
    }

    pub fn isAtEnd(self: *Scanner) bool {
        return self.current >= self.source.len;
    }

    fn makeToken(self: *Scanner, tokenType: TokenType) Token {
        return Token{
            .ttype = tokenType,
            .lexeme = self.source[0..self.current],
            .line = self.line,
        };
    }

    fn makeError(self: *Scanner, message: []const u8) Token {
        return Token{
            .ttype = .ERROR,
            .lexeme = message,
            .line = self.line,
        };
    }

    fn skipWhitespace(self: *Scanner) void {
        while (true) {
            switch (self.peek()) {
                ' ', '\r', '\t' => self.advance(),
                '\n' => {
                    self.line += 1;
                    self.advance();
                },
                '/' => {
                    if (self.peekNext() == '/') {
                        // a comment goes until the end of line
                        while (self.peek() != '\n' and !self.isAtEnd()) {
                            self.advance();
                        }
                    } else {
                        return;
                    }
                },
                else => return,
            }
        }
    }
};

fn isDigit(char: u8) bool {
    return '0' <= char and char <= '9';
}

fn isAlpha(char: u8) bool {
    return (('a' <= char and char <= 'z') or
        ('A' <= char and char <= 'Z') or
        char == '_');
}
