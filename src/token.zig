ttype: TokenType,
lexeme: []const u8,
line: usize,

pub const TokenType = enum {
    // single character tokens
    left_paren,
    right_paren,
    left_brace,
    right_brace,
    comma,
    dot,
    minus,
    plus,
    semicolon,
    slash,
    star,

    // One or two character tokens
    bang,
    bang_equal,
    equal,
    equal_equal,
    greater,
    greater_equal,
    less,
    less_equal,

    // Literals
    identifier,
    string,
    number,

    // Keywords
    keyword_and,
    keyword_class,
    keyword_else,
    keyword_false,
    keyword_for,
    keyword_fun,
    keyword_if,
    keyword_nil,
    keyword_or,
    keyword_print,
    keyword_return,
    keyword_super,
    keyword_this,
    keyword_true,
    keyword_var,
    keyword_while,

    err,
    eof,
};
