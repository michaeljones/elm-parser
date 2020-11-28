use logos::{Lexer, Logos};

#[derive(Logos, Debug, PartialEq)]
pub enum Token<'a> {
    // Keywords
    #[token("module")]
    Module,
    #[token("port")]
    Port,
    #[token("type")]
    Type,
    #[token("alias")]
    Alias,
    #[token("exposing")]
    Exposing,
    #[token("as")]
    As,
    #[token("import")]
    Import,
    #[token("case")]
    Case,
    #[token("of")]
    Of,
    #[token("let")]
    Let,
    #[token("in")]
    In,
    #[token("if")]
    If,
    #[token("else")]
    Else,
    #[token("then")]
    Then,

    // Open & Close
    #[token("(")]
    OpenParen,
    #[token(")")]
    CloseParen,

    #[token("[")]
    OpenBracket,
    #[token("]")]
    CloseBracket,

    #[token("{")]
    OpenBrace,
    #[token("}")]
    CloseBrace,

    // Whitespace
    #[regex(" +", |lex| lex.slice().len())]
    Space(usize),

    #[token("\n")]
    NewLine,

    // Key symbols
    #[token("|")]
    Bar,

    #[token(",")]
    Comma,

    #[token(".")]
    Point,

    #[token("..")]
    Ellipsis,

    #[token("=")]
    Equal,

    #[token(":")]
    Colon,

    #[token("\\")]
    BackSlash,

    #[token("->")]
    RightArrow,

    // Names
    #[regex("[A-Z][a-zA-Z0-9]*")]
    TypeOrModuleName(&'a str),

    #[regex("[a-z_][a-zA-Z0-9]*")]
    Name(&'a str),

    #[regex(r#"[+><!*-:|]+"#)]
    Operator(&'a str),

    #[regex("--[^\n]*")]
    SingleLineComment(&'a str),

    #[regex(r#"\{-(?:[^-]|\-[^}])*\-}"#)]
    MultiLineComment(&'a str),

    #[regex(r#"\[glsl\|(?:[^|]|\|[^]])*\|]"#)]
    WebGL(&'a str),

    #[regex("-?[0-9]+", |lex| lex.slice().parse::<i32>(), priority = 2)]
    LiteralInteger(i32),

    #[regex("[0-9]*\\.[0-9]+([eE][+-]?[0-9]+)?|[0-9]+[eE][+-]?[0-9]+")]
    LiteralFloat,

    #[regex(r#""([^"])*""#, string_contents)]
    LiteralString(&'a str),

    #[regex(r#"'[^']'"#, string_contents)]
    LiteralChar(&'a str),

    // Error
    #[error]
    Error,
}

fn string_contents<'a>(lex: &mut Lexer<'a, Token<'a>>) -> Option<&'a str> {
    let slice = lex.slice();
    Some(&slice[1..slice.len() - 1])
}
