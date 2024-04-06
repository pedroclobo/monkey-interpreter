use crate::location::Location;

#[derive(Debug, PartialEq, Clone, Eq)]
pub enum TokenKind {
    Eof,

    Identifier(String),

    Integer(i32),
    String(String),

    Assign,
    Plus,
    Minus,
    Multiplication,
    Division,
    LessThan,
    GreaterThan,
    GreaterThanOrEqual,
    LessThanOrEqual,
    Equal,
    NotEqual,
    And,
    Or,
    Not,

    Comma,
    Semicolon,
    LeftParenthesis,
    RightParenthesis,
    LeftBrace,
    RightBrace,
    LeftBracket,
    RightBracket,

    Function,
    Let,
    True,
    False,
    If,
    Else,
    Return,

    Unary,
}

impl std::fmt::Display for TokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TokenKind::Eof => write!(f, "tEOF"),
            TokenKind::Identifier(id) => write!(f, "tIDENTIFIER({})", id),
            TokenKind::Integer(i) => write!(f, "tINTEGER({})", i),
            TokenKind::String(s) => write!(f, "tSTRING({})", s),
            TokenKind::Assign => write!(f, "tASSIGN"),
            TokenKind::Plus => write!(f, "tPLUS"),
            TokenKind::Minus => write!(f, "tMINUS"),
            TokenKind::Multiplication => write!(f, "tMUL"),
            TokenKind::Division => write!(f, "tDIV"),
            TokenKind::LessThan => write!(f, "tLT"),
            TokenKind::GreaterThan => write!(f, "tGT"),
            TokenKind::GreaterThanOrEqual => write!(f, "tGE"),
            TokenKind::LessThanOrEqual => write!(f, "tLE"),
            TokenKind::Equal => write!(f, "tEQ"),
            TokenKind::NotEqual => write!(f, "tNE"),
            TokenKind::And => write!(f, "tAND"),
            TokenKind::Or => write!(f, "tOR"),
            TokenKind::Not => write!(f, "tNOT"),
            TokenKind::Comma => write!(f, "tCOMMA"),
            TokenKind::Semicolon => write!(f, "tSEMICOLON"),
            TokenKind::LeftParenthesis => write!(f, "tLPAREN"),
            TokenKind::RightParenthesis => write!(f, "tRPAREN"),
            TokenKind::LeftBrace => write!(f, "tLBRACE"),
            TokenKind::RightBrace => write!(f, "tRBRACE"),
            TokenKind::LeftBracket => write!(f, "tLBRACKET"),
            TokenKind::RightBracket => write!(f, "tRBRACKET"),
            TokenKind::Function => write!(f, "tFUNCTION"),
            TokenKind::Let => write!(f, "tLET"),
            TokenKind::True => write!(f, "tTRUE"),
            TokenKind::False => write!(f, "tFALSE"),
            TokenKind::If => write!(f, "tIF"),
            TokenKind::Else => write!(f, "tELSE"),
            TokenKind::Return => write!(f, "tRETURN"),
            TokenKind::Unary => write!(f, "tUNARY"),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub location: Location,
}

impl Token {
    pub fn new(kind: TokenKind) -> Self {
        Token {
            kind,
            location: Location::default(),
        }
    }
}

impl Default for Token {
    fn default() -> Self {
        Token {
            kind: TokenKind::Eof,
            location: Location::default(),
        }
    }
}

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.kind.fmt(f)
    }
}
