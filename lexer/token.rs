use crate::location::Location;

#[derive(Debug, PartialEq, Clone, Hash, Eq)]
pub enum TokenKind {
    EOF,

    IDENTIFIER(String),

    INTEGER(i32),
    STRING(String),

    ASSIGN,
    PLUS,
    MINUS,
    MUL,
    DIV,
    LT,
    GT,
    GE,
    LE,
    EQ,
    NE,
    AND,
    OR,
    NOT,

    COMMA,
    SEMICOLON,
    LPAREN,
    RPAREN,
    LBRACE,
    RBRACE,
    LBRACKET,
    RBRACKET,

    FUNCTION,
    LET,
    TRUE,
    FALSE,
    IF,
    ELSE,
    RETURN,

    UNARY,
}

impl std::fmt::Display for TokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TokenKind::EOF => write!(f, "tEOF"),
            TokenKind::IDENTIFIER(id) => write!(f, "tIDENTIFIER({})", id),
            TokenKind::INTEGER(i) => write!(f, "tINTEGER({})", i),
            TokenKind::STRING(s) => write!(f, "tSTRING({})", s),
            TokenKind::ASSIGN => write!(f, "tASSIGN"),
            TokenKind::PLUS => write!(f, "tPLUS"),
            TokenKind::MINUS => write!(f, "tMINUS"),
            TokenKind::MUL => write!(f, "tMUL"),
            TokenKind::DIV => write!(f, "tDIV"),
            TokenKind::LT => write!(f, "tLT"),
            TokenKind::GT => write!(f, "tGT"),
            TokenKind::GE => write!(f, "tGE"),
            TokenKind::LE => write!(f, "tLE"),
            TokenKind::EQ => write!(f, "tEQ"),
            TokenKind::NE => write!(f, "tNE"),
            TokenKind::AND => write!(f, "tAND"),
            TokenKind::OR => write!(f, "tOR"),
            TokenKind::NOT => write!(f, "tNOT"),
            TokenKind::COMMA => write!(f, "tCOMMA"),
            TokenKind::SEMICOLON => write!(f, "tSEMICOLON"),
            TokenKind::LPAREN => write!(f, "tLPAREN"),
            TokenKind::RPAREN => write!(f, "tRPAREN"),
            TokenKind::LBRACE => write!(f, "tLBRACE"),
            TokenKind::RBRACE => write!(f, "tRBRACE"),
            TokenKind::LBRACKET => write!(f, "tLBRACKET"),
            TokenKind::RBRACKET => write!(f, "tRBRACKET"),
            TokenKind::FUNCTION => write!(f, "tFUNCTION"),
            TokenKind::LET => write!(f, "tLET"),
            TokenKind::TRUE => write!(f, "tTRUE"),
            TokenKind::FALSE => write!(f, "tFALSE"),
            TokenKind::IF => write!(f, "tIF"),
            TokenKind::ELSE => write!(f, "tELSE"),
            TokenKind::RETURN => write!(f, "tRETURN"),
            TokenKind::UNARY => write!(f, "tUNARY"),
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
            kind: TokenKind::EOF,
            location: Location::default(),
        }
    }
}

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.kind.fmt(f)
    }
}
