use lexer::{LexerError, Location, TokenKind};

pub enum ParserError {
    Lexer(LexerError),
    Expected(TokenKind, Location),
    InvalidPrefix(TokenKind, Location),
}

impl std::fmt::Display for ParserError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            ParserError::Lexer(l) => write!(f, "{}", l),
            ParserError::Expected(expected, location) => {
                write!(f, "{} - PARSER ERROR: Expected {}", location, expected)
            }
            ParserError::InvalidPrefix(expected, location) => write!(
                f,
                "{} - PARSER ERROR: Invalid prefix operator {}",
                location, expected
            ),
        }
    }
}

impl From<LexerError> for ParserError {
    fn from(err: LexerError) -> Self {
        ParserError::Lexer(err)
    }
}
