use lexer::{LexerError, Location, TokenKind};

pub enum ParserError {
    LexerError(LexerError),
    ExpectedError(TokenKind, Location),
    InvalidPrefixError(TokenKind, Location),
}

impl std::fmt::Display for ParserError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            ParserError::LexerError(l) => write!(f, "{}", l.to_string()),
            ParserError::ExpectedError(expected, location) => {
                write!(f, "{} - PARSER ERROR: Expected {}", location, expected)
            }
            ParserError::InvalidPrefixError(expected, location) => write!(
                f,
                "{} - PARSER ERROR: Invalid prefix operator {}",
                location, expected
            ),
        }
    }
}

impl From<LexerError> for ParserError {
    fn from(err: LexerError) -> Self {
        ParserError::LexerError(err)
    }
}
