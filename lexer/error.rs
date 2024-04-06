use crate::location::Location;

#[derive(Debug)]
pub enum LexerError {
    InvalidToken(String, Location),
    InvalidTokenSequence(String, Location),
    InvalidInteger(String, Location),
    NoMoreTokens(Location),
}

impl std::fmt::Display for LexerError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            LexerError::InvalidToken(lexeme, location) => {
                write!(f, "{}: error: invalid lexeme `{}`", location, lexeme)
            }
            LexerError::InvalidTokenSequence(lexeme, location) => {
                write!(
                    f,
                    "{}: error: invalid token sequence `{}`",
                    location, lexeme
                )
            }
            LexerError::InvalidInteger(lexeme, location) => {
                write!(f, "{}: error: invalid integer: `{}`", location, lexeme)
            }
            LexerError::NoMoreTokens(location) => {
                write!(f, "{}: error: no more tokens", location)
            }
        }
    }
}
