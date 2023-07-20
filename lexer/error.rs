use crate::location::Location;

#[derive(Debug, PartialEq)]
pub enum LexerError {
	InvalidTokenError(String, Location),
	InvalidTokenSequenceError(String, Location),
	InvalidIntegerError(String, Location),
}

impl std::fmt::Display for LexerError {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		match self {
			LexerError::InvalidTokenError(lexeme, location) => {
				write!(f, "{} - Invalid lexeme: {}", location, lexeme)
			}
			LexerError::InvalidTokenSequenceError(lexeme, location) => {
				write!(f, "{} - Invalid lexeme: {}", location, lexeme)
			}
			LexerError::InvalidIntegerError(lexeme, location) => {
				write!(f, "{} - Invalid Integer: {}", location, lexeme)
			}
		}
	}
}
