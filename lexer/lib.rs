pub use crate::error::LexerError;
pub use crate::location::Location;
pub use crate::token::{Token, TokenKind};

pub mod error;
mod location;
mod tests;
pub mod token;

pub struct Lexer<'a> {
	pub location: Location,
	input: &'a str,
	position: usize,
	read_position: usize,
	char: u8,
}

impl<'a> Lexer<'a> {
	pub fn new(input: &'a str, file: &'a str) -> Self {
		let mut lex = Lexer {
			location: Location::default(),
			input,
			position: 0,
			read_position: 0,
			char: 0,
		};

		lex.location.file = file.to_string();
		lex.read_char();

		lex
	}

	pub fn next_token(&mut self) -> Result<Token, LexerError> {
		let mut token: Token = Token::default();
		token.location = self.location.clone();

		match self.char {
			b'=' => {
				if self.peek_char() == b'=' {
					self.read_char();
					self.read_char();
					token.kind = TokenKind::EQ;
					Ok(token)
				} else {
					self.read_char();
					token.kind = TokenKind::ASSIGN;
					Ok(token)
				}
			}
			b'!' => {
				if self.peek_char() == b'=' {
					self.read_char();
					self.read_char();
					token.kind = TokenKind::NE;
					Ok(token)
				} else {
					self.read_char();
					token.kind = TokenKind::NOT;
					Ok(token)
				}
			}

			b'+' => {
				self.read_char();
				token.kind = TokenKind::PLUS;
				Ok(token)
			}
			b'-' => {
				self.read_char();
				token.kind = TokenKind::MINUS;
				Ok(token)
			}
			b'*' => {
				self.read_char();
				token.kind = TokenKind::MUL;
				Ok(token)
			}
			b'/' => {
				self.read_char();
				token.kind = TokenKind::DIV;
				Ok(token)
			}
			b'<' => {
				if self.peek_char() == b'=' {
					self.read_char();
					self.read_char();
					token.kind = TokenKind::LE;
					Ok(token)
				} else {
					self.read_char();
					token.kind = TokenKind::LT;
					Ok(token)
				}
			}
			b'>' => {
				if self.peek_char() == b'=' {
					self.read_char();
					self.read_char();
					token.kind = TokenKind::GE;
					Ok(token)
				} else {
					self.read_char();
					token.kind = TokenKind::GT;
					Ok(token)
				}
			}
			b'&' => match self.peek_char() {
				b'&' => {
					self.read_char();
					self.read_char();
					token.kind = TokenKind::AND;
					Ok(token)
				}
				lex => Err(LexerError::InvalidTokenSequenceError(
					format!("&{}", lex as char),
					token.location.clone(),
				)),
			},
			b'|' => match self.peek_char() {
				b'|' => {
					self.read_char();
					self.read_char();
					token.kind = TokenKind::OR;
					Ok(token)
				}
				lex => Err(LexerError::InvalidTokenSequenceError(
					format!("|{}", lex as char),
					token.location.clone(),
				)),
			},

			b',' => {
				self.read_char();
				token.kind = TokenKind::COMMA;
				Ok(token)
			}
			b';' => {
				self.read_char();
				token.kind = TokenKind::SEMICOLON;
				Ok(token)
			}
			b'(' => {
				self.read_char();
				token.kind = TokenKind::LPAREN;
				Ok(token)
			}
			b')' => {
				self.read_char();
				token.kind = TokenKind::RPAREN;
				Ok(token)
			}
			b'{' => {
				self.read_char();
				token.kind = TokenKind::LBRACE;
				Ok(token)
			}
			b'}' => {
				self.read_char();
				token.kind = TokenKind::RBRACE;
				Ok(token)
			}
			b'[' => {
				self.read_char();
				token.kind = TokenKind::LBRACKET;
				Ok(token)
			}
			b']' => {
				self.read_char();
				token.kind = TokenKind::RBRACKET;
				Ok(token)
			}

			b'0'..=b'9' => self.read_integer(),

			b'A'..=b'Z' | b'a'..=b'z' => Ok(self.read_identifier()),

			b'"' => Ok(self.read_string()),

			b'\n' | b'\t' | b' ' | b'\r' => {
				self.read_char();
				self.next_token()
			}

			0 => {
				token.kind = TokenKind::EOF;
				Ok(token)
			}

			lex => Err(LexerError::InvalidTokenError(
				format!("{}", lex as char),
				token.location.clone(),
			)),
		}
	}

	fn read_char(&mut self) {
		if self.read_position >= self.input.len() {
			self.char = 0;
		} else {
			self.char = self.input.as_bytes()[self.read_position];
		}

		if self.char == b'\n' {
			self.location.line += 1;
			self.location.column = 0;
		} else {
			self.location.column += 1;
		}

		self.position = self.read_position;
		self.read_position += 1;
	}

	fn peek_char(&self) -> u8 {
		if self.read_position >= self.input.len() {
			0
		} else {
			self.input.as_bytes()[self.read_position]
		}
	}

	fn read_identifier(&mut self) -> Token {
		let mut token = Token::default();
		token.location = self.location.clone();

		let position = self.position;
		while self.char.is_ascii_alphabetic() || self.char == b'_' {
			self.read_char();
		}

		let identifier = &self.input[position..self.position];
		token.kind = Token::lookup_identifier(identifier);

		token
	}

	fn read_integer(&mut self) -> Result<Token, LexerError> {
		let mut token = Token::default();
		token.location = self.location.clone();

		let position = self.position;
		while self.char.is_ascii_digit() {
			self.read_char();
		}

		match &self.input[position..self.position].parse::<i32>() {
			Ok(integer) => {
				token.kind = TokenKind::INTEGER(*integer);
				Ok(token)
			}
			Err(_) => Err(LexerError::InvalidIntegerError(
				self.input[position..self.position].to_string(),
				token.location.clone(),
			)),
		}
	}

	fn read_string(&mut self) -> Token {
		let mut token: Token = Token::default();
		token.location = self.location.clone();

		self.read_char(); // read the opening "

		let position = self.position;
		while self.char != b'"' && self.char != 0 {
			self.read_char();
		}

		self.read_char(); // read the closing "

		let string = &self.input[position..self.position - 1];
		token.kind = TokenKind::STRING(string.to_string());
		token
	}
}
