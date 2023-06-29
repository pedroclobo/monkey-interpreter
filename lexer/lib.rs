use crate::token::Token;

mod lexer_tests;
pub mod token;

#[derive(Debug, PartialEq, Clone)]
pub enum LexerError {
	InvalidToken(String, String, usize, usize),
	InvalidInteger(String, String, usize, usize),
}

impl std::fmt::Display for LexerError {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		match self {
			LexerError::InvalidToken(lexeme, file, line, column) => write!(
				f,
				"{}:{}:{} - Invalid lexeme: {}",
				file, line, column, lexeme
			),
			LexerError::InvalidInteger(lexeme, file, line, column) => write!(
				f,
				"{}:{}:{} - Invalid Integer: {}",
				file, line, column, lexeme
			),
		}
	}
}

pub struct Lexer<'a> {
	pub file: &'a str,
	pub line: usize,
	pub column: usize,

	input: &'a str,
	position: usize,
	read_position: usize,
	char: u8,
}

impl<'a> Lexer<'a> {
	pub fn new(input: &'a str, file: &'a str) -> Self {
		let mut lex = Lexer {
			input,
			file,
			line: 0,
			column: 0,
			position: 0,
			read_position: 0,
			char: 0,
		};

		lex.read_char();

		lex
	}

	pub fn next_token(&mut self) -> Result<Token, LexerError> {
		match self.char {
			b'=' => {
				if self.peek_char() == b'=' {
					self.read_char();
					self.read_char();
					Ok(Token::EQ)
				} else {
					self.read_char();
					Ok(Token::ASSIGN)
				}
			}
			b'!' => {
				if self.peek_char() == b'=' {
					self.read_char();
					self.read_char();
					Ok(Token::NE)
				} else {
					self.read_char();
					Ok(Token::NOT)
				}
			}

			b'+' => {
				self.read_char();
				Ok(Token::PLUS)
			}
			b'-' => {
				self.read_char();
				Ok(Token::MINUS)
			}
			b'*' => {
				self.read_char();
				Ok(Token::MUL)
			}
			b'/' => {
				self.read_char();
				Ok(Token::DIV)
			}
			b'<' => {
				if self.peek_char() == b'=' {
					self.read_char();
					self.read_char();
					Ok(Token::LE)
				} else {
					self.read_char();
					Ok(Token::LT)
				}
			}
			b'>' => {
				if self.peek_char() == b'=' {
					self.read_char();
					self.read_char();
					Ok(Token::GE)
				} else {
					self.read_char();
					Ok(Token::GT)
				}
			}
			b'&' => match self.peek_char() {
				b'&' => {
					self.read_char();
					self.read_char();
					Ok(Token::AND)
				}
				lex => Err(LexerError::InvalidToken(
					format!("&{}", lex as char),
					self.file.to_string(),
					self.line,
					self.column,
				)),
			},
			b'|' => match self.peek_char() {
				b'|' => {
					self.read_char();
					self.read_char();
					Ok(Token::OR)
				}
				lex => Err(LexerError::InvalidToken(
					format!("|{}", lex as char),
					self.file.to_string(),
					self.line,
					self.column,
				)),
			},

			b',' => {
				self.read_char();
				Ok(Token::COMMA)
			}
			b';' => {
				self.read_char();
				Ok(Token::SEMICOLON)
			}
			b'(' => {
				self.read_char();
				Ok(Token::LPAREN)
			}
			b')' => {
				self.read_char();
				Ok(Token::RPAREN)
			}
			b'{' => {
				self.read_char();
				Ok(Token::LBRACE)
			}
			b'}' => {
				self.read_char();
				Ok(Token::RBRACE)
			}
			b'[' => {
				self.read_char();
				Ok(Token::LBRACKET)
			}
			b']' => {
				self.read_char();
				Ok(Token::RBRACKET)
			}

			b'0'..=b'9' => self.read_integer(),

			b'A'..=b'Z' | b'a'..=b'z' => Ok(self.read_identifier()),

			b'"' => Ok(self.read_string()),

			b'\n' | b'\t' | b' ' | b'\r' => {
				self.read_char();
				self.next_token()
			}

			0 => Ok(Token::EOF),

			lex => Err(LexerError::InvalidToken(
				format!("{}", lex as char),
				self.file.to_string(),
				self.line,
				self.column,
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
			self.line += 1;
			self.column = 0;
		} else {
			self.column += 1;
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
		let position = self.position;
		while self.char.is_ascii_alphabetic() || self.char == b'_' {
			self.read_char();
		}

		let identifier = &self.input[position..self.position];
		Token::lookup_identifier(identifier)
	}

	fn read_integer(&mut self) -> Result<Token, LexerError> {
		let position = self.position;
		while self.char.is_ascii_digit() {
			self.read_char();
		}

		match &self.input[position..self.position].parse::<i32>() {
			Ok(integer) => Ok(Token::INTEGER(*integer)),
			Err(_) => Err(LexerError::InvalidInteger(
				self.file.to_string(),
				self.input[position..self.position].to_string(),
				self.line,
				self.column,
			)),
		}
	}

	fn read_string(&mut self) -> Token {
		self.read_char(); // read the opening "

		let position = self.position;
		while self.char != b'"' && self.char != 0 {
			self.read_char();
		}

		self.read_char(); // read the closing "

		let string = &self.input[position..self.position - 1];
		Token::STRING(string.to_string())
	}
}
