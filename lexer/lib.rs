use crate::token::Token;

mod lexer_tests;
pub mod token;

pub struct Lexer<'a> {
	input: &'a str,
	position: usize,
	read_position: usize,
	char: u8,
}

impl<'a> Lexer<'a> {
	pub fn new(input: &'a str) -> Self {
		let mut lex = Lexer {
			input,
			position: 0,
			read_position: 0,
			char: 0,
		};

		lex.read_char();

		lex
	}

	pub fn next_token(&mut self) -> Token {
		match self.char {
			b'=' => {
				if self.peek_char() == b'=' {
					self.read_char();
					self.read_char();
					Token::EQ
				} else {
					self.read_char();
					Token::ASSIGN
				}
			}
			b'!' => {
				if self.peek_char() == b'=' {
					self.read_char();
					self.read_char();
					Token::NE
				} else {
					self.read_char();
					Token::NOT
				}
			}

			b'+' => {
				self.read_char();
				Token::PLUS
			}
			b'-' => {
				self.read_char();
				Token::MINUS
			}
			b'*' => {
				self.read_char();
				Token::MUL
			}
			b'/' => {
				self.read_char();
				Token::DIV
			}
			b'<' => {
				if self.peek_char() == b'=' {
					self.read_char();
					self.read_char();
					Token::LE
				} else {
					self.read_char();
					Token::LT
				}
			}
			b'>' => {
				if self.peek_char() == b'=' {
					self.read_char();
					self.read_char();
					Token::GE
				} else {
					self.read_char();
					Token::GT
				}
			}
			b'&' => match self.peek_char() {
				b'&' => {
					self.read_char();
					self.read_char();
					Token::AND
				}
				lex => Token::ILLEGAL(format!("&{}", lex as char)),
			},
			b'|' => match self.peek_char() {
				b'|' => {
					self.read_char();
					self.read_char();
					Token::OR
				}
				lex => Token::ILLEGAL(format!("|{}", lex as char)),
			},

			b',' => {
				self.read_char();
				Token::COMMA
			}
			b';' => {
				self.read_char();
				Token::SEMICOLON
			}
			b'(' => {
				self.read_char();
				Token::LPAREN
			}
			b')' => {
				self.read_char();
				Token::RPAREN
			}
			b'{' => {
				self.read_char();
				Token::LBRACE
			}
			b'}' => {
				self.read_char();
				Token::RBRACE
			}
			b'[' => {
				self.read_char();
				Token::LBRACKET
			}
			b']' => {
				self.read_char();
				Token::RBRACKET
			}

			b'0'..=b'9' => self.read_integer(),

			b'A'..=b'Z' | b'a'..=b'z' => self.read_identifier(),

			b'"' => self.read_string(),

			b'\t' | b' ' | b'\n' | b'\r' => {
				self.read_char();
				self.next_token()
			}

			0 => Token::EOF,
			lex => Token::ILLEGAL((lex as char).to_string()),
		}
	}

	fn read_char(&mut self) {
		if self.read_position >= self.input.len() {
			self.char = 0;
		} else {
			self.char = self.input.as_bytes()[self.read_position];
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

	fn read_integer(&mut self) -> Token {
		let position = self.position;
		while self.char.is_ascii_digit() {
			self.read_char();
		}

		let integer = &self.input[position..self.position];
		Token::INTEGER(integer.parse::<i32>().unwrap())
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
