#[cfg(test)]
mod tests {
	use Lexer;
	use LexerError;
	use Location;
	use Token;
	use TokenKind;

	fn test(input: &str, expected: &Vec<Token>) {
		let mut lexer = Lexer::new(input, "test");

		for expected_token in expected {
			let token = lexer.next_token();
			assert_eq!(token.unwrap(), *expected_token);
		}
	}

	fn test_error(input: &Vec<&str>, expected: &Vec<Vec<Result<Token, LexerError>>>) {
		for (input, expected) in input.iter().zip(expected.iter()) {
			let mut lexer = Lexer::new(input, "test");

			let mut i = 0;
			while let Ok(tok) = lexer.next_token() {
				assert_eq!(tok, *expected[i].as_ref().unwrap());
				i += 1;
			}
		}
	}

	#[test]
	fn single_char_tokens() {
		let input = "=+-*/<>!,;(){}[]";

		let expected = vec![
			Token {
				kind: TokenKind::ASSIGN,
				location: Location {
					file: "test".to_string(),
					line: 1,
					column: 1,
				},
			},
			Token {
				kind: TokenKind::PLUS,
				location: Location {
					file: "test".to_string(),
					line: 1,
					column: 2,
				},
			},
			Token {
				kind: TokenKind::MINUS,
				location: Location {
					file: "test".to_string(),
					line: 1,
					column: 3,
				},
			},
			Token {
				kind: TokenKind::MUL,
				location: Location {
					file: "test".to_string(),
					line: 1,
					column: 4,
				},
			},
			Token {
				kind: TokenKind::DIV,
				location: Location {
					file: "test".to_string(),
					line: 1,
					column: 5,
				},
			},
			Token {
				kind: TokenKind::LT,
				location: Location {
					file: "test".to_string(),
					line: 1,
					column: 6,
				},
			},
			Token {
				kind: TokenKind::GT,
				location: Location {
					file: "test".to_string(),
					line: 1,
					column: 7,
				},
			},
			Token {
				kind: TokenKind::NOT,
				location: Location {
					file: "test".to_string(),
					line: 1,
					column: 8,
				},
			},
			Token {
				kind: TokenKind::COMMA,
				location: Location {
					file: "test".to_string(),
					line: 1,
					column: 9,
				},
			},
			Token {
				kind: TokenKind::SEMICOLON,
				location: Location {
					file: "test".to_string(),
					line: 1,
					column: 10,
				},
			},
			Token {
				kind: TokenKind::LPAREN,
				location: Location {
					file: "test".to_string(),
					line: 1,
					column: 11,
				},
			},
			Token {
				kind: TokenKind::RPAREN,
				location: Location {
					file: "test".to_string(),
					line: 1,
					column: 12,
				},
			},
			Token {
				kind: TokenKind::LBRACE,
				location: Location {
					file: "test".to_string(),
					line: 1,
					column: 13,
				},
			},
			Token {
				kind: TokenKind::RBRACE,
				location: Location {
					file: "test".to_string(),
					line: 1,
					column: 14,
				},
			},
			Token {
				kind: TokenKind::LBRACKET,
				location: Location {
					file: "test".to_string(),
					line: 1,
					column: 15,
				},
			},
			Token {
				kind: TokenKind::RBRACKET,
				location: Location {
					file: "test".to_string(),
					line: 1,
					column: 16,
				},
			},
		];

		test(input, &expected);
	}

	#[test]
	fn double_char_tokens() {
		let input = "== != <= >= && ||";

		let expected = vec![
			Token {
				kind: TokenKind::EQ,
				location: Location {
					file: "test".to_string(),
					line: 1,
					column: 1,
				},
			},
			Token {
				kind: TokenKind::NE,
				location: Location {
					file: "test".to_string(),
					line: 1,
					column: 4,
				},
			},
			Token {
				kind: TokenKind::LE,
				location: Location {
					file: "test".to_string(),
					line: 1,
					column: 7,
				},
			},
			Token {
				kind: TokenKind::GE,
				location: Location {
					file: "test".to_string(),
					line: 1,
					column: 10,
				},
			},
			Token {
				kind: TokenKind::AND,
				location: Location {
					file: "test".to_string(),
					line: 1,
					column: 13,
				},
			},
			Token {
				kind: TokenKind::OR,
				location: Location {
					file: "test".to_string(),
					line: 1,
					column: 16,
				},
			},
		];

		test(input, &expected);
	}

	#[test]
	fn identifiers() {
		let input = "five ten add";

		let expected = vec![
			Token {
				kind: TokenKind::IDENTIFIER("five".to_string()),
				location: Location {
					file: "test".to_string(),
					line: 1,
					column: 1,
				},
			},
			Token {
				kind: TokenKind::IDENTIFIER("ten".to_string()),
				location: Location {
					file: "test".to_string(),
					line: 1,
					column: 6,
				},
			},
			Token {
				kind: TokenKind::IDENTIFIER("add".to_string()),
				location: Location {
					file: "test".to_string(),
					line: 1,
					column: 10,
				},
			},
		];

		test(input, &expected);
	}

	#[test]
	fn keywords() {
		let input = "fn let true false if else return";

		let expected = vec![
			Token {
				kind: TokenKind::FUNCTION,
				location: Location {
					file: "test".to_string(),
					line: 1,
					column: 1,
				},
			},
			Token {
				kind: TokenKind::LET,
				location: Location {
					file: "test".to_string(),
					line: 1,
					column: 4,
				},
			},
			Token {
				kind: TokenKind::TRUE,
				location: Location {
					file: "test".to_string(),
					line: 1,
					column: 8,
				},
			},
			Token {
				kind: TokenKind::FALSE,
				location: Location {
					file: "test".to_string(),
					line: 1,
					column: 13,
				},
			},
			Token {
				kind: TokenKind::IF,
				location: Location {
					file: "test".to_string(),
					line: 1,
					column: 19,
				},
			},
			Token {
				kind: TokenKind::ELSE,
				location: Location {
					file: "test".to_string(),
					line: 1,
					column: 22,
				},
			},
			Token {
				kind: TokenKind::RETURN,
				location: Location {
					file: "test".to_string(),
					line: 1,
					column: 27,
				},
			},
		];

		test(input, &expected);
	}

	#[test]
	fn integers() {
		let input = "5 10 100 9999";

		let expected = vec![
			Token {
				kind: TokenKind::INTEGER(5),
				location: Location {
					file: "test".to_string(),
					line: 1,
					column: 1,
				},
			},
			Token {
				kind: TokenKind::INTEGER(10),
				location: Location {
					file: "test".to_string(),
					line: 1,
					column: 3,
				},
			},
			Token {
				kind: TokenKind::INTEGER(100),
				location: Location {
					file: "test".to_string(),
					line: 1,
					column: 6,
				},
			},
			Token {
				kind: TokenKind::INTEGER(9999),
				location: Location {
					file: "test".to_string(),
					line: 1,
					column: 10,
				},
			},
		];

		test(input, &expected);
	}

	#[test]
	fn strings() {
		let input = "\"hello\" \"world\"";

		let expected = vec![
			Token {
				kind: TokenKind::STRING("hello".to_string()),
				location: Location {
					file: "test".to_string(),
					line: 1,
					column: 1,
				},
			},
			Token {
				kind: TokenKind::STRING("world".to_string()),
				location: Location {
					file: "test".to_string(),
					line: 1,
					column: 9,
				},
			},
		];

		test(input, &expected);
	}

	#[test]
	fn invalid_token() {
		let inputs = vec!["let a = %", "x = `"];

		let expected = vec![
			vec![
				Ok(Token {
					kind: TokenKind::LET,
					location: Location {
						file: "test".to_string(),
						line: 1,
						column: 1,
					},
				}),
				Ok(Token {
					kind: TokenKind::IDENTIFIER("a".to_string()),
					location: Location {
						file: "test".to_string(),
						line: 1,
						column: 5,
					},
				}),
				Ok(Token {
					kind: TokenKind::ASSIGN,
					location: Location {
						file: "test".to_string(),
						line: 1,
						column: 7,
					},
				}),
				Err(LexerError::InvalidTokenError(
					"%".to_string(),
					Location {
						file: "test".to_string(),
						line: 1,
						column: 9,
					},
				)),
			],
			vec![
				Ok(Token {
					kind: TokenKind::IDENTIFIER("x".to_string()),
					location: Location {
						file: "test".to_string(),
						line: 1,
						column: 1,
					},
				}),
				Ok(Token {
					kind: TokenKind::ASSIGN,
					location: Location {
						file: "test".to_string(),
						line: 1,
						column: 3,
					},
				}),
				Err(LexerError::InvalidTokenError(
					"`".to_string(),
					Location {
						file: "test".to_string(),
						line: 1,
						column: 5,
					},
				)),
			],
		];

		test_error(&inputs, &expected);
	}

	#[test]
	fn invalid_token_sequence() {
		let inputs = vec!["false &| true", "let a = |-"];

		let expected = vec![
			vec![
				Ok(Token {
					kind: TokenKind::FALSE,
					location: Location {
						file: "test".to_string(),
						line: 1,
						column: 1,
					},
				}),
				Err(LexerError::InvalidTokenSequenceError(
					"&|".to_string(),
					Location {
						file: "test".to_string(),
						line: 1,
						column: 7,
					},
				)),
			],
			vec![
				Ok(Token {
					kind: TokenKind::LET,
					location: Location {
						file: "test".to_string(),
						line: 1,
						column: 1,
					},
				}),
				Ok(Token {
					kind: TokenKind::IDENTIFIER("a".to_string()),
					location: Location {
						file: "test".to_string(),
						line: 1,
						column: 5,
					},
				}),
				Ok(Token {
					kind: TokenKind::ASSIGN,
					location: Location {
						file: "test".to_string(),
						line: 1,
						column: 7,
					},
				}),
				Err(LexerError::InvalidTokenSequenceError(
					"|-".to_string(),
					Location {
						file: "test".to_string(),
						line: 1,
						column: 9,
					},
				)),
			],
		];

		test_error(&inputs, &expected);
	}
}
