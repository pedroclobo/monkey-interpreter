#[cfg(test)]
mod tests {
	use Lexer;
	use LexerError;
	use Location;
	use TokenKind;

	fn test(input: &str, expected: &Vec<TokenKind>) {
		let mut lexer = Lexer::new(input, "test");

		for expected_token in expected {
			let token = lexer.next_token();
			assert_eq!(token.unwrap().kind, *expected_token);
		}
	}

	fn test_error(input: &Vec<&str>, expected: &Vec<LexerError>) {
		for (input, expected) in input.iter().zip(expected.iter()) {
			let mut lexer = Lexer::new(input, "test");
			let output = lexer.next_token();

			match output {
				Ok(_) => continue,
				Err(e) => assert_eq!(e, *expected),
			}
		}
	}

	#[test]
	fn single_char_tokens() {
		let input = "=+-*/<>!,;(){}[]";

		let expected = vec![
			TokenKind::ASSIGN,
			TokenKind::PLUS,
			TokenKind::MINUS,
			TokenKind::MUL,
			TokenKind::DIV,
			TokenKind::LT,
			TokenKind::GT,
			TokenKind::NOT,
			TokenKind::COMMA,
			TokenKind::SEMICOLON,
			TokenKind::LPAREN,
			TokenKind::RPAREN,
			TokenKind::LBRACE,
			TokenKind::RBRACE,
			TokenKind::LBRACKET,
			TokenKind::RBRACKET,
		];

		test(input, &expected);
	}

	#[test]
	fn double_char_tokens() {
		let input = "== != <= >= && ||";

		let expected = vec![
			TokenKind::EQ,
			TokenKind::NE,
			TokenKind::LE,
			TokenKind::GE,
			TokenKind::AND,
			TokenKind::OR,
		];

		test(input, &expected);
	}

	#[test]
	fn identifiers() {
		let input = "five ten add";

		let expected = vec![
			TokenKind::IDENTIFIER("five".to_string()),
			TokenKind::IDENTIFIER("ten".to_string()),
			TokenKind::IDENTIFIER("add".to_string()),
		];

		test(input, &expected);
	}

	#[test]
	fn keywords() {
		let input = "fn let true false if else return";

		let expected = vec![
			TokenKind::FUNCTION,
			TokenKind::LET,
			TokenKind::TRUE,
			TokenKind::FALSE,
			TokenKind::IF,
		];

		test(input, &expected);
	}

	#[test]
	fn integers() {
		let input = "5 10 100 9999";

		let expected = vec![
			TokenKind::INTEGER(5),
			TokenKind::INTEGER(10),
			TokenKind::INTEGER(100),
			TokenKind::INTEGER(9999),
		];

		test(input, &expected);
	}

	#[test]
	fn strings() {
		let input = "\"hello\" \"world\"";

		let expected = vec![
			TokenKind::STRING("hello".to_string()),
			TokenKind::STRING("world".to_string()),
		];

		test(input, &expected);
	}

	#[test]
	fn invalid_token() {
		let inputs = vec!["let a = %", "x = `"];

		let expected = vec![
			LexerError::InvalidTokenError(
				"%".to_string(),
				Location {
					file: "test".to_string(),
					line: 1,
					column: 9,
				},
			),
			LexerError::InvalidTokenError(
				"`".to_string(),
				Location {
					file: "test".to_string(),
					line: 1,
					column: 5,
				},
			),
		];

		test_error(&inputs, &expected);
	}

	#[test]
	fn invalid_integer() {
		let inputs = vec!["let a = 4a4", "x = 4.0"];

		let expected = vec![
			LexerError::InvalidIntegerError(
				"%".to_string(),
				Location {
					file: "test".to_string(),
					line: 1,
					column: 9,
				},
			),
			LexerError::InvalidIntegerError(
				"`".to_string(),
				Location {
					file: "test".to_string(),
					line: 1,
					column: 5,
				},
			),
		];

		test_error(&inputs, &expected);
	}

	#[test]
	fn invalid_token_sequence() {
		let inputs = vec!["&|", "|-"];

		let expected = vec![
			LexerError::InvalidTokenSequenceError(
				"&|".to_string(),
				Location {
					file: "test".to_string(),
					line: 1,
					column: 2,
				},
			),
			LexerError::InvalidTokenSequenceError(
				"|-".to_string(),
				Location {
					file: "test".to_string(),
					line: 1,
					column: 2,
				},
			),
		];

		test_error(&inputs, &expected);
	}
}
