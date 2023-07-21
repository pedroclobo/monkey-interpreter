#[cfg(test)]
mod tests {
	use Lexer;
	use LexerError;
	use LexerError::*;
	use Location;
	use Token;
	use TokenKind::*;

	macro_rules! token {
		($kind:expr, $line:expr, $column:expr) => {
			Token {
				kind: $kind,
				location: Location {
					file: "test".to_string(),
					line: $line,
					column: $column,
				},
			}
		};
	}

	macro_rules! error {
		($kind:ident, $lexeme:expr, $line:expr, $column:expr) => {
			$kind(
				$lexeme.to_string(),
				Location {
					file: "test".to_string(),
					line: $line,
					column: $column,
				},
			)
		};
	}

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
			token!(ASSIGN, 1, 1),
			token!(PLUS, 1, 2),
			token!(MINUS, 1, 3),
			token!(MUL, 1, 4),
			token!(DIV, 1, 5),
			token!(LT, 1, 6),
			token!(GT, 1, 7),
			token!(NOT, 1, 8),
			token!(COMMA, 1, 9),
			token!(SEMICOLON, 1, 10),
			token!(LPAREN, 1, 11),
			token!(RPAREN, 1, 12),
			token!(LBRACE, 1, 13),
			token!(RBRACE, 1, 14),
			token!(LBRACKET, 1, 15),
			token!(RBRACKET, 1, 16),
		];

		test(input, &expected);
	}

	#[test]
	fn double_char_tokens() {
		let input = "== != <= >= && ||";

		let expected = vec![
			token!(EQ, 1, 1),
			token!(NE, 1, 4),
			token!(LE, 1, 7),
			token!(GE, 1, 10),
			token!(AND, 1, 13),
			token!(OR, 1, 16),
		];

		test(input, &expected);
	}

	#[test]
	fn identifiers() {
		let input = "five ten add";

		let expected = vec![
			token!(IDENTIFIER("five".to_string()), 1, 1),
			token!(IDENTIFIER("ten".to_string()), 1, 6),
			token!(IDENTIFIER("add".to_string()), 1, 10),
		];

		test(input, &expected);
	}

	#[test]
	fn keywords() {
		let input = "fn let true false if else return";

		let expected = vec![
			token!(FUNCTION, 1, 1),
			token!(LET, 1, 4),
			token!(TRUE, 1, 8),
			token!(FALSE, 1, 13),
			token!(IF, 1, 19),
			token!(ELSE, 1, 22),
			token!(RETURN, 1, 27),
		];

		test(input, &expected);
	}

	#[test]
	fn integers() {
		let input = "5 10 100 9999";

		let expected = vec![
			token!(INTEGER(5), 1, 1),
			token!(INTEGER(10), 1, 3),
			token!(INTEGER(100), 1, 6),
			token!(INTEGER(9999), 1, 10),
		];

		test(input, &expected);
	}

	#[test]
	fn strings() {
		let input = "\"hello\" \"world\"";

		let expected = vec![
			token!(STRING("hello".to_string()), 1, 1),
			token!(STRING("world".to_string()), 1, 9),
		];

		test(input, &expected);
	}

	#[test]
	fn invalid_token() {
		let inputs = vec!["let a = %", "x = `"];

		let expected = vec![
			vec![
				Ok(token!(LET, 1, 1)),
				Ok(token!(IDENTIFIER("a".to_string()), 1, 5)),
				Ok(token!(ASSIGN, 1, 7)),
				Err(error!(InvalidTokenError, "%", 1, 9)),
			],
			vec![
				Ok(token!(IDENTIFIER("x".to_string()), 1, 1)),
				Ok(token!(ASSIGN, 1, 3)),
				Err(error!(InvalidTokenError, "`", 1, 5)),
			],
		];

		test_error(&inputs, &expected);
	}

	#[test]
	fn invalid_token_sequence() {
		let inputs = vec!["false &| true", "let a = |-"];

		let expected = vec![
			vec![
				Ok(token!(FALSE, 1, 1)),
				Err(error!(InvalidTokenSequenceError, "&|", 1, 7)),
			],
			vec![
				Ok(token!(LET, 1, 1)),
				Ok(token!(IDENTIFIER("a".to_string()), 1, 5)),
				Ok(token!(ASSIGN, 1, 7)),
				Err(error!(InvalidTokenSequenceError, "|-", 1, 9)),
			],
		];

		test_error(&inputs, &expected);
	}
}
