#[cfg(test)]
mod tests {
	use Lexer;
	use Token;

	#[test]
	fn single_char_tokens() {
		let input = "=+-*/<>!,;(){}[]";

		let result = vec![
			Token::ASSIGN,
			Token::PLUS,
			Token::MINUS,
			Token::MUL,
			Token::DIV,
			Token::LT,
			Token::GT,
			Token::NOT,
			Token::COMMA,
			Token::SEMICOLON,
			Token::LPAREN,
			Token::RPAREN,
			Token::LBRACE,
			Token::RBRACE,
			Token::LBRACKET,
			Token::RBRACKET,
		];

		let mut lexer = Lexer::new(input, "single_char_tokens");

		for (i, expected_token) in result.iter().enumerate() {
			let token = lexer.next_token();
			assert_eq!(token.unwrap(), *expected_token, "index: {}", i);
		}
	}

	#[test]
	fn double_char_tokens() {
		let input = "== != <= >= && ||";

		let result = vec![
			Token::EQ,
			Token::NE,
			Token::LE,
			Token::GE,
			Token::AND,
			Token::OR,
		];

		let mut lexer = Lexer::new(input, "double_char_tokens");

		for (i, expected_token) in result.iter().enumerate() {
			let token = lexer.next_token();
			assert_eq!(token.unwrap(), *expected_token, "index: {}", i);
		}
	}

	#[test]
	fn identifiers() {
		let input = "five ten add";

		let result = vec![
			Token::IDENTIFIER("five".to_string()),
			Token::IDENTIFIER("ten".to_string()),
			Token::IDENTIFIER("add".to_string()),
		];

		let mut lexer = Lexer::new(input, "indentifiers");

		for (i, expected_token) in result.iter().enumerate() {
			let token = lexer.next_token();
			assert_eq!(token.unwrap(), *expected_token, "index: {}", i);
		}
	}

	#[test]
	fn keywords() {
		let input = "fn let true false if else return";

		let result = vec![
			Token::FUNCTION,
			Token::LET,
			Token::TRUE,
			Token::FALSE,
			Token::IF,
		];

		let mut lexer = Lexer::new(input, "keywords");

		for (i, expected_token) in result.iter().enumerate() {
			let token = lexer.next_token();
			assert_eq!(token.unwrap(), *expected_token, "index: {}", i);
		}
	}

	#[test]
	fn integers() {
		let input = "5 10 100 9999";

		let result = vec![
			Token::INTEGER(5),
			Token::INTEGER(10),
			Token::INTEGER(100),
			Token::INTEGER(9999),
		];

		let mut lexer = Lexer::new(input, "integers");

		for (i, expected_token) in result.iter().enumerate() {
			let token = lexer.next_token();
			assert_eq!(token.unwrap(), *expected_token, "index: {}", i);
		}
	}

	#[test]
	fn strings() {
		let input = "\"hello\" \"world\"";

		let result = vec![
			Token::STRING("hello".to_string()),
			Token::STRING("world".to_string()),
		];

		let mut lexer = Lexer::new(input, "strings");

		for (i, expected_token) in result.iter().enumerate() {
			let token = lexer.next_token();
			assert_eq!(token.unwrap(), *expected_token, "index: {}", i);
		}
	}
}
