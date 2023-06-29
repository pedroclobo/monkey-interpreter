#[cfg(test)]
mod tests {
	use eval;
	use lexer::Lexer;
	use parser::Parser;
	use Node;
	use Symbol;

	fn test(input: &Vec<&str>, expected: &Vec<Symbol>) {
		for (i, symbol) in expected.iter().enumerate() {
			let mut lexer = Lexer::new(input[i], "stdin");

			let mut parser = match Parser::new(&mut lexer) {
				Ok(parser) => parser,
				Err(e) => panic!("{}", e),
			};

			let program = match parser.parse_program() {
				Ok(program) => program,
				Err(e) => panic!("{}", e),
			};

			let eval = eval(Node::Program(program));
			assert_eq!(eval, *symbol);
		}
	}

	#[test]
	fn integer_expressions() {
		let input = vec!["5", "10"];
		let expected = vec![
			Symbol::Integer(symbol::Integer { value: 5 }),
			Symbol::Integer(symbol::Integer { value: 10 }),
		];

		test(&input, &expected);
	}

	#[test]
	fn boolean_expressions() {
		let input = vec!["false", "true"];
		let expected = vec![
			Symbol::Boolean(symbol::Boolean { value: false }),
			Symbol::Boolean(symbol::Boolean { value: true }),
		];

		test(&input, &expected);
	}

	#[test]
	fn string_expressions() {
		let input = vec!["\"hello\"", "\"world\""];
		let expected = vec![
			Symbol::StringLiteral(symbol::StringLiteral {
				value: "hello".to_string(),
			}),
			Symbol::StringLiteral(symbol::StringLiteral {
				value: "world".to_string(),
			}),
		];

		test(&input, &expected);
	}

	#[test]
	fn prefix_expressions() {
		let input = vec!["!true", "!false", "-5", "-10", "-0"];
		let expected = vec![
			Symbol::Boolean(symbol::Boolean { value: false }),
			Symbol::Boolean(symbol::Boolean { value: true }),
			Symbol::Integer(symbol::Integer { value: -5 }),
			Symbol::Integer(symbol::Integer { value: -10 }),
			Symbol::Integer(symbol::Integer { value: 0 }),
		];

		test(&input, &expected);
	}

	#[test]
	fn infix_expressions() {
		let input = vec![
			"5 + 5 + 5 + 5 - 10",
			"2 * 2 * 2 * 2 * 2",
			"-50 + 100 + -50",
			"5 * 2 + 10",
			"5 + 2 * 10",
			"20 + 2 * -10",
			"50 / 2 * 2 + 10",
			"2 * (5 + 10)",
			"3 * 3 * 3 + 10",
			"3 * (3 * 3) + 10",
			"(5 + 10 * 2 + 15 / 3) * 2 + -10",
			"5 > 4 == 3 < 4;",
			"5 >= 4 == 3 <= 4;",
			"3 + 4 * 5 != 3 * 1 + 4 * 5;",
			"3 < 5 == false;",
			"true || false && true;",
			"true && false || true;",
		];
		let expected = vec![
			Symbol::Integer(symbol::Integer { value: 10 }),
			Symbol::Integer(symbol::Integer { value: 32 }),
			Symbol::Integer(symbol::Integer { value: 0 }),
			Symbol::Integer(symbol::Integer { value: 20 }),
			Symbol::Integer(symbol::Integer { value: 25 }),
			Symbol::Integer(symbol::Integer { value: 0 }),
			Symbol::Integer(symbol::Integer { value: 60 }),
			Symbol::Integer(symbol::Integer { value: 30 }),
			Symbol::Integer(symbol::Integer { value: 37 }),
			Symbol::Integer(symbol::Integer { value: 37 }),
			Symbol::Integer(symbol::Integer { value: 50 }),
			Symbol::Boolean(symbol::Boolean { value: true }),
			Symbol::Boolean(symbol::Boolean { value: true }),
			Symbol::Boolean(symbol::Boolean { value: false }),
			Symbol::Boolean(symbol::Boolean { value: false }),
			Symbol::Boolean(symbol::Boolean { value: true }),
			Symbol::Boolean(symbol::Boolean { value: true }),
		];

		test(&input, &expected);
	}

	#[test]
	fn conditional_expressions() {
		let input = vec![
			"if (true) { 10 };",
			"if (false) { 10 };",
			"if (true) { 10 } else { 20 };",
			"if (false) { 10 } else { 20 };",
		];
		let expected = vec![
			Symbol::Integer(symbol::Integer { value: 10 }),
			Symbol::Null(symbol::Null {}),
			Symbol::Integer(symbol::Integer { value: 10 }),
			Symbol::Integer(symbol::Integer { value: 20 }),
		];

		test(&input, &expected);
	}

	#[test]
	fn return_statements() {
		let input = vec![
			"return 10;",
			"return 10; 9;",
			"return 2 * 5; 9;",
			"9; return 2 * 5; 9;",
			r#"
                if (10 > 1) {
                    if (10 > 1) {
                        return 10;
                    }
                    return 1;
                }
            "#,
		];
		let expected = vec![
			Symbol::Integer(symbol::Integer { value: 10 }),
			Symbol::Integer(symbol::Integer { value: 10 }),
			Symbol::Integer(symbol::Integer { value: 10 }),
			Symbol::Integer(symbol::Integer { value: 10 }),
			Symbol::Integer(symbol::Integer { value: 10 }),
		];

		test(&input, &expected);
	}
}
