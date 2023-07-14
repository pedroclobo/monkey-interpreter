#[cfg(test)]
mod tests {
	use lexer::Lexer;
	use parser::Parser;
	use Environment;
	use Evaluator;
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

			let mut env = Environment::new();
			let program = ast::Node::Program(program);
			let mut evaluator = Evaluator::new(&program, &mut env);

			assert_eq!(*evaluator.eval_program(), *symbol);
		}
	}

	#[test]
	fn integer_expressions() {
		let input = vec!["5", "10"];
		let expected = vec![
			Symbol::Integer(5),
			Symbol::Integer(10),
		];

		test(&input, &expected);
	}

	#[test]
	fn boolean_expressions() {
		let input = vec!["false", "true"];
		let expected = vec![
			Symbol::Boolean(false),
			Symbol::Boolean(true),
		];

		test(&input, &expected);
	}

	#[test]
	fn string_expressions() {
		let input = vec!["\"hello\"", "\"world\""];
		let expected = vec![
			Symbol::StringLiteral("hello".to_string()),
			Symbol::StringLiteral("world".to_string()),
		];

		test(&input, &expected);
	}

	#[test]
	fn prefix_expressions() {
		let input = vec!["!true", "!false", "-5", "-10", "-0"];
		let expected = vec![
			Symbol::Boolean(false),
			Symbol::Boolean(true),
			Symbol::Integer(-5),
			Symbol::Integer(-10),
			Symbol::Integer(0),
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
			Symbol::Integer(10),
			Symbol::Integer(32),
			Symbol::Integer(0),
			Symbol::Integer(20),
			Symbol::Integer(25),
			Symbol::Integer(0),
			Symbol::Integer(60),
			Symbol::Integer(30),
			Symbol::Integer(37),
			Symbol::Integer(37),
			Symbol::Integer(50),
			Symbol::Boolean(true),
			Symbol::Boolean(true),
			Symbol::Boolean(false),
			Symbol::Boolean(false),
			Symbol::Boolean(true),
			Symbol::Boolean(true),
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
			Symbol::Integer(10),
			Symbol::Null,
			Symbol::Integer(10),
			Symbol::Integer(20),
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
			Symbol::ReturnValue(Symbol::Integer(10).into()),
			Symbol::ReturnValue(Symbol::Integer(10).into()),
			Symbol::ReturnValue(Symbol::Integer(10).into()),
			Symbol::ReturnValue(Symbol::Integer(10).into()),
			Symbol::ReturnValue(Symbol::Integer(10).into()),
		];

		test(&input, &expected);
	}

	#[test]
	fn let_statements() {
		let input = vec![
			"let a = 5; a;",
			"let a = 5 * 5; a;",
			"let a = 5; let b = a; b;",
			"let a = 5; let b = a; let c = a + b + 5; c;",
		];
		let expected = vec![
			Symbol::Integer(5),
			Symbol::Integer(25),
			Symbol::Integer(5),
			Symbol::Integer(15),
		];

		test(&input, &expected);
	}
}
