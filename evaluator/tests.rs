#[cfg(test)]
mod tests {
	use ast::Node;
	use eval;
	use lexer::Lexer;
	use parser::Parser;
	use symbol::{environment::Environment, Symbol};
	use Rc;
	use RefCell;

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

			let env = Rc::new(RefCell::new(Environment::new()));
			let program = Node::Program(program);

			assert_eq!(*eval(program, &env), *symbol);
		}
	}

	#[test]
	fn integer_expressions() {
		let input = vec!["5", "10"];
		let expected = vec![Symbol::Integer(5), Symbol::Integer(10)];

		test(&input, &expected);
	}

	#[test]
	fn boolean_expressions() {
		let input = vec!["false", "true"];
		let expected = vec![Symbol::Boolean(false), Symbol::Boolean(true)];

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
			Symbol::ReturnValue(Rc::from(Symbol::Integer(10))),
			Symbol::ReturnValue(Rc::from(Symbol::Integer(10))),
			Symbol::ReturnValue(Rc::from(Symbol::Integer(10))),
			Symbol::ReturnValue(Rc::from(Symbol::Integer(10))),
			Symbol::ReturnValue(Rc::from(Symbol::Integer(10))),
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

	#[test]
	fn function_calls() {
		let input = vec![
			"let identity = fn(x) { x; }; identity(5);",
			"let identity = fn(x) { return x; }; identity(5);",
			"let double = fn(x) { x * 2; }; double(5);",
			"let add = fn(x, y) { x + y; }; add(5, 5);",
			"let add = fn(x, y) { x + y; }; add(5 + 5, add(5, 5));",
			"fn(x) { x; }(5)",
		];
		let expected = vec![
			Symbol::Integer(5),
			Symbol::Integer(5),
			Symbol::Integer(10),
			Symbol::Integer(10),
			Symbol::Integer(20),
			Symbol::Integer(5),
		];

		test(&input, &expected);
	}

	#[test]
	fn recursive_functions() {
		let input = vec![
			"let fib = fn(x) {
				if (x <= 1) {
					x
				} else {
					fib(x - 1) + fib(x - 2);
				}
			};
            fib(10);",
			"let factorial = fn(x) {
				if (x == 0) {
					1
				} else {
					x * factorial(x - 1);
				}
			};
            factorial(5);",
		];
		let expected = vec![Symbol::Integer(55), Symbol::Integer(120)];

		test(&input, &expected);
	}
}
