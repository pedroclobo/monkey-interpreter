#[cfg(test)]
mod tests {
	use Lexer;
	use Parser;

	fn test(input: &str, expected: &Vec<&str>) {
		let mut lexer = Lexer::new(input, "test");

		let mut parser = match Parser::new(&mut lexer) {
			Ok(parser) => parser,
			Err(e) => panic!("{}", e),
		};

		let program = match parser.parse_program() {
			Ok(program) => program,
			Err(e) => panic!("{}", e),
		};

		assert_eq!(program.statements.len(), expected.len());

		for (i, stmt) in program.statements.iter().enumerate() {
			assert_eq!(stmt.to_string(), expected[i]);
		}
	}

	#[test]
	fn let_statements() {
		let input = r#"
			let a = 10;
			let b = false;
			let c = "foo";
			let d = a;
		"#;
		let expected = vec![
			"tLET(a, 10)",
			"tLET(b, false)",
			"tLET(c, foo)",
			"tLET(d, a)",
		];

		test(input, &expected);
	}

	#[test]
	fn return_statements() {
		let input = r#"
			return 1;
			return true;
			return "bar";
			return a;
		"#;
		let expected = vec!["tRETURN(1)", "tRETURN(true)", "tRETURN(bar)", "tRETURN(a)"];

		test(input, &expected);
	}

	#[test]
	fn identifier_expression() {
		let input = r#"
			a;
		"#;
		let expected = vec!["a"];

		test(input, &expected);
	}

	#[test]
	fn integer_expression() {
		let input = r#"
			1;
		"#;
		let expected = vec!["1"];

		test(input, &expected);
	}

	#[test]
	fn boolean_expression() {
		let input = r#"
			true;
			false;
		"#;
		let expected = vec!["true", "false"];

		test(input, &expected);
	}

	#[test]
	fn string_expression() {
		let input = r#"
			hello;
			world;
		"#;
		let expected = vec!["hello", "world"];

		test(input, &expected);
	}

	#[test]
	fn prefix_expression() {
		let input = r#"
			!10;
			-15;
		"#;
		let expected = vec!["tNOT(10)", "tMINUS(15)"];

		test(input, &expected);
	}

	#[test]
	fn infix_expression() {
		let input = r#"
			-a * b;
			a * -b;
			!-a;

			a + b + c;
			a + b - c;
			a * b * c;
			a * b / c;
			a + b / c;
			a + b * c + d / e - f;

			5 > 4 == 3 < 4;
			5 >= 4 == 3 <= 4;
			3 + 4 * 5 == 3 * 1 + 4 * 5;

			3 < 5 == true;

			true || false && true;
			true && false || true;

			a + add(b * c) + d

			!(-(2 + 3 * 4) == 5 || (6 >= 7) && foo(8) < -bar(9 - 1))
		"#;
		let expected = vec![
			"(tMINUS(a) tMUL b)",
			"(a tMUL tMINUS(b))",
			"tNOT(tMINUS(a))",

			"((a tPLUS b) tPLUS c)",
			"((a tPLUS b) tMINUS c)",
			"((a tMUL b) tMUL c)",
			"((a tMUL b) tDIV c)",
			"(a tPLUS (b tDIV c))",
			"(((a tPLUS (b tMUL c)) tPLUS (d tDIV e)) tMINUS f)",

			"((5 tGT 4) tEQ (3 tLT 4))",
			"((5 tGE 4) tEQ (3 tLE 4))",
			"((3 tPLUS (4 tMUL 5)) tEQ ((3 tMUL 1) tPLUS (4 tMUL 5)))",
			"((3 tLT 5) tEQ true)",

			"(true tOR (false tAND true))",
			"((true tAND false) tOR true)",

			"((a tPLUS tCALL(add, ((b tMUL c)))) tPLUS d)",

			"tNOT(((tMINUS((2 tPLUS (3 tMUL 4))) tEQ 5) tOR ((6 tGE 7) tAND (tCALL(foo, (8)) tLT tMINUS(tCALL(bar, ((9 tMINUS 1))))))))",
		];

		test(input, &expected);
	}

	#[test]
	fn grouped_expressions() {
		let input = r#"
			1 + (2 + 3) + 4;
			(5 + 5) * 2;
			-(5 + 5);
			!(true == true);
		"#;

		let expected = vec![
			"((1 tPLUS (2 tPLUS 3)) tPLUS 4)",
			"((5 tPLUS 5) tMUL 2)",
			"tMINUS((5 tPLUS 5))",
			"tNOT((true tEQ true))",
		];

		test(input, &expected);
	}

	#[test]
	fn if_expressions() {
		let input = r#"
			if (x < y) { x };
			if (x < y) { x } else { y };
		"#;

		let expected = vec!["tIF (x tLT y) {x}", "tIF (x tLT y) {x} tELSE {y}"];

		test(input, &expected);
	}

	#[test]
	fn function_literals() {
		let input = r#"
			fn(x, y) { x + y; }
			fn() {}
			fn(x, y, z) {}
		"#;

		let expected = vec![
			"tFUNCTION(x, y) {(x tPLUS y)}",
			"tFUNCTION() {}",
			"tFUNCTION(x, y, z) {}",
		];

		test(input, &expected);
	}

	#[test]
	fn function_calls() {
		let input = r#"
			add(1, 2 * 3, 4 + 5);
			add();
		"#;

		let expected = vec!["tCALL(add, (1, (2 tMUL 3), (4 tPLUS 5)))", "tCALL(add, ())"];

		test(input, &expected);
	}
}
