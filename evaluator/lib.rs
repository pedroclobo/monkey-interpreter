extern crate ast;
extern crate lexer;
extern crate parser;
extern crate symbol;

use ast::{Expression, Node, Statement};
use lexer::token::Token;
use symbol::Symbol;

pub fn eval(node: Node) -> Symbol {
	match node {
		Node::Program(p) => eval_statements(p.statements),
		Node::Statement(stmt) => match stmt {
			Statement::LetStatement(_) => todo!(),
			Statement::ReturnStatement(_) => todo!(),
			Statement::ExpressionStatement(expr) => eval(Node::Expression(expr.expression)),
		},
		Node::BlockStatement(blk) => eval_statements(blk.statements),
		Node::Expression(expr) => match expr {
			Expression::Identifier(_) => todo!(),
			Expression::IntegerLiteral(i) => Symbol::Integer(symbol::Integer { value: i.value }),
			Expression::Boolean(b) => Symbol::Boolean(symbol::Boolean { value: b.value }),
			Expression::StringLiteral(s) => {
				Symbol::StringLiteral(symbol::StringLiteral { value: s.value })
			}
			Expression::FunctionLiteral(_) => todo!(),
			Expression::UnaryExpression(expr) => {
				let right = eval(Node::Expression(*expr.right));
				eval_prefix_expression(expr.operator, right)
			}
			Expression::BinaryExpression(expr) => {
				let left = eval(Node::Expression(*expr.left));
				let right = eval(Node::Expression(*expr.right));
				eval_infix_expression(expr.operator, left, right)
			}
			Expression::IfExpression(expr) => {
				let condition = eval(Node::Expression(*expr.condition));
				match condition {
					Symbol::Boolean(symbol::Boolean { value: true }) => {
						eval(Node::BlockStatement(expr.consequence))
					}
					Symbol::Boolean(symbol::Boolean { value: false }) => match expr.alternative {
						Some(alt) => eval(Node::BlockStatement(alt)),
						None => Symbol::Null(symbol::Null {}),
					},
					_ => panic!("invalid condition"),
				}
			}
			Expression::FunctionCall(_) => todo!(),
		},
	}
}

fn eval_statements(statements: Vec<Statement>) -> Symbol {
	let mut result = Symbol::Integer(symbol::Integer { value: 0 });

	for statement in statements {
		result = eval(Node::Statement(statement));
	}

	result
}

fn eval_prefix_expression(operator: Token, symbol: Symbol) -> Symbol {
	match operator {
		Token::MINUS => match symbol {
			Symbol::Integer(i) => Symbol::Integer(symbol::Integer { value: -i.value }),
			_ => panic!("invalid minus operator expression"),
		},
		Token::NOT => match symbol {
			Symbol::Boolean(b) => Symbol::Boolean(symbol::Boolean { value: !b.value }),
			_ => panic!("invalid not operator expression"),
		},
		_ => panic!("invalid prefix operator"),
	}
}

fn eval_infix_expression(operator: Token, left: Symbol, right: Symbol) -> Symbol {
	match operator {
		Token::PLUS => match (left, right) {
			(Symbol::Integer(li), Symbol::Integer(ri)) => Symbol::Integer(symbol::Integer {
				value: li.value + ri.value,
			}),
			_ => panic!("invalid plus operator expression"),
		},
		Token::MINUS => match (left, right) {
			(Symbol::Integer(li), Symbol::Integer(ri)) => Symbol::Integer(symbol::Integer {
				value: li.value - ri.value,
			}),
			_ => panic!("invalid minus operator expression"),
		},
		Token::MUL => match (left, right) {
			(Symbol::Integer(li), Symbol::Integer(ri)) => Symbol::Integer(symbol::Integer {
				value: li.value * ri.value,
			}),
			_ => panic!("invalid mul operator expression"),
		},
		Token::DIV => match (left, right) {
			(Symbol::Integer(li), Symbol::Integer(ri)) => Symbol::Integer(symbol::Integer {
				value: li.value / ri.value,
			}),
			_ => panic!("invalid div operator expression"),
		},
		Token::LT => match (left, right) {
			(Symbol::Integer(li), Symbol::Integer(ri)) => Symbol::Boolean(symbol::Boolean {
				value: li.value < ri.value,
			}),
			_ => panic!("invalid lt operator expression"),
		},
		Token::GT => match (left, right) {
			(Symbol::Integer(li), Symbol::Integer(ri)) => Symbol::Boolean(symbol::Boolean {
				value: li.value > ri.value,
			}),
			_ => panic!("invalid gt operator expression"),
		},
		Token::LE => match (left, right) {
			(Symbol::Integer(li), Symbol::Integer(ri)) => Symbol::Boolean(symbol::Boolean {
				value: li.value <= ri.value,
			}),
			_ => panic!("invalid le operator expression"),
		},
		Token::GE => match (left, right) {
			(Symbol::Integer(li), Symbol::Integer(ri)) => Symbol::Boolean(symbol::Boolean {
				value: li.value >= ri.value,
			}),
			_ => panic!("invalid ge operator expression"),
		},
		Token::EQ => match (left, right) {
			(Symbol::Integer(li), Symbol::Integer(ri)) => Symbol::Boolean(symbol::Boolean {
				value: li.value == ri.value,
			}),
			(Symbol::Boolean(lb), Symbol::Boolean(rb)) => Symbol::Boolean(symbol::Boolean {
				value: lb.value == rb.value,
			}),
			_ => panic!("invalid eq operator expression"),
		},
		Token::NE => match (left, right) {
			(Symbol::Integer(li), Symbol::Integer(ri)) => Symbol::Boolean(symbol::Boolean {
				value: li.value != ri.value,
			}),
			(Symbol::Boolean(lb), Symbol::Boolean(rb)) => Symbol::Boolean(symbol::Boolean {
				value: lb.value != rb.value,
			}),
			_ => panic!("invalid ne operator expression"),
		},
		Token::AND => match (left, right) {
			(Symbol::Boolean(lb), Symbol::Boolean(rb)) => Symbol::Boolean(symbol::Boolean {
				value: lb.value && rb.value,
			}),
			_ => panic!("invalid and operator expression"),
		},
		Token::OR => match (left, right) {
			(Symbol::Boolean(lb), Symbol::Boolean(rb)) => Symbol::Boolean(symbol::Boolean {
				value: lb.value || rb.value,
			}),
			_ => panic!("invalid and operator expression"),
		},
		_ => panic!("invalid infix operator"),
	}
}

#[cfg(test)]
mod tests {
	use eval;
	use lexer::Lexer;
	use parser::Parser;
	use Node;
	use Symbol;

	fn test(input: &Vec<&str>, expected: &Vec<Symbol>) {
		for (i, symbol) in expected.iter().enumerate() {
			let mut lexer = Lexer::new(input[i]);
			let mut parser = Parser::new(&mut lexer);
			let program = parser.parse_program();
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
}
