extern crate ast;
extern crate lexer;
extern crate parser;
extern crate symbol;

use ast::{Expression, Node, Statement};
use lexer::token::Token;
use symbol::Symbol;

mod evaluator_tests;

pub fn eval(node: Node) -> Symbol {
	match node {
		Node::Program(p) => eval_program(p.statements),
		Node::Statement(stmt) => match stmt {
			Statement::LetStatement(_) => todo!(),
			Statement::ReturnStatement(ret) => Symbol::ReturnValue(symbol::ReturnValue {
				value: Box::new(eval(Node::Expression(ret.value))),
			}),
			Statement::ExpressionStatement(expr) => eval(Node::Expression(expr.expression)),
		},
		Node::BlockStatement(blk) => eval_block(blk.statements),
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

fn eval_program(statements: Vec<Statement>) -> Symbol {
	let mut result = Symbol::Null(symbol::Null {});

	for statement in statements {
		result = eval(Node::Statement(statement));

		match result {
			Symbol::ReturnValue(ret) => return *ret.value,
			_ => continue,
		}
	}

	result
}

fn eval_block(statements: Vec<Statement>) -> Symbol {
	let mut result = Symbol::Null(symbol::Null {});

	for statement in statements {
		result = eval(Node::Statement(statement));

		match result {
			Symbol::ReturnValue(_) => return result,
			_ => continue,
		}
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
