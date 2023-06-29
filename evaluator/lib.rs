extern crate ast;
extern crate lexer;
extern crate parser;
extern crate symbol;

use ast::{Expression, Node, Program, Statement};
use lexer::token::Token;
use symbol::{environment::Environment, Symbol};

mod evaluator_tests;

pub struct Evaluator<'a> {
	pub program: Program,
	pub env: &'a mut Environment,
}

impl<'a> Evaluator<'a> {
	pub fn new(program: Node, env: &'a mut Environment) -> Self {
		let program = match program {
			Node::Program(p) => p,
			_ => panic!("Expected a program."),
		};

		Evaluator { program, env }
	}

	pub fn eval_program(&mut self) -> Symbol {
		let mut result = Symbol::Null(symbol::Null {});

		let statements = &self.program.statements;

		for statement in statements.clone() {
			result = self.eval(Node::Statement(statement));

			match result {
				Symbol::ReturnValue(ret) => return *ret.value,
				_ => continue,
			}
		}

		result
	}

	fn eval(&mut self, node: Node) -> Symbol {
		match node {
			Node::Program(_) => unreachable!(),
			Node::Statement(stmt) => match stmt {
				Statement::LetStatement(l) => {
					let val = self.eval(Node::Expression(l.value));
					let copy = val.clone();
					self.env.set(l.identifier.value, val);
					copy
				}
				Statement::ReturnStatement(ret) => Symbol::ReturnValue(symbol::ReturnValue {
					value: Box::new(self.eval(Node::Expression(ret.value))),
				}),
				Statement::ExpressionStatement(expr) => {
					self.eval(Node::Expression(expr.expression))
				}
			},
			Node::BlockStatement(blk) => self.eval_block(blk.statements),
			Node::Expression(expr) => match expr {
				Expression::Identifier(id) => match self.env.get(&id.value) {
					Some(v) => v.clone(),
					None => panic!("invalid identifier"),
				},
				Expression::IntegerLiteral(i) => {
					Symbol::Integer(symbol::Integer { value: i.value })
				}
				Expression::Boolean(b) => Symbol::Boolean(symbol::Boolean { value: b.value }),
				Expression::StringLiteral(s) => {
					Symbol::StringLiteral(symbol::StringLiteral { value: s.value })
				}
				Expression::FunctionLiteral(_) => todo!(),
				Expression::UnaryExpression(expr) => {
					let right = self.eval(Node::Expression(*expr.right));
					self.eval_prefix_expression(expr.operator, right)
				}
				Expression::BinaryExpression(expr) => {
					let left = self.eval(Node::Expression(*expr.left));
					let right = self.eval(Node::Expression(*expr.right));
					self.eval_infix_expression(expr.operator, left, right)
				}
				Expression::IfExpression(expr) => {
					let condition = self.eval(Node::Expression(*expr.condition));
					match condition {
						Symbol::Boolean(symbol::Boolean { value: true }) => {
							self.eval(Node::BlockStatement(expr.consequence))
						}
						Symbol::Boolean(symbol::Boolean { value: false }) => match expr.alternative
						{
							Some(alt) => self.eval(Node::BlockStatement(alt)),
							None => Symbol::Null(symbol::Null {}),
						},
						_ => panic!("invalid condition"),
					}
				}
				Expression::FunctionCall(_) => todo!(),
			},
		}
	}

	fn eval_block(&mut self, statements: Vec<Statement>) -> Symbol {
		let mut result = Symbol::Null(symbol::Null {});

		for statement in statements {
			result = self.eval(Node::Statement(statement));

			match result {
				Symbol::ReturnValue(_) => return result,
				_ => continue,
			}
		}

		result
	}

	fn eval_prefix_expression(&mut self, operator: Token, symbol: Symbol) -> Symbol {
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

	fn eval_infix_expression(&mut self, operator: Token, left: Symbol, right: Symbol) -> Symbol {
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
}
