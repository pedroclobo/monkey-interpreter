extern crate ast;
extern crate lexer;
extern crate parser;
extern crate symbol;

use ast::{Expression, Node, Program, Statement};
use lexer::token::Token;
use std::rc::Rc;
use symbol::{environment::Environment, Symbol};

mod evaluator_tests;

pub struct Evaluator<'a> {
	pub program: &'a Program,
	pub env: &'a mut Environment,
}

impl<'a> Evaluator<'a> {
	pub fn new(program: &'a Node, env: &'a mut Environment) -> Self {
		let program = match program {
			Node::Program(p) => p,
			_ => panic!("Expected a program."),
		};

		Evaluator {
			program: &program,
			env,
		}
	}

	pub fn eval_program(&mut self) -> Rc<Symbol> {
		self.eval_block(&self.program.statements)
	}

	fn eval(&mut self, node: Node) -> Rc<Symbol> {
		match node {
			Node::Program(_) => unreachable!(),
			Node::Statement(stmt) => match stmt {
				Statement::LetStatement(l) => {
					let val = self.eval(Node::Expression(l.value));
					self.env.set(l.identifier.value, Rc::clone(&val));
					val
				}
				Statement::ReturnStatement(ret) => {
					Symbol::ReturnValue(self.eval(Node::Expression(ret.value))).into()
				}
				Statement::ExpressionStatement(expr) => {
					self.eval(Node::Expression(expr.expression))
				}
			},
			Node::BlockStatement(blk) => self.eval_block(&blk.statements),
			Node::Expression(expr) => match expr {
				Expression::Identifier(id) => match self.env.get(&id.value) {
					Some(v) => v.clone().into(),
					None => panic!("invalid identifier"),
				},
				Expression::IntegerLiteral(i) => Symbol::Integer(i.value).into(),
				Expression::Boolean(b) => Symbol::Boolean(b.value).into(),
				Expression::StringLiteral(s) => Symbol::StringLiteral(s.value).into(),
				Expression::FunctionLiteral(_) => todo!(),
				Expression::UnaryExpression(expr) => {
					let right = self.eval(Node::Expression(*expr.right));
					self.eval_prefix_expression(expr.operator, &right).into()
				}
				Expression::BinaryExpression(expr) => {
					let left = self.eval(Node::Expression(*expr.left));
					let right = self.eval(Node::Expression(*expr.right));
					self.eval_infix_expression(expr.operator, &left, &right)
						.into()
				}
				Expression::IfExpression(expr) => {
					let condition = self.eval(Node::Expression(*expr.condition));
					match *condition {
						Symbol::Boolean(true) => self.eval(Node::BlockStatement(expr.consequence)),
						Symbol::Boolean(false) => match expr.alternative {
							Some(alt) => self.eval(Node::BlockStatement(alt)),
							None => Symbol::Null.into(),
						},
						_ => panic!("invalid condition"),
					}
				}
				Expression::FunctionCall(_) => todo!(),
			},
		}
	}

	fn eval_block(&mut self, statements: &Vec<Statement>) -> Rc<Symbol> {
		let mut result = Rc::new(Symbol::Null);

		for statement in statements {
			result = self.eval(Node::Statement(statement.clone()));

			match *result {
				Symbol::ReturnValue(_) => return result,
				_ => continue,
			}
		}

		result
	}

	fn eval_prefix_expression(&mut self, operator: Token, symbol: &Symbol) -> Rc<Symbol> {
		match operator {
			Token::MINUS => match symbol {
				Symbol::Integer(i) => Symbol::Integer(-i).into(),
				_ => panic!("invalid minus operator expression"),
			},
			Token::NOT => match symbol {
				Symbol::Boolean(b) => Symbol::Boolean(!b).into(),
				_ => panic!("invalid not operator expression"),
			},
			_ => panic!("invalid prefix operator"),
		}
	}

	fn eval_infix_expression(
		&mut self,
		operator: Token,
		left: &Symbol,
		right: &Symbol,
	) -> Rc<Symbol> {
		match operator {
			Token::PLUS => match (left, right) {
				(Symbol::Integer(li), Symbol::Integer(ri)) => Symbol::Integer(li + ri).into(),
				_ => panic!("invalid plus operator expression"),
			},
			Token::MINUS => match (left, right) {
				(Symbol::Integer(li), Symbol::Integer(ri)) => Symbol::Integer(li - ri).into(),
				_ => panic!("invalid minus operator expression"),
			},
			Token::MUL => match (left, right) {
				(Symbol::Integer(li), Symbol::Integer(ri)) => Symbol::Integer(li * ri).into(),
				_ => panic!("invalid mul operator expression"),
			},
			Token::DIV => match (left, right) {
				(Symbol::Integer(li), Symbol::Integer(ri)) => Symbol::Integer(li / ri).into(),
				_ => panic!("invalid div operator expression"),
			},
			Token::LT => match (left, right) {
				(Symbol::Integer(li), Symbol::Integer(ri)) => Symbol::Boolean(li < ri).into(),
				_ => panic!("invalid lt operator expression"),
			},
			Token::GT => match (left, right) {
				(Symbol::Integer(li), Symbol::Integer(ri)) => Symbol::Boolean(li > ri).into(),
				_ => panic!("invalid gt operator expression"),
			},
			Token::LE => match (left, right) {
				(Symbol::Integer(li), Symbol::Integer(ri)) => Symbol::Boolean(li <= ri).into(),
				_ => panic!("invalid le operator expression"),
			},
			Token::GE => match (left, right) {
				(Symbol::Integer(li), Symbol::Integer(ri)) => Symbol::Boolean(li >= ri).into(),
				_ => panic!("invalid ge operator expression"),
			},
			Token::EQ => match (left, right) {
				(Symbol::Integer(li), Symbol::Integer(ri)) => Symbol::Boolean(li == ri).into(),
				(Symbol::Boolean(lb), Symbol::Boolean(rb)) => Symbol::Boolean(lb == rb).into(),
				_ => panic!("invalid eq operator expression"),
			},
			Token::NE => match (left, right) {
				(Symbol::Integer(li), Symbol::Integer(ri)) => Symbol::Boolean(li != ri).into(),
				(Symbol::Boolean(lb), Symbol::Boolean(rb)) => Symbol::Boolean(lb != rb).into(),
				_ => panic!("invalid ne operator expression"),
			},
			Token::AND => match (left, right) {
				(Symbol::Boolean(lb), Symbol::Boolean(rb)) => Symbol::Boolean(*lb && *rb).into(),
				_ => panic!("invalid and operator expression"),
			},
			Token::OR => match (left, right) {
				(Symbol::Boolean(lb), Symbol::Boolean(rb)) => Symbol::Boolean(*lb || *rb).into(),
				_ => panic!("invalid and operator expression"),
			},
			_ => panic!("invalid infix operator"),
		}
	}
}
