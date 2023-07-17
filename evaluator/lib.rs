extern crate ast;
extern crate lexer;
extern crate parser;
extern crate symbol;

use symbol::{environment::Environment, Symbol};

use std::rc::Rc;

mod evaluator_tests;

pub fn eval(node: ast::Node, env: &mut Environment) -> Rc<Symbol> {
	match node {
		ast::Node::Program(ast::Program { statements }) => eval_block(&statements, env),
		ast::Node::Statement(statement) => match statement {
			ast::Statement::LetStatement(ast::LetStatement { identifier, value }) => {
				let val = eval(ast::Node::Expression(value), env);
				env.set(identifier.value, Rc::clone(&val));
				val
			}
			ast::Statement::ReturnStatement(ast::ReturnStatement { value }) => {
				Symbol::ReturnValue(eval(ast::Node::Expression(value), env)).into()
			}
			ast::Statement::ExpressionStatement(ast::ExpressionStatement { expression }) => {
				eval(ast::Node::Expression(expression), env)
			}
		},
		ast::Node::BlockStatement(ast::BlockStatement { statements }) => {
			eval_block(&statements, env)
		}
		ast::Node::Expression(expression) => match expression {
			ast::Expression::Identifier(ast::Identifier { value }) => match env.get(&value) {
				Some(v) => v.clone().into(),
				None => panic!("invalid identifier"),
			},
			ast::Expression::IntegerLiteral(ast::IntegerLiteral { value }) => {
				Symbol::Integer(value).into()
			}
			ast::Expression::Boolean(ast::Boolean { value }) => Symbol::Boolean(value).into(),
			ast::Expression::StringLiteral(ast::StringLiteral { value }) => {
				Symbol::StringLiteral(value).into()
			}
			ast::Expression::FunctionLiteral(_) => todo!(),
			ast::Expression::UnaryExpression(ast::UnaryExpression { operator, right }) => {
				let right = eval(ast::Node::Expression(*right), env);
				eval_prefix_expression(operator, &right).into()
			}
			ast::Expression::BinaryExpression(ast::BinaryExpression {
				left,
				operator,
				right,
			}) => {
				let left = eval(ast::Node::Expression(*left), env);
				let right = eval(ast::Node::Expression(*right), env);
				eval_infix_expression(operator, &left, &right).into()
			}
			ast::Expression::IfExpression(ast::IfExpression {
				condition,
				consequence,
				alternative,
			}) => {
				let condition = eval(ast::Node::Expression(*condition), env);
				match *condition {
					Symbol::Boolean(true) => eval(ast::Node::BlockStatement(consequence), env),
					Symbol::Boolean(false) => match alternative {
						Some(alt) => eval(ast::Node::BlockStatement(alt), env),
						None => Symbol::Null.into(),
					},
					_ => panic!("invalid condition"),
				}
			}
			ast::Expression::FunctionCall(_) => todo!(),
		},
	}
}

fn eval_block(statements: &Vec<ast::Statement>, env: &mut Environment) -> Rc<Symbol> {
	let mut result = Rc::new(Symbol::Null);

	for statement in statements {
		result = eval(ast::Node::Statement(statement.clone()), env);

		match *result {
			Symbol::ReturnValue(_) => return result,
			_ => continue,
		}
	}

	result
}

fn eval_prefix_expression(operator: lexer::token::Token, symbol: &Symbol) -> Rc<Symbol> {
	match operator {
		lexer::token::Token::MINUS => match symbol {
			Symbol::Integer(i) => Symbol::Integer(-i).into(),
			_ => panic!("invalid minus operator expression"),
		},
		lexer::token::Token::NOT => match symbol {
			Symbol::Boolean(b) => Symbol::Boolean(!b).into(),
			_ => panic!("invalid not operator expression"),
		},
		_ => panic!("invalid prefix operator"),
	}
}

fn eval_infix_expression(
	operator: lexer::token::Token,
	left: &Symbol,
	right: &Symbol,
) -> Rc<Symbol> {
	match operator {
		lexer::token::Token::PLUS => match (left, right) {
			(Symbol::Integer(li), Symbol::Integer(ri)) => Symbol::Integer(li + ri).into(),
			_ => panic!("invalid plus operator expression"),
		},
		lexer::token::Token::MINUS => match (left, right) {
			(Symbol::Integer(li), Symbol::Integer(ri)) => Symbol::Integer(li - ri).into(),
			_ => panic!("invalid minus operator expression"),
		},
		lexer::token::Token::MUL => match (left, right) {
			(Symbol::Integer(li), Symbol::Integer(ri)) => Symbol::Integer(li * ri).into(),
			_ => panic!("invalid mul operator expression"),
		},
		lexer::token::Token::DIV => match (left, right) {
			(Symbol::Integer(li), Symbol::Integer(ri)) => Symbol::Integer(li / ri).into(),
			_ => panic!("invalid div operator expression"),
		},
		lexer::token::Token::LT => match (left, right) {
			(Symbol::Integer(li), Symbol::Integer(ri)) => Symbol::Boolean(li < ri).into(),
			_ => panic!("invalid lt operator expression"),
		},
		lexer::token::Token::GT => match (left, right) {
			(Symbol::Integer(li), Symbol::Integer(ri)) => Symbol::Boolean(li > ri).into(),
			_ => panic!("invalid gt operator expression"),
		},
		lexer::token::Token::LE => match (left, right) {
			(Symbol::Integer(li), Symbol::Integer(ri)) => Symbol::Boolean(li <= ri).into(),
			_ => panic!("invalid le operator expression"),
		},
		lexer::token::Token::GE => match (left, right) {
			(Symbol::Integer(li), Symbol::Integer(ri)) => Symbol::Boolean(li >= ri).into(),
			_ => panic!("invalid ge operator expression"),
		},
		lexer::token::Token::EQ => match (left, right) {
			(Symbol::Integer(li), Symbol::Integer(ri)) => Symbol::Boolean(li == ri).into(),
			(Symbol::Boolean(lb), Symbol::Boolean(rb)) => Symbol::Boolean(lb == rb).into(),
			_ => panic!("invalid eq operator expression"),
		},
		lexer::token::Token::NE => match (left, right) {
			(Symbol::Integer(li), Symbol::Integer(ri)) => Symbol::Boolean(li != ri).into(),
			(Symbol::Boolean(lb), Symbol::Boolean(rb)) => Symbol::Boolean(lb != rb).into(),
			_ => panic!("invalid ne operator expression"),
		},
		lexer::token::Token::AND => match (left, right) {
			(Symbol::Boolean(lb), Symbol::Boolean(rb)) => Symbol::Boolean(*lb && *rb).into(),
			_ => panic!("invalid and operator expression"),
		},
		lexer::token::Token::OR => match (left, right) {
			(Symbol::Boolean(lb), Symbol::Boolean(rb)) => Symbol::Boolean(*lb || *rb).into(),
			_ => panic!("invalid and operator expression"),
		},
		_ => panic!("invalid infix operator"),
	}
}
