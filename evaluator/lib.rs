extern crate ast;
extern crate lexer;
extern crate parser;
extern crate symbol;

use ast::{Expression, Node, Program, Statement};
use lexer::token::Token;
use symbol::environment::Environment;
use symbol::Symbol;

use std::cell::RefCell;
use std::rc::Rc;

mod evaluator_tests;

type Env = Rc<RefCell<Environment>>;

pub fn eval(node: Node, env: &Env) -> Rc<Symbol> {
	match node {
		Node::Program(Program { statements }) => eval_block(&statements, &Rc::clone(&env)),
		Node::Statement(statement) => eval_statement(statement, &Rc::clone(&env)),
		Node::Expression(expression) => eval_expression(expression, &Rc::clone(env)),
	}
}

fn eval_statement(statement: Statement, env: &Env) -> Rc<Symbol> {
	match statement {
		Statement::LetStatement(ast::LetStatement {
			identifier,
			expression,
		}) => {
			let expression = eval(Node::Expression(expression), &Rc::clone(&env));
			env.borrow_mut()
				.set(identifier.value, Rc::clone(&expression));
			expression
		}
		Statement::ReturnStatement(ast::ReturnStatement { value }) => {
			Rc::from(Symbol::ReturnValue(eval(Node::Expression(value), env)))
		}
		Statement::ExpressionStatement(ast::ExpressionStatement { expression }) => {
			eval(Node::Expression(expression), env)
		}
		Statement::BlockStatement(ast::BlockStatement { statements }) => {
			eval_block(&statements, &Rc::clone(&env))
		}
	}
}

fn eval_expression(expression: Expression, env: &Env) -> Rc<Symbol> {
	match expression {
		Expression::Identifier(ast::Identifier { value }) => match env.borrow().get(&value) {
			Some(value) => Rc::clone(&value),
			None => panic!("invalid identifier: {}", value),
		},
		Expression::IntegerLiteral(ast::IntegerLiteral { value }) => {
			Rc::from(Symbol::Integer(value))
		}
		Expression::Boolean(ast::Boolean { value }) => Rc::from(Symbol::Boolean(value)),
		Expression::StringLiteral(ast::StringLiteral { value }) => {
			Rc::from(Symbol::StringLiteral(value))
		}
		Expression::FunctionLiteral(ast::FunctionLiteral { parameters, body }) => {
			Rc::from(Symbol::FunctionLiteral { parameters, body })
		}
		Expression::UnaryExpression(ast::UnaryExpression { operator, right }) => {
			let right = eval(Node::Expression(*right), env);
			Rc::clone(&eval_prefix_expression(operator, &right))
		}
		Expression::BinaryExpression(ast::BinaryExpression {
			left,
			operator,
			right,
		}) => {
			let left = eval(Node::Expression(*left), &Rc::clone(&env));
			let right = eval(Node::Expression(*right), &Rc::clone(&env));
			Rc::clone(&eval_infix_expression(operator, &left, &right))
		}
		Expression::IfExpression(ast::IfExpression {
			condition,
			consequence,
			alternative,
		}) => {
			let condition = eval(Node::Expression(*condition), &Rc::clone(&env));
			match *condition {
				Symbol::Boolean(true) => eval(
					Node::Statement(Statement::BlockStatement(consequence)),
					&Rc::clone(&env),
				),
				Symbol::Boolean(false) => match alternative {
					Some(alternative) => {
						eval(Node::Statement(Statement::BlockStatement(alternative)), env)
					}
					None => Rc::from(Symbol::Null),
				},
				_ => panic!("invalid condition"),
			}
		}
		Expression::FunctionCall(ast::FunctionCall {
			function,
			arguments,
		}) => {
			let function = eval(Node::Expression(*function), &Rc::clone(&env));
			let arguments = eval_expressions(&arguments, &Rc::clone(&env));

			apply_function(&function, &arguments, &Rc::clone(&env))
		}
	}
}

fn eval_block(statements: &Vec<Statement>, env: &Env) -> Rc<Symbol> {
	let mut result = Rc::new(Symbol::Null);

	for statement in statements {
		result = eval(Node::Statement(statement.clone()), env);

		match *result {
			Symbol::ReturnValue(_) => return result,
			_ => continue,
		}
	}

	result
}

fn eval_expressions(expressions: &Vec<Expression>, env: &Env) -> Vec<Rc<Symbol>> {
	expressions
		.iter()
		.map(|expression| eval(Node::Expression(expression.clone()), env))
		.collect()
}

fn eval_prefix_expression(operator: Token, symbol: &Symbol) -> Rc<Symbol> {
	match operator {
		Token::MINUS => match symbol {
			Symbol::Integer(value) => Rc::from(Symbol::Integer(-value)),
			_ => panic!("invalid minus operator expression"),
		},
		Token::NOT => match symbol {
			Symbol::Boolean(value) => Rc::from(Symbol::Boolean(!value)),
			_ => panic!("invalid not operator expression"),
		},
		_ => panic!("invalid prefix operator"),
	}
}

fn eval_infix_expression(operator: Token, left: &Symbol, right: &Symbol) -> Rc<Symbol> {
	match operator {
		Token::PLUS => match (left, right) {
			(Symbol::Integer(li), Symbol::Integer(ri)) => Rc::from(Symbol::Integer(li + ri)),
			_ => panic!("invalid plus operator expression"),
		},
		Token::MINUS => match (left, right) {
			(Symbol::Integer(li), Symbol::Integer(ri)) => Rc::from(Symbol::Integer(li - ri)),
			_ => panic!("invalid minus operator expression"),
		},
		Token::MUL => match (left, right) {
			(Symbol::Integer(li), Symbol::Integer(ri)) => Rc::from(Symbol::Integer(li * ri)),
			_ => panic!("invalid mul operator expression"),
		},
		Token::DIV => match (left, right) {
			(Symbol::Integer(li), Symbol::Integer(ri)) => Rc::from(Symbol::Integer(li / ri)),
			_ => panic!("invalid div operator expression"),
		},
		Token::LT => match (left, right) {
			(Symbol::Integer(li), Symbol::Integer(ri)) => Rc::from(Symbol::Boolean(li < ri)),
			_ => panic!("invalid lt operator expression"),
		},
		Token::GT => match (left, right) {
			(Symbol::Integer(li), Symbol::Integer(ri)) => Rc::from(Symbol::Boolean(li > ri)),
			_ => panic!("invalid gt operator expression"),
		},
		Token::LE => match (left, right) {
			(Symbol::Integer(li), Symbol::Integer(ri)) => Rc::from(Symbol::Boolean(li <= ri)),
			_ => panic!("invalid le operator expression"),
		},
		Token::GE => match (left, right) {
			(Symbol::Integer(li), Symbol::Integer(ri)) => Rc::from(Symbol::Boolean(li >= ri)),
			_ => panic!("invalid ge operator expression"),
		},
		Token::EQ => match (left, right) {
			(Symbol::Integer(li), Symbol::Integer(ri)) => Rc::from(Symbol::Boolean(li == ri)),
			(Symbol::Boolean(lb), Symbol::Boolean(rb)) => Rc::from(Symbol::Boolean(lb == rb)),

			_ => panic!("invalid eq operator expression"),
		},
		Token::NE => match (left, right) {
			(Symbol::Integer(li), Symbol::Integer(ri)) => Rc::from(Symbol::Boolean(li != ri)),
			(Symbol::Boolean(lb), Symbol::Boolean(rb)) => Rc::from(Symbol::Boolean(lb != rb)),

			_ => panic!("invalid ne operator expression"),
		},
		Token::AND => match (left, right) {
			(Symbol::Boolean(lb), Symbol::Boolean(rb)) => Rc::from(Symbol::Boolean(*lb && *rb)),
			_ => panic!("invalid and operator expression"),
		},
		Token::OR => match (left, right) {
			(Symbol::Boolean(lb), Symbol::Boolean(rb)) => Rc::from(Symbol::Boolean(*lb || *rb)),
			_ => panic!("invalid and operator expression"),
		},
		_ => panic!("invalid infix operator"),
	}
}

fn apply_function(function: &Symbol, arguments: &Vec<Rc<Symbol>>, env: &Env) -> Rc<Symbol> {
	match function {
		Symbol::FunctionLiteral { parameters, body } => {
			let mut enclosing_env = Environment::new_enclosed(Rc::clone(&env));
			parameters.iter().enumerate().for_each(|(i, parameter)| {
				enclosing_env.set(parameter.value.clone(), Rc::clone(&arguments[i]));
			});

			let evaluated = eval(
				Node::Statement(Statement::BlockStatement(body.clone())),
				&Rc::new(RefCell::new(enclosing_env)),
			);

			Rc::clone(&unwrap_return_value(evaluated))
		}
		_ => panic!("invalid function"),
	}
}

fn unwrap_return_value(symbol: Rc<Symbol>) -> Rc<Symbol> {
	if let Symbol::ReturnValue(value) = symbol.as_ref() {
		Rc::clone(&value)
	} else {
		symbol
	}
}
