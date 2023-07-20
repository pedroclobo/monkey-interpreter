extern crate lexer;
extern crate parser;
extern crate symbol;

use lexer::token::Token;
use parser::ast;
use parser::ast::{Expression, Node, Program, Statement};
use symbol::environment::Environment;
use symbol::Symbol;

use std::cell::RefCell;
use std::rc::Rc;

mod tests;

type Env = Rc<RefCell<Environment>>;

#[derive(Debug)]
pub enum EvaluatorError {
	InvalidIndentifierError(String),
	InvalidPrefixExpressionError(Option<Token>),
	InvalidConditionError,
	InvalidFunctionError,
	InvalidInfixExpressionError(Option<Token>),
}

impl std::fmt::Display for EvaluatorError {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		use EvaluatorError::*;

		match self {
			InvalidIndentifierError(identifier) => {
				write!(f, "identifier not found: {}", identifier)
			}
			InvalidPrefixExpressionError(token) => match token {
				Some(token) => write!(f, "invalid prefix expression: {}", token),
				None => write!(f, "invalid prefix expression"),
			},
			InvalidConditionError => write!(f, "invalid condition"),
			InvalidFunctionError => write!(f, "invalid function"),
			InvalidInfixExpressionError(token) => match token {
				Some(token) => write!(f, "invalid infix expression: {}", token),
				None => write!(f, "invalid infix expression"),
			},
		}
	}
}

pub fn eval(node: Node, env: &Env) -> Result<Rc<Symbol>, EvaluatorError> {
	match node {
		Node::Program(Program { statements }) => Ok(eval_block(&statements, &Rc::clone(&env))?),
		Node::Statement(statement) => Ok(eval_statement(statement, &Rc::clone(&env))?),
		Node::Expression(expression) => Ok(eval_expression(expression, &Rc::clone(env))?),
	}
}

fn eval_statement(statement: Statement, env: &Env) -> Result<Rc<Symbol>, EvaluatorError> {
	match statement {
		Statement::LetStatement(ast::LetStatement {
			identifier,
			expression,
		}) => {
			let expression = eval(Node::Expression(expression), &Rc::clone(&env))?;
			env.borrow_mut()
				.set(identifier.value, Rc::clone(&expression));
			Ok(expression)
		}
		Statement::ReturnStatement(ast::ReturnStatement { value }) => Ok(Rc::from(
			Symbol::ReturnValue(eval(Node::Expression(value), env)?),
		)),
		Statement::ExpressionStatement(ast::ExpressionStatement { expression }) => {
			Ok(eval(Node::Expression(expression), env)?)
		}
		Statement::BlockStatement(ast::BlockStatement { statements }) => {
			Ok(eval_block(&statements, &Rc::clone(&env))?)
		}
	}
}

fn eval_expression(expression: Expression, env: &Env) -> Result<Rc<Symbol>, EvaluatorError> {
	match expression {
		Expression::Identifier(ast::Identifier { value }) => match env.borrow().get(&value) {
			Some(value) => Ok(Rc::clone(&value)),
			None => Err(EvaluatorError::InvalidIndentifierError(value)),
		},
		Expression::IntegerLiteral(ast::IntegerLiteral { value }) => {
			Ok(Rc::from(Symbol::Integer(value)))
		}
		Expression::Boolean(ast::Boolean { value }) => Ok(Rc::from(Symbol::Boolean(value))),
		Expression::StringLiteral(ast::StringLiteral { value }) => {
			Ok(Rc::from(Symbol::StringLiteral(value)))
		}
		Expression::FunctionLiteral(ast::FunctionLiteral { parameters, body }) => {
			Ok(Rc::from(Symbol::FunctionLiteral { parameters, body }))
		}
		Expression::UnaryExpression(ast::UnaryExpression { operator, right }) => {
			let right = eval(Node::Expression(*right), env)?;
			Ok(Rc::clone(&eval_prefix_expression(operator, &right)?))
		}
		Expression::BinaryExpression(ast::BinaryExpression {
			left,
			operator,
			right,
		}) => {
			let left = eval(Node::Expression(*left), &Rc::clone(&env))?;
			let right = eval(Node::Expression(*right), &Rc::clone(&env))?;
			Ok(Rc::clone(&eval_infix_expression(operator, &left, &right)?))
		}
		Expression::IfExpression(ast::IfExpression {
			condition,
			consequence,
			alternative,
		}) => {
			let condition = eval(Node::Expression(*condition), &Rc::clone(&env))?;
			match *condition {
				Symbol::Boolean(true) => eval(
					Node::Statement(Statement::BlockStatement(consequence)),
					&Rc::clone(&env),
				),
				Symbol::Boolean(false) => match alternative {
					Some(alternative) => Ok(eval(
						Node::Statement(Statement::BlockStatement(alternative)),
						env,
					)?),
					None => Ok(Rc::from(Symbol::Null)),
				},
				_ => Err(EvaluatorError::InvalidConditionError),
			}
		}
		Expression::FunctionCall(ast::FunctionCall {
			function,
			arguments,
		}) => {
			let function = eval(Node::Expression(*function), &Rc::clone(&env))?;
			let arguments = eval_expressions(&arguments, &Rc::clone(&env))?;

			Ok(apply_function(&function, &arguments, &Rc::clone(&env))?)
		}
	}
}

fn eval_block(statements: &Vec<Statement>, env: &Env) -> Result<Rc<Symbol>, EvaluatorError> {
	let mut result = Rc::new(Symbol::Null);

	for statement in statements {
		result = eval(Node::Statement(statement.clone()), env)?;

		match *result {
			Symbol::ReturnValue(_) => return Ok(result),
			_ => continue,
		}
	}

	Ok(result)
}

fn eval_expressions(
	expressions: &Vec<Expression>,
	env: &Env,
) -> Result<Vec<Rc<Symbol>>, EvaluatorError> {
	expressions
		.iter()
		.map(|expression| eval(Node::Expression(expression.clone()), env))
		.collect()
}

fn eval_prefix_expression(operator: Token, symbol: &Symbol) -> Result<Rc<Symbol>, EvaluatorError> {
	match operator {
		Token::MINUS => match symbol {
			Symbol::Integer(value) => Ok(Rc::from(Symbol::Integer(-value))),
			_ => Err(EvaluatorError::InvalidPrefixExpressionError(Some(
				Token::MINUS,
			))),
		},
		Token::NOT => match symbol {
			Symbol::Boolean(value) => Ok(Rc::from(Symbol::Boolean(!value))),
			_ => Err(EvaluatorError::InvalidPrefixExpressionError(Some(
				Token::NOT,
			))),
		},
		_ => Err(EvaluatorError::InvalidPrefixExpressionError(None)),
	}
}

fn eval_infix_expression(
	operator: Token,
	left: &Symbol,
	right: &Symbol,
) -> Result<Rc<Symbol>, EvaluatorError> {
	match operator {
		Token::PLUS => match (left, right) {
			(Symbol::Integer(li), Symbol::Integer(ri)) => Ok(Rc::from(Symbol::Integer(li + ri))),
			(Symbol::StringLiteral(ls), Symbol::StringLiteral(rs)) => {
				Ok(Rc::from(Symbol::StringLiteral(ls.to_owned() + rs)))
			}
			_ => Err(EvaluatorError::InvalidInfixExpressionError(Some(
				Token::PLUS,
			))),
		},
		Token::MINUS => match (left, right) {
			(Symbol::Integer(li), Symbol::Integer(ri)) => Ok(Rc::from(Symbol::Integer(li - ri))),
			_ => Err(EvaluatorError::InvalidInfixExpressionError(Some(
				Token::MINUS,
			))),
		},
		Token::MUL => match (left, right) {
			(Symbol::Integer(li), Symbol::Integer(ri)) => Ok(Rc::from(Symbol::Integer(li * ri))),
			_ => Err(EvaluatorError::InvalidInfixExpressionError(Some(
				Token::MUL,
			))),
		},
		Token::DIV => match (left, right) {
			(Symbol::Integer(li), Symbol::Integer(ri)) => Ok(Rc::from(Symbol::Integer(li / ri))),
			_ => Err(EvaluatorError::InvalidInfixExpressionError(Some(
				Token::DIV,
			))),
		},
		Token::LT => match (left, right) {
			(Symbol::Integer(li), Symbol::Integer(ri)) => Ok(Rc::from(Symbol::Boolean(li < ri))),
			_ => Err(EvaluatorError::InvalidInfixExpressionError(Some(Token::LT))),
		},
		Token::GT => match (left, right) {
			(Symbol::Integer(li), Symbol::Integer(ri)) => Ok(Rc::from(Symbol::Boolean(li > ri))),
			_ => Err(EvaluatorError::InvalidInfixExpressionError(Some(Token::GT))),
		},
		Token::LE => match (left, right) {
			(Symbol::Integer(li), Symbol::Integer(ri)) => Ok(Rc::from(Symbol::Boolean(li <= ri))),
			_ => Err(EvaluatorError::InvalidInfixExpressionError(Some(Token::LE))),
		},
		Token::GE => match (left, right) {
			(Symbol::Integer(li), Symbol::Integer(ri)) => Ok(Rc::from(Symbol::Boolean(li >= ri))),
			_ => Err(EvaluatorError::InvalidInfixExpressionError(Some(Token::GE))),
		},
		Token::EQ => match (left, right) {
			(Symbol::Integer(li), Symbol::Integer(ri)) => Ok(Rc::from(Symbol::Boolean(li == ri))),
			(Symbol::Boolean(lb), Symbol::Boolean(rb)) => Ok(Rc::from(Symbol::Boolean(lb == rb))),

			_ => Err(EvaluatorError::InvalidInfixExpressionError(Some(Token::EQ))),
		},
		Token::NE => match (left, right) {
			(Symbol::Integer(li), Symbol::Integer(ri)) => Ok(Rc::from(Symbol::Boolean(li != ri))),
			(Symbol::Boolean(lb), Symbol::Boolean(rb)) => Ok(Rc::from(Symbol::Boolean(lb != rb))),

			_ => Err(EvaluatorError::InvalidInfixExpressionError(Some(Token::NE))),
		},
		Token::AND => match (left, right) {
			(Symbol::Boolean(lb), Symbol::Boolean(rb)) => Ok(Rc::from(Symbol::Boolean(*lb && *rb))),
			_ => Err(EvaluatorError::InvalidInfixExpressionError(Some(
				Token::AND,
			))),
		},
		Token::OR => match (left, right) {
			(Symbol::Boolean(lb), Symbol::Boolean(rb)) => Ok(Rc::from(Symbol::Boolean(*lb || *rb))),
			_ => Err(EvaluatorError::InvalidInfixExpressionError(Some(Token::OR))),
		},
		_ => Err(EvaluatorError::InvalidInfixExpressionError(None)),
	}
}

fn apply_function(
	function: &Symbol,
	arguments: &Vec<Rc<Symbol>>,
	env: &Env,
) -> Result<Rc<Symbol>, EvaluatorError> {
	match function {
		Symbol::FunctionLiteral { parameters, body } => {
			let mut enclosing_env = Environment::new_enclosed(Rc::clone(&env));
			parameters.iter().enumerate().for_each(|(i, parameter)| {
				enclosing_env.set(parameter.value.clone(), Rc::clone(&arguments[i]));
			});

			let evaluated = eval(
				Node::Statement(Statement::BlockStatement(body.clone())),
				&Rc::new(RefCell::new(enclosing_env)),
			)?;

			Ok(Rc::clone(&(unwrap_return_value(evaluated)?)))
		}
		_ => Err(EvaluatorError::InvalidFunctionError),
	}
}

fn unwrap_return_value(symbol: Rc<Symbol>) -> Result<Rc<Symbol>, EvaluatorError> {
	if let Symbol::ReturnValue(value) = symbol.as_ref() {
		Ok(Rc::clone(&value))
	} else {
		Ok(symbol)
	}
}
