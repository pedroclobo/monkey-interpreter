extern crate ast;
extern crate lexer;
extern crate parser;
extern crate symbol;

use std::cell::RefCell;
use std::rc::Rc;

mod evaluator_tests;

pub fn eval(
	node: ast::Node,
	env: &Rc<RefCell<symbol::environment::Environment>>,
) -> Rc<symbol::Symbol> {
	match node {
		ast::Node::Program(ast::Program { statements }) => {
			eval_block(&statements, &Rc::clone(&env))
		}
		ast::Node::Statement(statement) => match statement {
			ast::Statement::LetStatement(ast::LetStatement { identifier, value }) => {
				let val = eval(ast::Node::Expression(value), &Rc::clone(&env));
				env.borrow_mut().set(identifier.value, Rc::clone(&val));
				val
			}
			ast::Statement::ReturnStatement(ast::ReturnStatement { value }) => {
				symbol::Symbol::ReturnValue(symbol::ReturnValue {
					value: eval(ast::Node::Expression(value), env),
				})
				.into()
			}
			ast::Statement::ExpressionStatement(ast::ExpressionStatement { expression }) => {
				eval(ast::Node::Expression(expression), env)
			}
		},
		ast::Node::BlockStatement(ast::BlockStatement { statements }) => {
			eval_block(&statements, &Rc::clone(&env))
		}
		ast::Node::Expression(expression) => match expression {
			ast::Expression::Identifier(ast::Identifier { value }) => {
				match env.borrow().get(&value) {
					Some(v) => v.clone().into(),
					None => panic!("invalid identifier: {}", value),
				}
			}
			ast::Expression::IntegerLiteral(ast::IntegerLiteral { value }) => {
				symbol::Symbol::Integer(symbol::Integer { value }).into()
			}
			ast::Expression::Boolean(ast::Boolean { value }) => {
				symbol::Symbol::Boolean(symbol::Boolean { value }).into()
			}
			ast::Expression::StringLiteral(ast::StringLiteral { value }) => {
				symbol::Symbol::StringLiteral(symbol::StringLiteral { value }).into()
			}
			ast::Expression::FunctionLiteral(ast::FunctionLiteral { parameters, body }) => {
				symbol::Symbol::FunctionLiteral(symbol::FunctionLiteral { parameters, body }).into()
			}
			ast::Expression::UnaryExpression(ast::UnaryExpression { operator, right }) => {
				let right = eval(ast::Node::Expression(*right), env);
				eval_prefix_expression(operator, &right).into()
			}
			ast::Expression::BinaryExpression(ast::BinaryExpression {
				left,
				operator,
				right,
			}) => {
				let left = eval(ast::Node::Expression(*left), &Rc::clone(&env));
				let right = eval(ast::Node::Expression(*right), &Rc::clone(&env));
				eval_infix_expression(operator, &left, &right).into()
			}
			ast::Expression::IfExpression(ast::IfExpression {
				condition,
				consequence,
				alternative,
			}) => {
				let condition = eval(ast::Node::Expression(*condition), &Rc::clone(&env));
				match *condition {
					symbol::Symbol::Boolean(symbol::Boolean { value: true }) => {
						eval(ast::Node::BlockStatement(consequence), &Rc::clone(&env))
					}
					symbol::Symbol::Boolean(symbol::Boolean { value: false }) => {
						match alternative {
							Some(alt) => eval(ast::Node::BlockStatement(alt), env),
							None => symbol::Symbol::Null(symbol::Null {}).into(),
						}
					}
					_ => panic!("invalid condition"),
				}
			}
			ast::Expression::FunctionCall(ast::FunctionCall {
				function,
				arguments,
			}) => {
				let function = eval(ast::Node::Expression(*function), &Rc::clone(&env));
				let arguments = eval_expressions(&arguments, &Rc::clone(&env));

				apply_function(&function, &arguments, &Rc::clone(&env))
			}
		},
	}
}

fn apply_function(
	function: &symbol::Symbol,
	arguments: &Vec<Rc<symbol::Symbol>>,
	env: &Rc<RefCell<symbol::environment::Environment>>,
) -> Rc<symbol::Symbol> {
	match function.clone() {
		symbol::Symbol::FunctionLiteral(symbol::FunctionLiteral { parameters, body }) => {
			let mut enclosing_env = symbol::environment::Environment::new_enclosed(Rc::clone(&env));
			parameters.iter().enumerate().for_each(|(i, parameter)| {
				enclosing_env.set(parameter.value.clone(), arguments[i].clone());
			});
			let evaluated = eval(
				ast::Node::BlockStatement(body.clone()).clone(),
				&Rc::new(RefCell::new(enclosing_env)),
			);

			unwrap_return_value(evaluated).into()
		}
		_ => unreachable!(),
	}
}

fn unwrap_return_value(symbol: Rc<symbol::Symbol>) -> Rc<symbol::Symbol> {
	if let symbol::Symbol::ReturnValue(ret) = symbol.as_ref() {
		ret.value.clone()
	} else {
		symbol
	}
}

fn eval_block(
	statements: &Vec<ast::Statement>,
	env: &Rc<RefCell<symbol::environment::Environment>>,
) -> Rc<symbol::Symbol> {
	let mut result = Rc::new(symbol::Symbol::Null(symbol::Null {}));

	for statement in statements {
		result = eval(ast::Node::Statement(statement.clone()), env);

		match *result {
			symbol::Symbol::ReturnValue(_) => return result,
			_ => continue,
		}
	}

	result
}

fn eval_expressions(
	expressions: &Vec<ast::Expression>,
	env: &Rc<RefCell<symbol::environment::Environment>>,
) -> Vec<Rc<symbol::Symbol>> {
	expressions
		.iter()
		.map(|e| eval(ast::Node::Expression(e.clone()), env))
		.collect()
}

fn eval_prefix_expression(
	operator: lexer::token::Token,
	symbol: &symbol::Symbol,
) -> Rc<symbol::Symbol> {
	match operator {
		lexer::token::Token::MINUS => match symbol {
			symbol::Symbol::Integer(symbol::Integer { value }) => {
				symbol::Symbol::Integer(symbol::Integer { value: -value }).into()
			}
			_ => panic!("invalid minus operator expression"),
		},
		lexer::token::Token::NOT => match symbol {
			symbol::Symbol::Boolean(symbol::Boolean { value }) => {
				symbol::Symbol::Boolean(symbol::Boolean { value: !value }).into()
			}
			_ => panic!("invalid not operator expression"),
		},
		_ => panic!("invalid prefix operator"),
	}
}

fn eval_infix_expression(
	operator: lexer::token::Token,
	left: &symbol::Symbol,
	right: &symbol::Symbol,
) -> Rc<symbol::Symbol> {
	match operator {
		lexer::token::Token::PLUS => match (left, right) {
			(
				symbol::Symbol::Integer(symbol::Integer { value: li }),
				symbol::Symbol::Integer(symbol::Integer { value: ri }),
			) => symbol::Symbol::Integer(symbol::Integer { value: li + ri }).into(),
			_ => panic!("invalid plus operator expression"),
		},
		lexer::token::Token::MINUS => match (left, right) {
			(
				symbol::Symbol::Integer(symbol::Integer { value: li }),
				symbol::Symbol::Integer(symbol::Integer { value: ri }),
			) => symbol::Symbol::Integer(symbol::Integer { value: li - ri }).into(),
			_ => panic!("invalid minus operator expression"),
		},
		lexer::token::Token::MUL => match (left, right) {
			(
				symbol::Symbol::Integer(symbol::Integer { value: li }),
				symbol::Symbol::Integer(symbol::Integer { value: ri }),
			) => symbol::Symbol::Integer(symbol::Integer { value: li * ri }).into(),
			_ => panic!("invalid mul operator expression"),
		},
		lexer::token::Token::DIV => match (left, right) {
			(
				symbol::Symbol::Integer(symbol::Integer { value: li }),
				symbol::Symbol::Integer(symbol::Integer { value: ri }),
			) => symbol::Symbol::Integer(symbol::Integer { value: li / ri }).into(),
			_ => panic!("invalid div operator expression"),
		},
		lexer::token::Token::LT => match (left, right) {
			(
				symbol::Symbol::Integer(symbol::Integer { value: li }),
				symbol::Symbol::Integer(symbol::Integer { value: ri }),
			) => symbol::Symbol::Boolean(symbol::Boolean { value: li < ri }).into(),
			_ => panic!("invalid lt operator expression"),
		},
		lexer::token::Token::GT => match (left, right) {
			(
				symbol::Symbol::Integer(symbol::Integer { value: li }),
				symbol::Symbol::Integer(symbol::Integer { value: ri }),
			) => symbol::Symbol::Boolean(symbol::Boolean { value: li > ri }).into(),
			_ => panic!("invalid gt operator expression"),
		},
		lexer::token::Token::LE => match (left, right) {
			(
				symbol::Symbol::Integer(symbol::Integer { value: li }),
				symbol::Symbol::Integer(symbol::Integer { value: ri }),
			) => symbol::Symbol::Boolean(symbol::Boolean { value: li <= ri }).into(),
			_ => panic!("invalid le operator expression"),
		},
		lexer::token::Token::GE => match (left, right) {
			(
				symbol::Symbol::Integer(symbol::Integer { value: li }),
				symbol::Symbol::Integer(symbol::Integer { value: ri }),
			) => symbol::Symbol::Boolean(symbol::Boolean { value: li >= ri }).into(),
			_ => panic!("invalid ge operator expression"),
		},
		lexer::token::Token::EQ => match (left, right) {
			(
				symbol::Symbol::Integer(symbol::Integer { value: li }),
				symbol::Symbol::Integer(symbol::Integer { value: ri }),
			) => symbol::Symbol::Boolean(symbol::Boolean { value: li == ri }).into(),
			(
				symbol::Symbol::Boolean(symbol::Boolean { value: lb }),
				symbol::Symbol::Boolean(symbol::Boolean { value: rb }),
			) => symbol::Symbol::Boolean(symbol::Boolean { value: lb == rb }).into(),

			_ => panic!("invalid eq operator expression"),
		},
		lexer::token::Token::NE => match (left, right) {
			(
				symbol::Symbol::Integer(symbol::Integer { value: li }),
				symbol::Symbol::Integer(symbol::Integer { value: ri }),
			) => symbol::Symbol::Boolean(symbol::Boolean { value: li != ri }).into(),
			(
				symbol::Symbol::Boolean(symbol::Boolean { value: lb }),
				symbol::Symbol::Boolean(symbol::Boolean { value: rb }),
			) => symbol::Symbol::Boolean(symbol::Boolean { value: lb != rb }).into(),

			_ => panic!("invalid ne operator expression"),
		},
		lexer::token::Token::AND => match (left, right) {
			(
				symbol::Symbol::Boolean(symbol::Boolean { value: lb }),
				symbol::Symbol::Boolean(symbol::Boolean { value: rb }),
			) => symbol::Symbol::Boolean(symbol::Boolean { value: *lb && *rb }).into(),
			_ => panic!("invalid and operator expression"),
		},
		lexer::token::Token::OR => match (left, right) {
			(
				symbol::Symbol::Boolean(symbol::Boolean { value: lb }),
				symbol::Symbol::Boolean(symbol::Boolean { value: rb }),
			) => symbol::Symbol::Boolean(symbol::Boolean { value: *lb || *rb }).into(),
			_ => panic!("invalid and operator expression"),
		},
		_ => panic!("invalid infix operator"),
	}
}
