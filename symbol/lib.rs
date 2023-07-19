pub mod environment;

extern crate ast;

use std::rc::Rc;

#[derive(Debug, PartialEq, Clone)]
pub enum Symbol {
	Integer(Integer),
	Boolean(Boolean),
	StringLiteral(StringLiteral),
	Null(Null),
	ReturnValue(ReturnValue),
	FunctionLiteral(FunctionLiteral),
}

#[derive(Debug, PartialEq, Clone)]
pub struct Integer {
	pub value: i32,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Boolean {
	pub value: bool,
}

#[derive(Debug, PartialEq, Clone)]
pub struct StringLiteral {
	pub value: String,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Null {}

#[derive(Debug, PartialEq, Clone)]
pub struct ReturnValue {
	pub value: Rc<Symbol>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct FunctionLiteral {
	pub parameters: Vec<ast::Identifier>,
	pub body: ast::BlockStatement,
}

impl std::fmt::Display for Symbol {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			Symbol::Integer(Integer { value }) => write!(f, "{}", value),
			Symbol::Boolean(Boolean { value }) => write!(f, "{}", value),
			Symbol::StringLiteral(StringLiteral { value }) => write!(f, "{}", value),
			Symbol::Null(Null {}) => write!(f, "NULL"),
			Symbol::ReturnValue(ReturnValue { value }) => write!(f, "{}", value),
			Symbol::FunctionLiteral(FunctionLiteral {
				parameters, body, ..
			}) => {
				write!(
					f,
					"fn({}) {{\n{}\n}}",
					parameters
						.iter()
						.map(|i| i.to_string())
						.collect::<Vec<String>>()
						.join(", "),
					body.to_string()
				)
			}
		}
	}
}
