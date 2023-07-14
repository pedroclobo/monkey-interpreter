pub mod environment;

use std::rc::Rc;

#[derive(Debug, PartialEq, Clone)]
pub enum Symbol {
	Integer(i32),
	Boolean(bool),
	StringLiteral(String),
	Null,
	ReturnValue(Rc<Symbol>),
}

impl std::fmt::Display for Symbol {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			Symbol::Integer(i) => write!(f, "{}", i),
			Symbol::Boolean(b) => write!(f, "{}", b),
			Symbol::StringLiteral(s) => write!(f, "{}", s),
			Symbol::Null => write!(f, "NULL"),
			Symbol::ReturnValue(s) => write!(f, "{}", s),
		}
	}
}
