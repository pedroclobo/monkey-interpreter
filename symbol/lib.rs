pub mod environment;

#[derive(Debug, PartialEq, Clone)]
pub enum Symbol {
	Integer(i32),
	Boolean(bool),
	StringLiteral(String),
	Null,
	ReturnValue(ReturnValue),
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
	pub value: Box<Symbol>,
}

impl std::fmt::Display for Symbol {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			Symbol::Integer(i) => write!(f, "{}", i),
			Symbol::Boolean(b) => write!(f, "{}", b),
			Symbol::StringLiteral(s) => write!(f, "{}", s),
			Symbol::Null => write!(f, "NULL"),
			Symbol::ReturnValue(s) => write!(f, "{}", s.value),
		}
	}
}
