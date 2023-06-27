#[derive(Debug, PartialEq)]
pub enum Symbol {
	Integer(Integer),
	Boolean(Boolean),
	StringLiteral(StringLiteral),
	Null(Null),
}

#[derive(Debug, PartialEq)]
pub struct Integer {
	pub value: i32,
}

#[derive(Debug, PartialEq)]
pub struct Boolean {
	pub value: bool,
}

#[derive(Debug, PartialEq)]
pub struct StringLiteral {
	pub value: String,
}

#[derive(Debug, PartialEq)]
pub struct Null {}

impl std::fmt::Display for Symbol {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			Symbol::Integer(i) => write!(f, "{}", i.value),
			Symbol::Boolean(b) => write!(f, "{}", b.value),
			Symbol::StringLiteral(s) => write!(f, "{}", s.value),
			Symbol::Null(_) => write!(f, "NULL"),
		}
	}
}
