extern crate lexer;

use lexer::token::Token;

pub enum Node {
	Program(Program),
	Statement(Statement),
	Expression(Expression),
}

pub enum Statement {
	LetStatement(LetStatement),
	ReturnStatement(ReturnStatement),
	ExpressionStatement(ExpressionStatement),
	BlockStatement(BlockStatement),
}

pub enum Expression {
	Identifier(Identifier),
	IntegerLiteral(IntegerLiteral),
	Boolean(Boolean),
	StringLiteral(StringLiteral),
	FunctionLiteral(FunctionLiteral),
	UnaryExpression(UnaryExpression),
	BinaryExpression(BinaryExpression),
	IfExpression(IfExpression),
	FunctionCall(FunctionCall),
}

pub struct Program {
	pub statements: Vec<Statement>,
}

pub struct LetStatement {
	pub identifier: Identifier,
	pub value: Expression,
}

pub struct ReturnStatement {
	pub value: Expression,
}

pub struct ExpressionStatement {
	pub expression: Expression,
}

pub struct BlockStatement {
	pub statements: Vec<Statement>,
}

pub struct Identifier {
	pub value: String,
}

pub struct IntegerLiteral {
	pub value: i32,
}

pub struct Boolean {
	pub value: bool,
}

pub struct StringLiteral {
	pub value: String,
}

pub struct FunctionLiteral {
	pub parameters: Vec<Identifier>,
	pub body: BlockStatement,
}

pub struct UnaryExpression {
	pub operator: Token,
	pub right: Box<Expression>,
}

pub struct BinaryExpression {
	pub left: Box<Expression>,
	pub operator: Token,
	pub right: Box<Expression>,
}

pub struct IfExpression {
	pub condition: Box<Expression>,
	pub consequence: BlockStatement,
	pub alternative: Option<BlockStatement>,
}

pub struct FunctionCall {
	pub function: Box<Expression>,
	pub arguments: Vec<Expression>,
}

impl std::fmt::Display for Node {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			Node::Program(p) => write!(f, "{}", p.to_string()),
			Node::Statement(s) => write!(f, "{}", s.to_string()),
			Node::Expression(e) => write!(f, "{}", e.to_string()),
		}
	}
}

impl std::fmt::Display for Program {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		let mut s = String::new();
		for stmt in &self.statements {
			s.push_str(&stmt.to_string());
		}
		write!(f, "{}", s)
	}
}

impl std::fmt::Display for Statement {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			Statement::LetStatement(l) => write!(
				f,
				"tLET({}, {})",
				l.identifier.value.to_string(),
				l.value.to_string()
			),
			Statement::ReturnStatement(r) => write!(f, "tRETURN({})", r.value.to_string()),
			Statement::ExpressionStatement(e) => write!(f, "{}", e.expression.to_string()),
			Statement::BlockStatement(b) => write!(
				f,
				"{}",
				b.statements
					.iter()
					.map(|statement| statement.to_string())
					.collect::<Vec<String>>()
					.join("")
			),
		}
	}
}

impl std::fmt::Display for Expression {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			Expression::Identifier(id) => write!(f, "{}", id.value.to_string()),
			Expression::IntegerLiteral(i) => write!(f, "{}", i.value.to_string()),
			Expression::Boolean(b) => write!(f, "{}", b.value.to_string()),
			Expression::StringLiteral(s) => write!(f, "{}", s.value.to_string()),
			Expression::FunctionLiteral(func) => write!(
				f,
				"tFUNCTION({}) {{{}}}",
				func.parameters
					.iter()
					.map(|param| param.value.to_string())
					.collect::<Vec<String>>()
					.join(", "),
				func.body
					.statements
					.iter()
					.map(|statement| statement.to_string())
					.collect::<Vec<String>>()
					.join(""),
			),
			Expression::UnaryExpression(expr) => write!(
				f,
				"{}({})",
				expr.operator.to_string(),
				expr.right.to_string()
			),
			Expression::BinaryExpression(expr) => write!(
				f,
				"({} {} {})",
				expr.left.to_string(),
				expr.operator.to_string(),
				expr.right.to_string()
			),
			Expression::IfExpression(expr) => {
				let alternative = match &expr.alternative {
					Some(alt) => {
						" tELSE {".to_owned()
							+ &alt
								.statements
								.iter()
								.map(|statement| statement.to_string())
								.collect::<Vec<String>>()
								.join("") + "}"
					}
					None => "".to_owned(),
				};

				write!(
					f,
					"tIF {} {{{}}}{}",
					expr.condition.to_string(),
					expr.consequence
						.statements
						.iter()
						.map(|statement| statement.to_string())
						.collect::<Vec<String>>()
						.join(""),
					alternative
				)
			}
			Expression::FunctionCall(call) => write!(
				f,
				"tCALL({}, ({}))",
				call.function.to_string(),
				call.arguments
					.iter()
					.map(|arg| arg.to_string())
					.collect::<Vec<String>>()
					.join(", ")
			),
		}
	}
}
