extern crate lexer;

use lexer::token::Token;

#[derive(Debug, Clone, PartialEq)]
pub enum Node {
    Program(Program),
    Statement(Statement),
    Expression(Expression),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    Let {
        identifier: Identifier,
        expression: Expression,
    },
    Return {
        value: Expression,
    },
    Expression {
        expression: Expression,
    },
    Block {
        block: Block,
    },
}

pub type Block = Program;

#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    Identifier(Identifier),
    IntegerLiteral {
        value: i32,
    },
    BooleanLiteral {
        value: bool,
    },
    StringLiteral {
        value: String,
    },
    FunctionLiteral {
        parameters: Vec<Identifier>,
        body: Block,
    },
    Unary {
        operator: Token,
        operand: Box<Expression>,
    },
    Binary {
        operator: Token,
        left: Box<Expression>,
        right: Box<Expression>,
    },
    If {
        condition: Box<Expression>,
        consequence: Block,
        alternative: Option<Block>,
    },
    FunctionCall {
        function: Box<Expression>,
        arguments: Vec<Expression>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub struct Program {
    pub statements: Vec<Statement>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Identifier {
    pub value: String,
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
            Statement::Let {
                identifier,
                expression,
            } => write!(
                f,
                "tLET({}, {})",
                identifier.value.to_string(),
                expression.to_string()
            ),
            Statement::Return { value } => write!(f, "tRETURN({})", value.to_string()),
            Statement::Expression { expression } => {
                write!(f, "{}", expression.to_string())
            }
            Statement::Block { block } => write!(f, "{}", block.to_string()),
        }
    }
}

impl std::fmt::Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expression::Identifier(id) => write!(f, "{}", id.value.to_string()),
            Expression::IntegerLiteral { value } => write!(f, "{}", value.to_string()),
            Expression::BooleanLiteral { value } => write!(f, "{}", value.to_string()),
            Expression::StringLiteral { value } => write!(f, "{}", value.to_string()),
            Expression::FunctionLiteral { parameters, body } => write!(
                f,
                "tFUNCTION({}) {{{}}}",
                parameters
                    .iter()
                    .map(|param| param.value.to_string())
                    .collect::<Vec<String>>()
                    .join(", "),
                body.statements
                    .iter()
                    .map(|statement| statement.to_string())
                    .collect::<Vec<String>>()
                    .join(""),
            ),
            Expression::Unary { operator, operand } => {
                write!(f, "{}({})", operator.to_string(), operand.to_string())
            }
            Expression::Binary {
                operator,
                left,
                right,
            } => write!(
                f,
                "({} {} {})",
                left.to_string(),
                operator.to_string(),
                right.to_string()
            ),
            Expression::If {
                condition,
                consequence,
                alternative,
            } => {
                let alternative = match alternative {
                    Some(alt) => {
                        " tELSE {".to_owned()
                            + &alt
                                .statements
                                .iter()
                                .map(|statement| statement.to_string())
                                .collect::<Vec<String>>()
                                .join("")
                            + "}"
                    }
                    None => "".to_owned(),
                };

                write!(
                    f,
                    "tIF {} {{{}}}{}",
                    condition.to_string(),
                    consequence
                        .statements
                        .iter()
                        .map(|statement| statement.to_string())
                        .collect::<Vec<String>>()
                        .join(""),
                    alternative
                )
            }
            Expression::FunctionCall {
                function,
                arguments,
            } => write!(
                f,
                "tCALL({}, ({}))",
                function.to_string(),
                arguments
                    .iter()
                    .map(|arg| arg.to_string())
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
        }
    }
}

impl std::fmt::Display for Identifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value.to_string())
    }
}
