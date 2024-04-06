extern crate parser;
use parser::ast;

pub mod environment;

use std::rc::Rc;

#[derive(Debug, PartialEq, Clone)]
pub enum Symbol {
    Integer(i32),
    Boolean(bool),
    StringLiteral(String),
    Null,
    ReturnValue(Rc<Symbol>),
    FunctionLiteral {
        parameters: Vec<ast::Identifier>,
        block: ast::Block,
    },
    Array {
        elements: Vec<Rc<Symbol>>,
    },
}

impl std::fmt::Display for Symbol {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Symbol::Integer(value) => write!(f, "{}", value),
            Symbol::Boolean(value) => write!(f, "{}", value),
            Symbol::StringLiteral(value) => write!(f, "{}", value),
            Symbol::Null => write!(f, "NULL"),
            Symbol::ReturnValue(value) => write!(f, "{}", value),
            Symbol::FunctionLiteral {
                parameters,
                block: body,
            } => {
                write!(
                    f,
                    "fn({}) {{\n{}\n}}",
                    parameters
                        .iter()
                        .map(|i| i.to_string())
                        .collect::<Vec<String>>()
                        .join(", "),
                    body
                )
            }
            Symbol::Array { elements } => {
                write!(
                    f,
                    "[{}]",
                    elements
                        .iter()
                        .map(|e| e.to_string())
                        .collect::<Vec<String>>()
                        .join(", ")
                )
            }
        }
    }
}
