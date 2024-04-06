use lexer::{Location, TokenKind};

#[derive(Debug)]
pub enum EvaluatorError {
    InvalidIndentifier(String),
    InvalidPrefixExpression(Option<TokenKind>, Location),
    InvalidCondition,
    InvalidFunction,
    InvalidInfixExpression(Option<TokenKind>, Location),
    OutOfBounds,
    InvalidArray,
}

impl std::fmt::Display for EvaluatorError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            EvaluatorError::InvalidIndentifier(identifier) => {
                write!(f, "Identifier not found: {}", identifier)
            }
            EvaluatorError::InvalidPrefixExpression(token, location) => match token {
                Some(token) => write!(f, "{} - invalid prefix expression: {}", location, token),
                None => write!(f, "{} - invalid prefix expression", location),
            },
            EvaluatorError::InvalidCondition => write!(f, "Invalid condition"),
            EvaluatorError::InvalidFunction => write!(f, "Invalid function"),
            EvaluatorError::InvalidInfixExpression(token, location) => match token {
                Some(token) => write!(f, "{} - invalid infix expression: {}", location, token),
                None => write!(f, "{} - invalid infix expression", location),
            },
            EvaluatorError::OutOfBounds => write!(f, "Out of bounds"),
            EvaluatorError::InvalidArray => write!(f, "Invalid array"),
        }
    }
}
