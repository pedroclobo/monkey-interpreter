use lexer::{Location, TokenKind};

#[derive(Debug)]
pub enum EvaluatorError {
    InvalidIdentifier(String),
    InvalidPrefixExpression(TokenKind, Location),
    InvalidCondition,
    InvalidFunction,
    InvalidInfixExpression(TokenKind, Location),
    OutOfBounds,
    InvalidArray,
    WrongArgumentCount(u8, u8),
    InvalidArgumentType,
    InvalidKey,
}

impl std::fmt::Display for EvaluatorError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            EvaluatorError::InvalidIdentifier(identifier) => {
                write!(f, "Identifier not found: {}", identifier)
            }
            EvaluatorError::InvalidPrefixExpression(token, location) => {
                write!(f, "{} - invalid prefix expression: {}", location, token)
            }
            EvaluatorError::InvalidCondition => write!(f, "Invalid condition"),
            EvaluatorError::InvalidFunction => write!(f, "Invalid function"),
            EvaluatorError::InvalidInfixExpression(token, location) => {
                write!(f, "{} - invalid infix expression: {}", location, token)
            }
            EvaluatorError::OutOfBounds => write!(f, "Out of bounds"),
            EvaluatorError::InvalidArray => write!(f, "Invalid array"),
            EvaluatorError::WrongArgumentCount(expected, actual) => {
                write!(
                    f,
                    "Wrong argument count: expected {}, got {}",
                    expected, actual
                )
            }
            EvaluatorError::InvalidArgumentType => write!(f, "Invalid argument type"),
            EvaluatorError::InvalidKey => write!(f, "Invalid key"),
        }
    }
}
