extern crate lexer;
extern crate parser;
extern crate symbol;

use lexer::{Location, Token, TokenKind};
use parser::ast::{self, Block};
use parser::ast::{Expression, Node, Statement};
use symbol::environment::Environment;
use symbol::Symbol;

use std::cell::RefCell;
use std::rc::Rc;

type Env = Rc<RefCell<Environment>>;

#[derive(Debug)]
pub enum EvaluatorError {
    InvalidIndentifierError(String),
    InvalidPrefixExpressionError(Option<TokenKind>, Location),
    InvalidConditionError,
    InvalidFunctionError,
    InvalidInfixExpressionError(Option<TokenKind>, Location),
}

impl std::fmt::Display for EvaluatorError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use EvaluatorError::*;

        match self {
            InvalidIndentifierError(identifier) => {
                write!(f, "identifier not found: {}", identifier)
            }
            InvalidPrefixExpressionError(token, location) => match token {
                Some(token) => write!(f, "{} - invalid prefix expression: {}", location, token),
                None => write!(f, "{} - invalid prefix expression", location),
            },
            InvalidConditionError => write!(f, "invalid condition"),
            InvalidFunctionError => write!(f, "invalid function"),
            InvalidInfixExpressionError(token, location) => match token {
                Some(token) => write!(f, "{} - invalid infix expression: {}", location, token),
                None => write!(f, "{} - invalid infix expression", location),
            },
        }
    }
}

pub fn eval(node: Node, env: &Env) -> Result<Rc<Symbol>, EvaluatorError> {
    match node {
        Node::Program(program) => Ok(eval_block(program, &Rc::clone(env))?),
        Node::Statement(statement) => Ok(eval_statement(statement, &Rc::clone(env))?),
        Node::Expression(expression) => Ok(eval_expression(expression, &Rc::clone(env))?),
    }
}

fn eval_statement(statement: Statement, env: &Env) -> Result<Rc<Symbol>, EvaluatorError> {
    match statement {
        Statement::Let {
            identifier,
            expression,
        } => {
            let expression = eval(Node::Expression(expression), &Rc::clone(env))?;
            env.borrow_mut()
                .set(identifier.value, Rc::clone(&expression));
            Ok(expression)
        }
        Statement::Return { value } => Ok(Rc::from(Symbol::ReturnValue(eval(
            Node::Expression(value),
            env,
        )?))),
        Statement::Expression { expression } => Ok(eval(Node::Expression(expression), env)?),
        Statement::Block { block } => Ok(eval_block(block, &Rc::clone(env))?),
    }
}

fn eval_expression(expression: Expression, env: &Env) -> Result<Rc<Symbol>, EvaluatorError> {
    match expression {
        Expression::Identifier(ast::Identifier { value }) => match env.borrow().get(&value) {
            Some(value) => Ok(Rc::clone(&value)),
            None => Err(EvaluatorError::InvalidIndentifierError(value)),
        },
        Expression::IntegerLiteral { value } => Ok(Rc::from(Symbol::Integer(value))),
        Expression::BooleanLiteral { value } => Ok(Rc::from(Symbol::Boolean(value))),
        Expression::StringLiteral { value } => Ok(Rc::from(Symbol::StringLiteral(value))),
        Expression::FunctionLiteral { parameters, body } => Ok(Rc::from(Symbol::FunctionLiteral {
            parameters,
            block: body,
        })),
        Expression::Unary { operator, operand } => {
            let right = eval(Node::Expression(*operand), env)?;
            Ok(Rc::clone(&eval_prefix_expression(operator, &right)?))
        }
        Expression::Binary {
            left,
            operator,
            right,
        } => {
            let left = eval(Node::Expression(*left), &Rc::clone(env))?;
            let right = eval(Node::Expression(*right), &Rc::clone(env))?;
            Ok(Rc::clone(&eval_infix_expression(operator, &left, &right)?))
        }
        Expression::If {
            condition,
            consequence,
            alternative,
        } => {
            let condition = eval(Node::Expression(*condition), &Rc::clone(env))?;
            match *condition {
                Symbol::Boolean(true) => eval_block(consequence, &Rc::clone(env)),
                Symbol::Boolean(false) => match alternative {
                    Some(alternative) => Ok(eval_block(alternative, env)?),
                    None => Ok(Rc::from(Symbol::Null)),
                },
                _ => Err(EvaluatorError::InvalidConditionError),
            }
        }
        Expression::FunctionCall {
            function,
            arguments,
        } => {
            let function = eval(Node::Expression(*function), &Rc::clone(env))?;
            let arguments = eval_expressions(&arguments, &Rc::clone(env))?;

            Ok(apply_function(&function, &arguments, &Rc::clone(env))?)
        }
    }
}

fn eval_block(block: Block, env: &Env) -> Result<Rc<Symbol>, EvaluatorError> {
    let mut result = Rc::new(Symbol::Null);

    for statement in block.statements {
        result = eval(Node::Statement(statement.clone()), env)?;

        match *result {
            Symbol::ReturnValue(_) => return Ok(result),
            _ => continue,
        }
    }

    Ok(result)
}

fn eval_expressions(
    expressions: &[Expression],
    env: &Env,
) -> Result<Vec<Rc<Symbol>>, EvaluatorError> {
    expressions
        .iter()
        .map(|expression| eval(Node::Expression(expression.clone()), env))
        .collect()
}

fn eval_prefix_expression(operator: Token, symbol: &Symbol) -> Result<Rc<Symbol>, EvaluatorError> {
    match operator.kind {
        TokenKind::Minus => match symbol {
            Symbol::Integer(value) => Ok(Rc::from(Symbol::Integer(-value))),
            _ => Err(EvaluatorError::InvalidPrefixExpressionError(
                Some(TokenKind::Minus),
                operator.location,
            )),
        },
        TokenKind::Not => match symbol {
            Symbol::Boolean(value) => Ok(Rc::from(Symbol::Boolean(!value))),
            _ => Err(EvaluatorError::InvalidPrefixExpressionError(
                Some(TokenKind::Not),
                operator.location,
            )),
        },
        kind => Err(EvaluatorError::InvalidPrefixExpressionError(
            Some(kind),
            operator.location,
        )),
    }
}

fn eval_infix_expression(
    operator: Token,
    left: &Symbol,
    right: &Symbol,
) -> Result<Rc<Symbol>, EvaluatorError> {
    match operator.kind {
        TokenKind::Plus => match (left, right) {
            (Symbol::Integer(li), Symbol::Integer(ri)) => Ok(Rc::from(Symbol::Integer(li + ri))),
            (Symbol::StringLiteral(ls), Symbol::StringLiteral(rs)) => {
                Ok(Rc::from(Symbol::StringLiteral(ls.to_owned() + rs)))
            }
            _ => Err(EvaluatorError::InvalidInfixExpressionError(
                Some(TokenKind::Plus),
                operator.location,
            )),
        },
        TokenKind::Minus => match (left, right) {
            (Symbol::Integer(li), Symbol::Integer(ri)) => Ok(Rc::from(Symbol::Integer(li - ri))),
            _ => Err(EvaluatorError::InvalidInfixExpressionError(
                Some(TokenKind::Minus),
                operator.location,
            )),
        },
        TokenKind::Multiplication => match (left, right) {
            (Symbol::Integer(li), Symbol::Integer(ri)) => Ok(Rc::from(Symbol::Integer(li * ri))),
            _ => Err(EvaluatorError::InvalidInfixExpressionError(
                Some(TokenKind::Multiplication),
                operator.location,
            )),
        },
        TokenKind::Division => match (left, right) {
            (Symbol::Integer(li), Symbol::Integer(ri)) => Ok(Rc::from(Symbol::Integer(li / ri))),
            _ => Err(EvaluatorError::InvalidInfixExpressionError(
                Some(TokenKind::Division),
                operator.location,
            )),
        },
        TokenKind::LessThan => match (left, right) {
            (Symbol::Integer(li), Symbol::Integer(ri)) => Ok(Rc::from(Symbol::Boolean(li < ri))),
            _ => Err(EvaluatorError::InvalidInfixExpressionError(
                Some(TokenKind::LessThan),
                operator.location,
            )),
        },
        TokenKind::GreaterThan => match (left, right) {
            (Symbol::Integer(li), Symbol::Integer(ri)) => Ok(Rc::from(Symbol::Boolean(li > ri))),
            _ => Err(EvaluatorError::InvalidInfixExpressionError(
                Some(TokenKind::GreaterThan),
                operator.location,
            )),
        },
        TokenKind::LessThanOrEqual => match (left, right) {
            (Symbol::Integer(li), Symbol::Integer(ri)) => Ok(Rc::from(Symbol::Boolean(li <= ri))),
            _ => Err(EvaluatorError::InvalidInfixExpressionError(
                Some(TokenKind::LessThanOrEqual),
                operator.location,
            )),
        },
        TokenKind::GreaterThanOrEqual => match (left, right) {
            (Symbol::Integer(li), Symbol::Integer(ri)) => Ok(Rc::from(Symbol::Boolean(li >= ri))),
            _ => Err(EvaluatorError::InvalidInfixExpressionError(
                Some(TokenKind::GreaterThanOrEqual),
                operator.location,
            )),
        },
        TokenKind::Equal => match (left, right) {
            (Symbol::Integer(li), Symbol::Integer(ri)) => Ok(Rc::from(Symbol::Boolean(li == ri))),
            (Symbol::Boolean(lb), Symbol::Boolean(rb)) => Ok(Rc::from(Symbol::Boolean(lb == rb))),

            _ => Err(EvaluatorError::InvalidInfixExpressionError(
                Some(TokenKind::Equal),
                operator.location,
            )),
        },
        TokenKind::NotEqual => match (left, right) {
            (Symbol::Integer(li), Symbol::Integer(ri)) => Ok(Rc::from(Symbol::Boolean(li != ri))),
            (Symbol::Boolean(lb), Symbol::Boolean(rb)) => Ok(Rc::from(Symbol::Boolean(lb != rb))),

            _ => Err(EvaluatorError::InvalidInfixExpressionError(
                Some(TokenKind::NotEqual),
                operator.location,
            )),
        },
        TokenKind::And => match (left, right) {
            (Symbol::Boolean(lb), Symbol::Boolean(rb)) => Ok(Rc::from(Symbol::Boolean(*lb && *rb))),
            _ => Err(EvaluatorError::InvalidInfixExpressionError(
                Some(TokenKind::And),
                operator.location,
            )),
        },
        TokenKind::Or => match (left, right) {
            (Symbol::Boolean(lb), Symbol::Boolean(rb)) => Ok(Rc::from(Symbol::Boolean(*lb || *rb))),
            _ => Err(EvaluatorError::InvalidInfixExpressionError(
                Some(TokenKind::Or),
                operator.location,
            )),
        },
        kind => Err(EvaluatorError::InvalidInfixExpressionError(
            Some(kind),
            operator.location,
        )),
    }
}

fn apply_function(
    function: &Symbol,
    arguments: &[Rc<Symbol>],
    env: &Env,
) -> Result<Rc<Symbol>, EvaluatorError> {
    match function {
        Symbol::FunctionLiteral {
            parameters,
            block: body,
        } => {
            let mut enclosing_env = Environment::new_enclosed(Rc::clone(env));
            parameters.iter().enumerate().for_each(|(i, parameter)| {
                enclosing_env.set(parameter.value.clone(), Rc::clone(&arguments[i]));
            });

            let evaluated = eval_block(body.clone(), &Rc::new(RefCell::new(enclosing_env)))?;

            Ok(Rc::clone(&(unwrap_return_value(evaluated)?)))
        }
        _ => Err(EvaluatorError::InvalidFunctionError),
    }
}

fn unwrap_return_value(symbol: Rc<Symbol>) -> Result<Rc<Symbol>, EvaluatorError> {
    if let Symbol::ReturnValue(value) = symbol.as_ref() {
        Ok(Rc::clone(value))
    } else {
        Ok(symbol)
    }
}

#[cfg(test)]
mod tests {
    use ast::Node;
    use eval;
    use lexer::Lexer;
    use parser::Parser;
    use symbol::{environment::Environment, Symbol};
    use Rc;
    use RefCell;

    fn test(input: &[&str], expected: &[Symbol]) {
        for (i, symbol) in expected.iter().enumerate() {
            let mut lexer = Lexer::new(input[i], "stdin");

            let mut parser = match Parser::new(&mut lexer) {
                Ok(parser) => parser,
                Err(e) => panic!("{}", e),
            };

            let program = match parser.parse_program() {
                Ok(program) => program,
                Err(e) => panic!("{}", e),
            };

            let env = Rc::new(RefCell::new(Environment::new()));
            let program = Node::Program(program);

            match eval(program, &env) {
                Ok(result) => assert_eq!(*result, *symbol),
                Err(e) => panic!("{}", e),
            }
        }
    }

    #[test]
    fn integer_expressions() {
        let input = vec!["5", "10"];
        let expected = vec![Symbol::Integer(5), Symbol::Integer(10)];

        test(&input, &expected);
    }

    #[test]
    fn boolean_expressions() {
        let input = vec!["false", "true"];
        let expected = vec![Symbol::Boolean(false), Symbol::Boolean(true)];

        test(&input, &expected);
    }

    #[test]
    fn string_expressions() {
        let input = vec!["\"hello\"", "\"world\""];
        let expected = vec![
            Symbol::StringLiteral("hello".to_string()),
            Symbol::StringLiteral("world".to_string()),
        ];

        test(&input, &expected);
    }

    #[test]
    fn prefix_expressions() {
        let input = vec!["!true", "!false", "-5", "-10", "-0"];
        let expected = vec![
            Symbol::Boolean(false),
            Symbol::Boolean(true),
            Symbol::Integer(-5),
            Symbol::Integer(-10),
            Symbol::Integer(0),
        ];

        test(&input, &expected);
    }

    #[test]
    fn infix_expressions() {
        let input = vec![
            "5 + 5 + 5 + 5 - 10",
            "2 * 2 * 2 * 2 * 2",
            "-50 + 100 + -50",
            "5 * 2 + 10",
            "5 + 2 * 10",
            "20 + 2 * -10",
            "50 / 2 * 2 + 10",
            "2 * (5 + 10)",
            "3 * 3 * 3 + 10",
            "3 * (3 * 3) + 10",
            "(5 + 10 * 2 + 15 / 3) * 2 + -10",
            "5 > 4 == 3 < 4;",
            "5 >= 4 == 3 <= 4;",
            "3 + 4 * 5 != 3 * 1 + 4 * 5;",
            "3 < 5 == false;",
            "true || false && true;",
            "true && false || true;",
            "\"hello \" + \"world\"",
        ];
        let expected = vec![
            Symbol::Integer(10),
            Symbol::Integer(32),
            Symbol::Integer(0),
            Symbol::Integer(20),
            Symbol::Integer(25),
            Symbol::Integer(0),
            Symbol::Integer(60),
            Symbol::Integer(30),
            Symbol::Integer(37),
            Symbol::Integer(37),
            Symbol::Integer(50),
            Symbol::Boolean(true),
            Symbol::Boolean(true),
            Symbol::Boolean(false),
            Symbol::Boolean(false),
            Symbol::Boolean(true),
            Symbol::Boolean(true),
            Symbol::StringLiteral("hello world".to_string()),
        ];

        test(&input, &expected);
    }

    #[test]
    fn conditional_expressions() {
        let input = vec![
            "if (true) { 10 };",
            "if (false) { 10 };",
            "if (true) { 10 } else { 20 };",
            "if (false) { 10 } else { 20 };",
        ];
        let expected = vec![
            Symbol::Integer(10),
            Symbol::Null,
            Symbol::Integer(10),
            Symbol::Integer(20),
        ];

        test(&input, &expected);
    }

    #[test]
    fn return_statements() {
        let input = vec![
            "return 10;",
            "return 10; 9;",
            "return 2 * 5; 9;",
            "9; return 2 * 5; 9;",
            r#"
                if (10 > 1) {
                    if (10 > 1) {
                        return 10;
                    }
                    return 1;
                }
            "#,
        ];
        let expected = vec![
            Symbol::ReturnValue(Rc::from(Symbol::Integer(10))),
            Symbol::ReturnValue(Rc::from(Symbol::Integer(10))),
            Symbol::ReturnValue(Rc::from(Symbol::Integer(10))),
            Symbol::ReturnValue(Rc::from(Symbol::Integer(10))),
            Symbol::ReturnValue(Rc::from(Symbol::Integer(10))),
        ];

        test(&input, &expected);
    }

    #[test]
    fn let_statements() {
        let input = vec![
            "let a = 5; a;",
            "let a = 5 * 5; a;",
            "let a = 5; let b = a; b;",
            "let a = 5; let b = a; let c = a + b + 5; c;",
        ];
        let expected = vec![
            Symbol::Integer(5),
            Symbol::Integer(25),
            Symbol::Integer(5),
            Symbol::Integer(15),
        ];

        test(&input, &expected);
    }

    #[test]
    fn function_calls() {
        let input = vec![
            "let identity = fn(x) { x; }; identity(5);",
            "let identity = fn(x) { return x; }; identity(5);",
            "let double = fn(x) { x * 2; }; double(5);",
            "let add = fn(x, y) { x + y; }; add(5, 5);",
            "let add = fn(x, y) { x + y; }; add(5 + 5, add(5, 5));",
            "fn(x) { x; }(5)",
        ];
        let expected = vec![
            Symbol::Integer(5),
            Symbol::Integer(5),
            Symbol::Integer(10),
            Symbol::Integer(10),
            Symbol::Integer(20),
            Symbol::Integer(5),
        ];

        test(&input, &expected);
    }

    #[test]
    fn recursive_functions() {
        let input = vec![
            "let fib = fn(x) {
				if (x <= 1) {
					x
				} else {
					fib(x - 1) + fib(x - 2);
				}
			};
            fib(10);",
            "let factorial = fn(x) {
				if (x == 0) {
					1
				} else {
					x * factorial(x - 1);
				}
			};
            factorial(5);",
        ];
        let expected = vec![Symbol::Integer(55), Symbol::Integer(120)];

        test(&input, &expected);
    }
}
