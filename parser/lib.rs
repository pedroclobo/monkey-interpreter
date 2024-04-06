pub extern crate lexer;

pub mod ast;

use ast::{Expression, Identifier, Program, Statement};
use lexer::{Lexer, Token, TokenKind};

mod error;
mod tests;

use error::ParserError;

const LOWEST_PRECEDENCE: u32 = 0;

pub struct Parser<'a, 'b> {
    lexer: &'b mut Lexer<'a>,

    curr_token: Token,
    peek_token: Token,
}

impl<'a, 'b> Parser<'a, 'b> {
    pub fn new(lexer: &'b mut Lexer<'a>) -> Result<Self, ParserError> {
        let mut parser = Parser {
            lexer,
            curr_token: Token::default(),
            peek_token: Token::default(),
        };

        // set curr_token and peek_token
        parser.next_token()?;
        parser.next_token()?;

        Ok(parser)
    }

    fn next_token(&mut self) -> Result<(), ParserError> {
        self.curr_token = self.peek_token.clone();
        self.peek_token = self.lexer.next_token()?;

        Ok(())
    }

    fn expect_curr_token(&mut self, kind: TokenKind) -> Result<(), ParserError> {
        if self.curr_token.kind == kind {
            self.next_token()?;
            Ok(())
        } else {
            Err(ParserError::Expected(
                kind,
                self.curr_token.location.clone(),
            ))
        }
    }

    fn expect_peek_token(&mut self, kind: TokenKind) -> Result<(), ParserError> {
        if self.peek_token.kind == kind {
            self.next_token()?;
            Ok(())
        } else {
            Err(ParserError::Expected(
                kind,
                self.peek_token.location.clone(),
            ))
        }
    }

    fn precedence(&self, kind: TokenKind) -> u32 {
        match kind {
            TokenKind::OR => 1,
            TokenKind::AND => 2,
            TokenKind::EQ | TokenKind::NE => 3,
            TokenKind::LT | TokenKind::GT | TokenKind::LE | TokenKind::GE => 4,
            TokenKind::PLUS | TokenKind::MINUS => 5,
            TokenKind::MUL | TokenKind::DIV => 6,
            TokenKind::UNARY => 7,
            TokenKind::LPAREN => 8,
            _ => LOWEST_PRECEDENCE,
        }
    }

    fn current_precedence(&self) -> u32 {
        self.precedence(self.curr_token.kind.clone())
    }

    fn peek_precedence(&self) -> u32 {
        self.precedence(self.peek_token.kind.clone())
    }

    pub fn parse_program(&mut self) -> Result<Program, ParserError> {
        let mut statements = Vec::new();

        while self.curr_token.kind != TokenKind::EOF {
            let statement = self.parse_statement()?;
            statements.push(statement);
            self.next_token()?;
        }

        Ok(Program { statements })
    }

    fn parse_statement(&mut self) -> Result<Statement, ParserError> {
        match self.curr_token.kind {
            TokenKind::LET => self.parse_let_statement(),
            TokenKind::RETURN => self.parse_return_statement(),
            _ => self.parse_expression_statement(),
        }
    }

    fn parse_expression_statement(&mut self) -> Result<Statement, ParserError> {
        let expression = self.parse_expression(LOWEST_PRECEDENCE)?;

        // semicolon is optional
        if self.peek_token.kind == TokenKind::SEMICOLON {
            self.next_token()?;
        };

        Ok(Statement::Expression { expression })
    }

    fn parse_expression(&mut self, precedence: u32) -> Result<Expression, ParserError> {
        let mut left_expr = match &self.curr_token.kind {
            TokenKind::IDENTIFIER(_) => self.parse_identifier()?,
            TokenKind::INTEGER(_) => self.parse_integer_literal()?,
            TokenKind::TRUE | TokenKind::FALSE => self.parse_boolean_literal()?,
            TokenKind::STRING(_) => self.parse_string_literal()?,
            TokenKind::NOT | TokenKind::MINUS => self.parse_unary_expression()?,
            TokenKind::LPAREN => self.parse_grouped_expression()?,
            TokenKind::IF => self.parse_if_expression()?,
            TokenKind::FUNCTION => self.parse_function_literal()?,

            tok => {
                return Err(ParserError::InvalidPrefix(
                    tok.clone(),
                    self.curr_token.location.clone(),
                ))
            }
        };

        while self.peek_token.kind != TokenKind::SEMICOLON && precedence < self.peek_precedence() {
            left_expr = match self.peek_token.kind {
                TokenKind::EQ
                | TokenKind::NE
                | TokenKind::LT
                | TokenKind::GT
                | TokenKind::GE
                | TokenKind::LE
                | TokenKind::OR
                | TokenKind::AND
                | TokenKind::PLUS
                | TokenKind::MINUS
                | TokenKind::MUL
                | TokenKind::DIV => {
                    self.next_token()?;
                    self.parse_binary_expression(left_expr)?
                }
                TokenKind::LPAREN => {
                    self.next_token()?;
                    self.parse_call_expression(left_expr)?
                }
                _ => return Ok(left_expr),
            };
        }

        Ok(left_expr)
    }

    fn parse_unary_expression(&mut self) -> Result<Expression, ParserError> {
        let operator = self.curr_token.clone();
        self.next_token()?;
        let operand = self.parse_expression(self.precedence(TokenKind::UNARY))?;

        Ok(Expression::Unary {
            operator,
            operand: Box::new(operand),
        })
    }

    fn parse_binary_expression(&mut self, left: Expression) -> Result<Expression, ParserError> {
        let operator = self.curr_token.clone();
        let precedence = self.current_precedence();
        self.next_token()?;
        let right = self.parse_expression(precedence)?;

        Ok(Expression::Binary {
            left: Box::new(left),
            operator,
            right: Box::new(right),
        })
    }

    fn parse_grouped_expression(&mut self) -> Result<Expression, ParserError> {
        self.next_token()?;

        let expr = self.parse_expression(LOWEST_PRECEDENCE);

        self.expect_peek_token(TokenKind::RPAREN)?;

        expr
    }

    fn parse_if_expression(&mut self) -> Result<Expression, ParserError> {
        self.expect_peek_token(TokenKind::LPAREN)?;
        self.next_token()?;

        let condition = self.parse_expression(LOWEST_PRECEDENCE)?;
        self.expect_peek_token(TokenKind::RPAREN)?;
        self.expect_peek_token(TokenKind::LBRACE)?;

        let consequence = self.parse_block_statement()?;
        let mut alternative = None;

        if self.peek_token.kind == TokenKind::ELSE {
            self.next_token()?;

            self.expect_peek_token(TokenKind::LBRACE)?;

            alternative = Some(self.parse_block_statement()?);
        }

        Ok(Expression::If {
            condition: Box::new(condition),
            consequence,
            alternative,
        })
    }

    fn parse_call_expression(&mut self, function: Expression) -> Result<Expression, ParserError> {
        let arguments = self.parse_call_arguments()?;

        Ok(Expression::FunctionCall {
            function: Box::new(function),
            arguments,
        })
    }

    fn parse_call_arguments(&mut self) -> Result<Vec<Expression>, ParserError> {
        let mut arguments = Vec::new();

        if self.peek_token.kind == TokenKind::RPAREN {
            self.next_token()?;
            return Ok(arguments);
        } else {
            self.next_token()?;
        }

        arguments.push(self.parse_expression(LOWEST_PRECEDENCE)?);

        while self.peek_token.kind == TokenKind::COMMA {
            self.next_token()?;
            self.next_token()?;
            arguments.push(self.parse_expression(LOWEST_PRECEDENCE)?);
        }

        self.expect_peek_token(TokenKind::RPAREN)?;

        Ok(arguments)
    }

    fn parse_let_statement(&mut self) -> Result<Statement, ParserError> {
        let identifier = match &self.peek_token.kind {
            TokenKind::IDENTIFIER(a) => ast::Identifier {
                value: a.to_string(),
            },
            _ => Err(ParserError::Expected(
                TokenKind::IDENTIFIER("id".to_string()),
                self.peek_token.location.clone(),
            ))?,
        };
        self.next_token()?; // curr = identifier

        self.expect_peek_token(TokenKind::ASSIGN)?;
        self.next_token()?; // curr = assign

        let expression = self.parse_expression(LOWEST_PRECEDENCE)?;

        self.expect_peek_token(TokenKind::SEMICOLON)?;

        Ok(Statement::Let {
            identifier,
            expression,
        })
    }

    fn parse_return_statement(&mut self) -> Result<Statement, ParserError> {
        self.expect_curr_token(TokenKind::RETURN)?;

        let value = self.parse_expression(LOWEST_PRECEDENCE)?;

        self.expect_peek_token(TokenKind::SEMICOLON)?;

        Ok(Statement::Return { value })
    }

    fn parse_block_statement(&mut self) -> Result<Program, ParserError> {
        self.next_token()?;

        let mut statements = Vec::new();
        while self.curr_token.kind != TokenKind::RBRACE && self.curr_token.kind != TokenKind::EOF {
            statements.push(self.parse_statement()?);
            self.next_token()?;
        }

        Ok(Program { statements })
    }

    fn parse_identifier(&mut self) -> Result<Expression, ParserError> {
        match &self.curr_token.kind {
            TokenKind::IDENTIFIER(id) => Ok(Expression::Identifier(Identifier {
                value: id.to_string(),
            })),
            _ => Err(ParserError::Expected(
                TokenKind::IDENTIFIER("id".to_string()),
                self.curr_token.location.clone(),
            ))?,
        }
    }

    fn parse_integer_literal(&self) -> Result<Expression, ParserError> {
        match self.curr_token.kind {
            TokenKind::INTEGER(i) => Ok(Expression::IntegerLiteral { value: i }),
            _ => Err(ParserError::Expected(
                TokenKind::INTEGER(0),
                self.curr_token.location.clone(),
            ))?,
        }
    }

    fn parse_boolean_literal(&self) -> Result<Expression, ParserError> {
        match self.curr_token.kind {
            TokenKind::TRUE => Ok(Expression::BooleanLiteral { value: true }),
            TokenKind::FALSE => Ok(Expression::BooleanLiteral { value: false }),
            _ => Err(ParserError::Expected(
                TokenKind::FALSE,
                self.curr_token.location.clone(),
            ))?,
        }
    }

    fn parse_string_literal(&self) -> Result<Expression, ParserError> {
        match &self.curr_token.kind {
            TokenKind::STRING(s) => Ok(Expression::StringLiteral {
                value: s.to_string(),
            }),
            _ => Err(ParserError::Expected(
                TokenKind::STRING("".to_string()),
                self.curr_token.location.clone(),
            ))?,
        }
    }

    fn parse_function_literal(&mut self) -> Result<Expression, ParserError> {
        self.expect_peek_token(TokenKind::LPAREN)?;

        let parameters = self.parse_function_parameters()?;

        self.expect_peek_token(TokenKind::LBRACE)?;

        let body = self.parse_block_statement()?;

        Ok(Expression::FunctionLiteral { parameters, body })
    }

    fn parse_function_parameters(&mut self) -> Result<Vec<Identifier>, ParserError> {
        let mut identifiers = Vec::new();

        match &self.peek_token.kind {
            TokenKind::RPAREN => {
                self.next_token()?;
                return Ok(identifiers);
            }
            _ => self.next_token(),
        }?;

        identifiers.push(ast::Identifier {
            value: match &self.curr_token.kind {
                TokenKind::IDENTIFIER(id) => id.to_string(),
                _ => Err(ParserError::Expected(
                    TokenKind::IDENTIFIER("id".to_string()),
                    self.curr_token.location.clone(),
                ))?,
            },
        });

        while self.peek_token.kind == TokenKind::COMMA {
            self.next_token()?;
            self.next_token()?;
            identifiers.push(ast::Identifier {
                value: match &self.curr_token.kind {
                    TokenKind::IDENTIFIER(id) => id.to_string(),
                    _ => Err(ParserError::Expected(
                        TokenKind::STRING("".to_string()),
                        self.curr_token.location.clone(),
                    ))?,
                },
            });
        }

        self.expect_peek_token(TokenKind::RPAREN)?;

        Ok(identifiers)
    }
}
