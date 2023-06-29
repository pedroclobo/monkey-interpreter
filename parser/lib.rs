pub extern crate ast;
pub extern crate lexer;

use ast::{
	BinaryExpression, BlockStatement, Boolean, Expression, ExpressionStatement, FunctionCall,
	FunctionLiteral, Identifier, IfExpression, IntegerLiteral, LetStatement, Program,
	ReturnStatement, Statement, StringLiteral, UnaryExpression,
};
use lexer::{token::Token, Lexer, LexerError};

mod parser_tests;

const LOWEST_PRECEDENCE: u32 = 0;

#[derive(Debug, PartialEq, Clone)]
pub enum ParserError {
	LexerError(LexerError),
	ExpectedError(Token, String, usize, usize),
	InvalidPrefixError(String, String, usize, usize),
}

impl std::fmt::Display for ParserError {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		match self {
			ParserError::LexerError(l) => write!(f, "{}", l.to_string()),
			ParserError::ExpectedError(expected, file, line, column) => write!(
				f,
				"{}:{}:{} - PARSER ERROR: Expected {}",
				file, line, column, expected
			),
			ParserError::InvalidPrefixError(expected, file, line, column) => write!(
				f,
				"{}:{}:{} - PARSER ERROR: Invalid prefix operator {}",
				file, line, column, expected
			),
		}
	}
}

impl From<LexerError> for ParserError {
	fn from(err: LexerError) -> Self {
		ParserError::LexerError(err)
	}
}

pub struct Parser<'a, 'b> {
	lexer: &'b mut Lexer<'a>,

	curr_token: Token,
	peek_token: Token,
}

impl<'a, 'b> Parser<'a, 'b> {
	pub fn new(lexer: &'b mut Lexer<'a>) -> Result<Self, ParserError> {
		let mut parser = Parser {
			lexer,
			curr_token: Token::EOF,
			peek_token: Token::EOF,
		};

		// set curr_token and peek_token
		let _ = parser.next_token()?;
		let _ = parser.next_token()?;

		Ok(parser)
	}

	fn next_token(&mut self) -> Result<(), ParserError> {
		self.curr_token = self.peek_token.clone();
		self.peek_token = self.lexer.next_token()?;

		Ok(())
	}

	fn expect_curr_token(&mut self, token: Token) -> Result<(), ParserError> {
		if self.curr_token == token {
			self.next_token()?;
			Ok(())
		} else {
			Err(ParserError::ExpectedError(
				token,
				self.lexer.file.to_string(),
				self.lexer.line,
				self.lexer.column,
			))
		}
	}

	fn expect_peek_token(&mut self, token: Token) -> Result<(), ParserError> {
		if self.peek_token == token {
			self.next_token()?;
			Ok(())
		} else {
			Err(ParserError::ExpectedError(
				token,
				self.lexer.file.to_string(),
				self.lexer.line,
				self.lexer.column,
			))
		}
	}

	fn precedence(&self, token: Token) -> u32 {
		match token {
			Token::OR => 1,
			Token::AND => 2,
			Token::EQ | Token::NE => 3,
			Token::LT | Token::GT | Token::LE | Token::GE => 4,
			Token::PLUS | Token::MINUS => 5,
			Token::MUL | Token::DIV => 6,
			Token::UNARY => 7,
			Token::LPAREN => 8,
			_ => LOWEST_PRECEDENCE,
		}
	}

	fn current_precedence(&self) -> u32 {
		self.precedence(self.curr_token.clone())
	}

	fn peek_precedence(&self) -> u32 {
		self.precedence(self.peek_token.clone())
	}

	pub fn parse_program(&mut self) -> Result<Program, ParserError> {
		let mut statements = Vec::new();

		while self.curr_token != Token::EOF {
			let statement = self.parse_statement()?;
			statements.push(statement);
			self.next_token()?;
		}

		Ok(Program { statements })
	}

	fn parse_statement(&mut self) -> Result<Statement, ParserError> {
		match self.curr_token {
			Token::LET => self.parse_let_statement(),
			Token::RETURN => self.parse_return_statement(),
			_ => self.parse_expression_statement(),
		}
	}

	fn parse_expression_statement(&mut self) -> Result<Statement, ParserError> {
		let expression = self.parse_expression(LOWEST_PRECEDENCE)?;

		// semicolon is optional
		if self.peek_token == Token::SEMICOLON {
			self.next_token()?;
		};

		Ok(Statement::ExpressionStatement(ExpressionStatement {
			expression,
		}))
	}

	fn parse_expression(&mut self, precedence: u32) -> Result<Expression, ParserError> {
		let mut left_expr = match &self.curr_token {
			Token::IDENTIFIER(_) => self.parse_identifier()?,
			Token::INTEGER(_) => self.parse_integer_literal()?,
			Token::TRUE | Token::FALSE => self.parse_boolean_literal()?,
			Token::STRING(_) => self.parse_string_literal()?,
			Token::NOT | Token::MINUS => self.parse_unary_expression()?,
			Token::LPAREN => self.parse_grouped_expression()?,
			Token::IF => self.parse_if_expression()?,
			Token::FUNCTION => self.parse_function_literal()?,

			tok => {
				return Err(ParserError::InvalidPrefixError(
					tok.to_string(),
					self.lexer.file.to_string(),
					self.lexer.line,
					self.lexer.column,
				))
			}
		};

		while self.peek_token != Token::SEMICOLON && precedence < self.peek_precedence() {
			left_expr = match self.peek_token {
				Token::EQ
				| Token::NE
				| Token::LT
				| Token::GT
				| Token::GE
				| Token::LE
				| Token::OR
				| Token::AND
				| Token::PLUS
				| Token::MINUS
				| Token::MUL
				| Token::DIV => {
					self.next_token()?;
					self.parse_binary_expression(left_expr)?
				}
				Token::LPAREN => {
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
		let right = self.parse_expression(self.precedence(Token::UNARY))?;

		Ok(Expression::UnaryExpression(UnaryExpression {
			operator,
			right: Box::new(right),
		}))
	}

	fn parse_binary_expression(&mut self, left: Expression) -> Result<Expression, ParserError> {
		let operator = self.curr_token.clone();
		let precedence = self.current_precedence();
		self.next_token()?;
		let right = self.parse_expression(precedence)?;

		Ok(Expression::BinaryExpression(BinaryExpression {
			left: Box::new(left),
			operator,
			right: Box::new(right),
		}))
	}

	fn parse_grouped_expression(&mut self) -> Result<Expression, ParserError> {
		self.next_token()?;

		let expr = self.parse_expression(LOWEST_PRECEDENCE);

		self.expect_peek_token(Token::RPAREN)?;

		expr
	}

	fn parse_if_expression(&mut self) -> Result<Expression, ParserError> {
		self.expect_peek_token(Token::LPAREN)?;
		self.next_token()?;

		let condition = self.parse_expression(LOWEST_PRECEDENCE)?;
		self.expect_peek_token(Token::RPAREN)?;
		self.expect_peek_token(Token::LBRACE)?;

		let consequence = self.parse_block_statement()?;
		let mut alternative = None;

		if self.peek_token == Token::ELSE {
			self.next_token()?;

			self.expect_peek_token(Token::LBRACE)?;

			alternative = Some(self.parse_block_statement()?);
		}

		Ok(Expression::IfExpression(IfExpression {
			condition: Box::new(condition),
			consequence,
			alternative,
		}))
	}

	fn parse_call_expression(&mut self, function: Expression) -> Result<Expression, ParserError> {
		let arguments = self.parse_call_arguments()?;

		Ok(Expression::FunctionCall(FunctionCall {
			function: Box::new(function),
			arguments,
		}))
	}

	fn parse_call_arguments(&mut self) -> Result<Vec<Expression>, ParserError> {
		let mut arguments = Vec::new();

		if self.peek_token == Token::RPAREN {
			self.next_token()?;
			return Ok(arguments);
		} else {
			self.next_token()?;
		}

		arguments.push(self.parse_expression(LOWEST_PRECEDENCE)?);

		while self.peek_token == Token::COMMA {
			self.next_token()?;
			self.next_token()?;
			arguments.push(self.parse_expression(LOWEST_PRECEDENCE)?);
		}

		self.expect_peek_token(Token::RPAREN)?;

		Ok(arguments)
	}

	fn parse_let_statement(&mut self) -> Result<Statement, ParserError> {
		let identifier = match &self.peek_token {
			Token::IDENTIFIER(a) => ast::Identifier {
				value: a.to_string(),
			},
			_ => Err(ParserError::ExpectedError(
				Token::IDENTIFIER("id".to_string()),
				self.lexer.file.to_string(),
				self.lexer.line,
				self.lexer.column,
			))?,
		};
		self.next_token()?; // curr = identifier

		self.expect_peek_token(Token::ASSIGN)?;
		self.next_token()?; // curr = assign

		let value = self.parse_expression(LOWEST_PRECEDENCE)?;

		self.expect_peek_token(Token::SEMICOLON)?;

		Ok(Statement::LetStatement(LetStatement { identifier, value }))
	}

	fn parse_return_statement(&mut self) -> Result<Statement, ParserError> {
		self.expect_curr_token(Token::RETURN)?;

		let value = self.parse_expression(LOWEST_PRECEDENCE)?;

		self.expect_peek_token(Token::SEMICOLON)?;

		Ok(Statement::ReturnStatement(ReturnStatement { value }))
	}

	fn parse_block_statement(&mut self) -> Result<BlockStatement, ParserError> {
		self.next_token()?;

		let mut statements = Vec::new();
		while self.curr_token != Token::RBRACE && self.curr_token != Token::EOF {
			statements.push(self.parse_statement()?);
			self.next_token()?;
		}

		Ok(BlockStatement { statements })
	}

	fn parse_identifier(&mut self) -> Result<Expression, ParserError> {
		match &self.curr_token {
			Token::IDENTIFIER(id) => Ok(Expression::Identifier(Identifier {
				value: id.to_string(),
			})),
			_ => Err(ParserError::ExpectedError(
				Token::IDENTIFIER("id".to_string()),
				self.lexer.file.to_string(),
				self.lexer.line,
				self.lexer.column,
			))?,
		}
	}

	fn parse_integer_literal(&self) -> Result<Expression, ParserError> {
		match &self.curr_token {
			Token::INTEGER(i) => Ok(Expression::IntegerLiteral(IntegerLiteral { value: *i })),
			_ => Err(ParserError::ExpectedError(
				Token::INTEGER(0),
				self.lexer.file.to_string(),
				self.lexer.line,
				self.lexer.column,
			))?,
		}
	}

	fn parse_boolean_literal(&self) -> Result<Expression, ParserError> {
		match &self.curr_token {
			Token::TRUE => Ok(Expression::Boolean(Boolean { value: true })),
			Token::FALSE => Ok(Expression::Boolean(Boolean { value: false })),
			_ => Err(ParserError::ExpectedError(
				Token::FALSE,
				self.lexer.file.to_string(),
				self.lexer.line,
				self.lexer.column,
			))?,
		}
	}

	fn parse_string_literal(&self) -> Result<Expression, ParserError> {
		match &self.curr_token {
			Token::STRING(s) => Ok(Expression::StringLiteral(StringLiteral {
				value: s.to_string(),
			})),
			_ => Err(ParserError::ExpectedError(
				Token::STRING("".to_string()),
				self.lexer.file.to_string(),
				self.lexer.line,
				self.lexer.column,
			))?,
		}
	}

	fn parse_function_literal(&mut self) -> Result<Expression, ParserError> {
		self.expect_peek_token(Token::LPAREN)?;

		let parameters = self.parse_function_parameters()?;

		self.expect_peek_token(Token::LBRACE)?;

		let body = self.parse_block_statement()?;

		Ok(Expression::FunctionLiteral(FunctionLiteral {
			parameters,
			body,
		}))
	}

	fn parse_function_parameters(&mut self) -> Result<Vec<Identifier>, ParserError> {
		let mut identifiers = Vec::new();

		match &self.peek_token {
			Token::RPAREN => {
				self.next_token()?;
				return Ok(identifiers);
			}
			_ => self.next_token(),
		}?;

		identifiers.push(ast::Identifier {
			value: match &self.curr_token {
				Token::IDENTIFIER(id) => id.to_string(),
				_ => Err(ParserError::ExpectedError(
					Token::IDENTIFIER("id".to_string()),
					self.lexer.file.to_string(),
					self.lexer.line,
					self.lexer.column,
				))?,
			},
		});

		while self.peek_token == Token::COMMA {
			self.next_token()?;
			self.next_token()?;
			identifiers.push(ast::Identifier {
				value: match &self.curr_token {
					Token::IDENTIFIER(id) => id.to_string(),
					_ => Err(ParserError::ExpectedError(
						Token::STRING("".to_string()),
						self.lexer.file.to_string(),
						self.lexer.line,
						self.lexer.column,
					))?,
				},
			});
		}

		self.expect_peek_token(Token::RPAREN)?;

		Ok(identifiers)
	}
}
