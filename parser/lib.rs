pub extern crate ast;
pub extern crate lexer;

use ast::{
	BinaryExpression, Boolean, Expression, ExpressionStatement, FunctionCall, FunctionLiteral,
	Identifier, IfExpression, IntegerLiteral, LetStatement, Program, ReturnStatement, Statement,
	StringLiteral, UnaryExpression,
};
use lexer::{token::Token, Lexer};

mod parser_tests;

const LOWEST_PRECEDENCE: u32 = 0;

pub struct Parser<'a, 'b> {
	lexer: &'b mut Lexer<'a>,

	curr_token: Token,
	peek_token: Token,
}

impl<'a, 'b> Parser<'a, 'b> {
	pub fn new(lexer: &'b mut Lexer<'a>) -> Self {
		let mut parser = Parser {
			lexer,
			curr_token: Token::EOF,
			peek_token: Token::EOF,
		};

		// set curr_token and peek_token
		parser.next_token();
		parser.next_token();

		parser
	}

	pub fn next_token(&mut self) {
		self.curr_token = self.peek_token.clone();
		self.peek_token = self.lexer.next_token();
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

	pub fn parse_program(&mut self) -> Program {
		let mut statements = Vec::new();

		while self.curr_token != Token::EOF {
			statements.push(self.parse_statement());
			self.next_token();
		}

		Program { statements }
	}

	fn parse_statement(&mut self) -> Statement {
		match self.curr_token {
			Token::LET => self.parse_let_statement(),
			Token::RETURN => self.parse_return_statement(),
			_ => self.parse_expression_statement(),
		}
	}

	fn parse_expression_statement(&mut self) -> Statement {
		let expression = self.parse_expression(LOWEST_PRECEDENCE);

		// semicolon is optional
		if self.peek_token == Token::SEMICOLON {
			self.next_token();
		};

		Statement::ExpressionStatement(ExpressionStatement { expression })
	}

	fn parse_expression(&mut self, precedence: u32) -> Expression {
		let mut left_expr = match self.curr_token {
			Token::IDENTIFIER(_) => self.parse_identifier(),
			Token::INTEGER(_) => self.parse_integer_literal(),
			Token::TRUE | Token::FALSE => self.parse_boolean_literal(),
			Token::STRING(_) => self.parse_string_literal(),
			Token::NOT | Token::MINUS => self.parse_unary_expression(),
			Token::LPAREN => self.parse_grouped_expression(),
			Token::IF => self.parse_if_expression(),
			Token::FUNCTION => self.parse_function_literal(),

			_ => panic!("invalid prefix token: {}", self.curr_token),
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
					self.next_token();
					self.parse_binary_expression(left_expr)
				}
				Token::LPAREN => {
					self.next_token();
					self.parse_call_expression(left_expr)
				}
				_ => return left_expr,
			};
		}

		left_expr
	}

	fn parse_unary_expression(&mut self) -> Expression {
		let operator = self.curr_token.clone();
		self.next_token();
		let right = self.parse_expression(self.precedence(Token::UNARY));

		Expression::UnaryExpression(UnaryExpression {
			operator,
			right: Box::new(right),
		})
	}

	fn parse_binary_expression(&mut self, left: Expression) -> Expression {
		let operator = self.curr_token.clone();
		let precedence = self.current_precedence();
		self.next_token();
		let right = self.parse_expression(precedence);

		Expression::BinaryExpression(BinaryExpression {
			left: Box::new(left),
			operator,
			right: Box::new(right),
		})
	}

	fn parse_grouped_expression(&mut self) -> Expression {
		self.next_token();

		let expr = self.parse_expression(LOWEST_PRECEDENCE);

		match self.peek_token {
			Token::RPAREN => self.next_token(),
			_ => panic!("expected RPAREN"),
		}

		expr
	}

	fn parse_if_expression(&mut self) -> Expression {
		match self.peek_token {
			Token::LPAREN => self.next_token(),
			_ => panic!("expected LPAREN"),
		}
		self.next_token();

		let condition = self.parse_expression(LOWEST_PRECEDENCE);
		match self.peek_token {
			Token::RPAREN => self.next_token(),
			_ => panic!("expected RPAREN"),
		}
		match self.peek_token {
			Token::LBRACE => self.next_token(),
			_ => panic!("expected LBRACE"),
		}

		let consequence = self.parse_block_statement();
		let mut alternative = None;

		if self.peek_token == Token::ELSE {
			self.next_token();

			match self.peek_token {
				Token::LBRACE => self.next_token(),
				_ => panic!("expected LBRACE"),
			}

			alternative = Some(self.parse_block_statement());
		}

		Expression::IfExpression(IfExpression {
			condition: Box::new(condition),
			consequence,
			alternative,
		})
	}

	fn parse_call_expression(&mut self, function: Expression) -> Expression {
		let arguments = self.parse_call_arguments();

		Expression::FunctionCall(FunctionCall {
			function: Box::new(function),
			arguments,
		})
	}

	fn parse_call_arguments(&mut self) -> Vec<Expression> {
		let mut arguments = Vec::new();

		match &self.peek_token {
			Token::RPAREN => {
				self.next_token();
				return arguments;
			}
			_ => self.next_token(),
		}

		arguments.push(self.parse_expression(LOWEST_PRECEDENCE));

		while self.peek_token == Token::COMMA {
			self.next_token();
			self.next_token();
			arguments.push(self.parse_expression(LOWEST_PRECEDENCE));
		}

		match &self.peek_token {
			Token::RPAREN => self.next_token(),
			_ => panic!("expected RPAREN"),
		}

		arguments
	}

	fn parse_let_statement(&mut self) -> Statement {
		if self.curr_token != Token::LET {
			panic!("expected LET");
		};

		let identifier = match &self.peek_token {
			Token::IDENTIFIER(a) => ast::Identifier {
				value: a.to_string(),
			},
			_ => panic!("expected IDENTIFIER"),
		};
		self.next_token();

		if self.peek_token != Token::ASSIGN {
			panic!("expected ASSIGN");
		};
		self.next_token();
		self.next_token();

		let value = self.parse_expression(LOWEST_PRECEDENCE);

		if self.peek_token != Token::SEMICOLON {
			panic!("expected SEMICOLON");
		}
		self.next_token();

		Statement::LetStatement(LetStatement { identifier, value })
	}

	fn parse_return_statement(&mut self) -> Statement {
		if self.curr_token != Token::RETURN {
			panic!("expected RETURN");
		};
		self.next_token();

		let value = self.parse_expression(LOWEST_PRECEDENCE);

		if self.peek_token != Token::SEMICOLON {
			panic!("expected SEMICOLON");
		}
		self.next_token();

		Statement::ReturnStatement(ReturnStatement { value })
	}

	fn parse_block_statement(&mut self) -> ast::BlockStatement {
		self.next_token();

		let mut statements = Vec::new();
		while self.curr_token != Token::RBRACE && self.curr_token != Token::EOF {
			statements.push(self.parse_statement());
			self.next_token();
		}

		ast::BlockStatement { statements }
	}

	fn parse_identifier(&mut self) -> Expression {
		match &self.curr_token {
			Token::IDENTIFIER(id) => Expression::Identifier(Identifier {
				value: id.to_string(),
			}),
			_ => panic!("expected IDENTIFIER"),
		}
	}

	fn parse_integer_literal(&self) -> Expression {
		match &self.curr_token {
			Token::INTEGER(i) => Expression::IntegerLiteral(IntegerLiteral { value: *i }),
			_ => panic!("expected INTEGER"),
		}
	}

	fn parse_boolean_literal(&self) -> Expression {
		match &self.curr_token {
			Token::TRUE => Expression::Boolean(Boolean { value: true }),
			Token::FALSE => Expression::Boolean(Boolean { value: false }),
			_ => panic!("expected BOOLEAN"),
		}
	}

	fn parse_string_literal(&self) -> Expression {
		match &self.curr_token {
			Token::STRING(s) => Expression::StringLiteral(StringLiteral {
				value: s.to_string(),
			}),
			_ => panic!("expected STRING"),
		}
	}

	fn parse_function_literal(&mut self) -> Expression {
		match &self.peek_token {
			Token::LPAREN => self.next_token(),
			_ => panic!("expected LPAREN"),
		}

		let parameters = self.parse_function_parameters();

		match &self.peek_token {
			Token::LBRACE => self.next_token(),
			_ => panic!("expected LBRACE"),
		}

		let body = self.parse_block_statement();

		Expression::FunctionLiteral(FunctionLiteral { parameters, body })
	}

	fn parse_function_parameters(&mut self) -> Vec<ast::Identifier> {
		let mut identifiers = Vec::new();

		match &self.peek_token {
			Token::RPAREN => {
				self.next_token();
				return identifiers;
			}
			_ => self.next_token(),
		}

		identifiers.push(ast::Identifier {
			value: match &self.curr_token {
				Token::IDENTIFIER(id) => id.to_string(),
				_ => panic!("expected IDENTIFIER"),
			},
		});

		while self.peek_token == Token::COMMA {
			self.next_token();
			self.next_token();
			identifiers.push(ast::Identifier {
				value: match &self.curr_token {
					Token::IDENTIFIER(id) => id.to_string(),
					_ => panic!("expected IDENTIFIER"),
				},
			});
		}

		match &self.peek_token {
			Token::RPAREN => self.next_token(),
			_ => panic!("expected RPAREN"),
		}

		identifiers
	}
}
