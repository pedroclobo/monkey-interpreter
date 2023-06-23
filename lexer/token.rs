#[derive(Debug, PartialEq)]
pub enum Token {
	ILLEGAL(String),

	EOF,

	IDENTIFIER(String),

	INTEGER(i32),
	STRING(String),

	ASSIGN,
	PLUS,
	MINUS,
	MUL,
	DIV,
	LT,
	GT,
	GE,
	LE,
	EQ,
	NE,
	AND,
	OR,
	NOT,

	COMMA,
	SEMICOLON,
	LPAREN,
	RPAREN,
	LBRACE,
	RBRACE,
	LBRACKET,
	RBRACKET,

	FUNCTION,
	LET,
	TRUE,
	FALSE,
	IF,
	ELSE,
	RETURN,
}

impl Token {
	pub fn lookup_identifier(identifier: &str) -> Token {
		match identifier {
			"fn" => Token::FUNCTION,
			"let" => Token::LET,
			"true" => Token::TRUE,
			"false" => Token::FALSE,
			"if" => Token::IF,
			"else" => Token::ELSE,
			"return" => Token::RETURN,
			_ => Token::IDENTIFIER(identifier.to_string()),
		}
	}
}

impl std::fmt::Display for Token {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			Token::ILLEGAL(s) => write!(f, "ERROR: Invalid Lexeme ({})", s),
			Token::EOF => write!(f, "tEOF"),
			Token::IDENTIFIER(id) => write!(f, "tIDENTIFIER({})", id),
			Token::INTEGER(i) => write!(f, "tINTEGER({})", i),
			Token::STRING(s) => write!(f, "tSTRING({})", s),
			Token::ASSIGN => write!(f, "tASSIGN"),
			Token::PLUS => write!(f, "tPLUS"),
			Token::MINUS => write!(f, "tMINUS"),
			Token::MUL => write!(f, "tMUL"),
			Token::DIV => write!(f, "tDIV"),
			Token::LT => write!(f, "tLT"),
			Token::GT => write!(f, "tGT"),
			Token::GE => write!(f, "tGE"),
			Token::LE => write!(f, "tLE"),
			Token::EQ => write!(f, "tEQ"),
			Token::NE => write!(f, "tNE"),
			Token::AND => write!(f, "tAND"),
			Token::OR => write!(f, "tOR"),
			Token::NOT => write!(f, "tNOT"),
			Token::COMMA => write!(f, "tCOMMA"),
			Token::SEMICOLON => write!(f, "tSEMICOLON"),
			Token::LPAREN => write!(f, "tLPAREN"),
			Token::RPAREN => write!(f, "tRPAREN"),
			Token::LBRACE => write!(f, "tLBRACE"),
			Token::RBRACE => write!(f, "tRBRACE"),
			Token::LBRACKET => write!(f, "tLBRACKET"),
			Token::RBRACKET => write!(f, "tRBRACKET"),
			Token::FUNCTION => write!(f, "tFUNCTION"),
			Token::LET => write!(f, "tLET"),
			Token::TRUE => write!(f, "tTRUE"),
			Token::FALSE => write!(f, "tFALSE"),
			Token::IF => write!(f, "tIF"),
			Token::ELSE => write!(f, "tELSE"),
			Token::RETURN => write!(f, "tRETURN"),
		}
	}
}
