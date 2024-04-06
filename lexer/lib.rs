pub use crate::error::LexerError;
pub use crate::location::Location;
pub use crate::token::{Token, TokenKind};

pub mod error;
mod location;
pub mod token;
pub struct Lexer<'a> {
    pub location: Location,
    input: &'a str,
    position: usize,
    read_position: usize,
    char: u8,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str, file: &'a str) -> Self {
        let mut lex = Lexer {
            location: Location::default(),
            input,
            position: 0,
            read_position: 0,
            char: 0,
        };

        lex.location.file = file.to_string();
        lex.read_char();

        lex
    }

    pub fn next_token(&mut self) -> Result<Token, LexerError> {
        let mut token = Token {
            location: self.location.clone(),
            ..Default::default()
        };

        match self.char {
            b'=' => {
                if self.peek_char() == b'=' {
                    self.read_char();
                    self.read_char();
                    token.kind = TokenKind::EQ;
                    Ok(token)
                } else {
                    self.read_char();
                    token.kind = TokenKind::ASSIGN;
                    Ok(token)
                }
            }
            b'!' => {
                if self.peek_char() == b'=' {
                    self.read_char();
                    self.read_char();
                    token.kind = TokenKind::NE;
                    Ok(token)
                } else {
                    self.read_char();
                    token.kind = TokenKind::NOT;
                    Ok(token)
                }
            }

            b'+' => {
                self.read_char();
                token.kind = TokenKind::PLUS;
                Ok(token)
            }
            b'-' => {
                self.read_char();
                token.kind = TokenKind::MINUS;
                Ok(token)
            }
            b'*' => {
                self.read_char();
                token.kind = TokenKind::MUL;
                Ok(token)
            }
            b'/' => {
                self.read_char();
                token.kind = TokenKind::DIV;
                Ok(token)
            }
            b'<' => {
                if self.peek_char() == b'=' {
                    self.read_char();
                    self.read_char();
                    token.kind = TokenKind::LE;
                    Ok(token)
                } else {
                    self.read_char();
                    token.kind = TokenKind::LT;
                    Ok(token)
                }
            }
            b'>' => {
                if self.peek_char() == b'=' {
                    self.read_char();
                    self.read_char();
                    token.kind = TokenKind::GE;
                    Ok(token)
                } else {
                    self.read_char();
                    token.kind = TokenKind::GT;
                    Ok(token)
                }
            }
            b'&' => match self.peek_char() {
                b'&' => {
                    self.read_char();
                    self.read_char();
                    token.kind = TokenKind::AND;
                    Ok(token)
                }
                lex => Err(LexerError::InvalidTokenSequenceError(
                    format!("&{}", lex as char),
                    token.location.clone(),
                )),
            },
            b'|' => match self.peek_char() {
                b'|' => {
                    self.read_char();
                    self.read_char();
                    token.kind = TokenKind::OR;
                    Ok(token)
                }
                lex => Err(LexerError::InvalidTokenSequenceError(
                    format!("|{}", lex as char),
                    token.location.clone(),
                )),
            },

            b',' => {
                self.read_char();
                token.kind = TokenKind::COMMA;
                Ok(token)
            }
            b';' => {
                self.read_char();
                token.kind = TokenKind::SEMICOLON;
                Ok(token)
            }
            b'(' => {
                self.read_char();
                token.kind = TokenKind::LPAREN;
                Ok(token)
            }
            b')' => {
                self.read_char();
                token.kind = TokenKind::RPAREN;
                Ok(token)
            }
            b'{' => {
                self.read_char();
                token.kind = TokenKind::LBRACE;
                Ok(token)
            }
            b'}' => {
                self.read_char();
                token.kind = TokenKind::RBRACE;
                Ok(token)
            }
            b'[' => {
                self.read_char();
                token.kind = TokenKind::LBRACKET;
                Ok(token)
            }
            b']' => {
                self.read_char();
                token.kind = TokenKind::RBRACKET;
                Ok(token)
            }

            b'0'..=b'9' => self.read_integer(),

            b'A'..=b'Z' | b'a'..=b'z' => Ok(self.read_identifier()),

            b'"' => Ok(self.read_string()),

            b'\n' | b'\t' | b' ' | b'\r' => {
                self.read_char();
                self.next_token()
            }

            0 => {
                token.kind = TokenKind::EOF;
                Ok(token)
            }

            lex => Err(LexerError::InvalidTokenError(
                format!("{}", lex as char),
                token.location.clone(),
            )),
        }
    }

    fn read_char(&mut self) {
        if self.read_position >= self.input.len() {
            self.char = 0;
        } else {
            self.char = self.input.as_bytes()[self.read_position];
        }

        if self.char == b'\n' {
            self.location.line += 1;
            self.location.column = 0;
        } else {
            self.location.column += 1;
        }

        self.position = self.read_position;
        self.read_position += 1;
    }

    fn peek_char(&self) -> u8 {
        if self.read_position >= self.input.len() {
            0
        } else {
            self.input.as_bytes()[self.read_position]
        }
    }

    fn read_identifier(&mut self) -> Token {
        let mut token = Token {
            location: self.location.clone(),
            ..Default::default()
        };

        let position = self.position;
        while self.char.is_ascii_alphabetic() || self.char == b'_' {
            self.read_char();
        }

        let identifier = &self.input[position..self.position];
        token.kind = match identifier {
            "fn" => TokenKind::FUNCTION,
            "let" => TokenKind::LET,
            "true" => TokenKind::TRUE,
            "false" => TokenKind::FALSE,
            "if" => TokenKind::IF,
            "else" => TokenKind::ELSE,
            "return" => TokenKind::RETURN,
            _ => TokenKind::IDENTIFIER(identifier.to_string()),
        };

        token
    }

    fn read_integer(&mut self) -> Result<Token, LexerError> {
        let mut token = Token {
            location: self.location.clone(),
            ..Default::default()
        };

        let position = self.position;
        while self.char.is_ascii_digit() {
            self.read_char();
        }

        match &self.input[position..self.position].parse::<i32>() {
            Ok(integer) => {
                token.kind = TokenKind::INTEGER(*integer);
                Ok(token)
            }
            Err(_) => Err(LexerError::InvalidIntegerError(
                self.input[position..self.position].to_string(),
                token.location.clone(),
            )),
        }
    }

    fn read_string(&mut self) -> Token {
        let mut token = Token {
            location: self.location.clone(),
            ..Default::default()
        };

        self.read_char(); // read the opening "

        let position = self.position;
        while self.char != b'"' && self.char != 0 {
            self.read_char();
        }

        self.read_char(); // read the closing "

        let string = &self.input[position..self.position - 1];
        token.kind = TokenKind::STRING(string.to_string());
        token
    }
}

#[cfg(test)]
mod tests {
    use Lexer;
    use LexerError;
    use LexerError::*;
    use Location;
    use Token;
    use TokenKind::*;

    macro_rules! token {
        ($kind:expr, $line:expr, $column:expr) => {
            Token {
                kind: $kind,
                location: Location {
                    file: "test".to_string(),
                    line: $line,
                    column: $column,
                },
            }
        };
    }

    macro_rules! error {
        ($kind:ident, $lexeme:expr, $line:expr, $column:expr) => {
            $kind(
                $lexeme.to_string(),
                Location {
                    file: "test".to_string(),
                    line: $line,
                    column: $column,
                },
            )
        };
    }

    fn test(input: &str, expected: &Vec<Token>) {
        let mut lexer = Lexer::new(input, "test");

        for expected_token in expected {
            let token = lexer.next_token();
            assert_eq!(token.unwrap(), *expected_token);
        }
    }

    fn test_error(input: &[&str], expected: &[Vec<Result<Token, LexerError>>]) {
        for (input, expected) in input.iter().zip(expected.iter()) {
            let mut lexer = Lexer::new(input, "test");

            let mut i = 0;
            while let Ok(tok) = lexer.next_token() {
                assert_eq!(tok, *expected[i].as_ref().unwrap());
                i += 1;
            }
        }
    }

    #[test]
    fn single_char_tokens() {
        let input = "=+-*/<>!,;(){}[]";

        let expected = vec![
            token!(ASSIGN, 1, 1),
            token!(PLUS, 1, 2),
            token!(MINUS, 1, 3),
            token!(MUL, 1, 4),
            token!(DIV, 1, 5),
            token!(LT, 1, 6),
            token!(GT, 1, 7),
            token!(NOT, 1, 8),
            token!(COMMA, 1, 9),
            token!(SEMICOLON, 1, 10),
            token!(LPAREN, 1, 11),
            token!(RPAREN, 1, 12),
            token!(LBRACE, 1, 13),
            token!(RBRACE, 1, 14),
            token!(LBRACKET, 1, 15),
            token!(RBRACKET, 1, 16),
        ];

        test(input, &expected);
    }

    #[test]
    fn double_char_tokens() {
        let input = "== != <= >= && ||";

        let expected = vec![
            token!(EQ, 1, 1),
            token!(NE, 1, 4),
            token!(LE, 1, 7),
            token!(GE, 1, 10),
            token!(AND, 1, 13),
            token!(OR, 1, 16),
        ];

        test(input, &expected);
    }

    #[test]
    fn identifiers() {
        let input = "five ten add";

        let expected = vec![
            token!(IDENTIFIER("five".to_string()), 1, 1),
            token!(IDENTIFIER("ten".to_string()), 1, 6),
            token!(IDENTIFIER("add".to_string()), 1, 10),
        ];

        test(input, &expected);
    }

    #[test]
    fn keywords() {
        let input = "fn let true false if else return";

        let expected = vec![
            token!(FUNCTION, 1, 1),
            token!(LET, 1, 4),
            token!(TRUE, 1, 8),
            token!(FALSE, 1, 13),
            token!(IF, 1, 19),
            token!(ELSE, 1, 22),
            token!(RETURN, 1, 27),
        ];

        test(input, &expected);
    }

    #[test]
    fn integers() {
        let input = "5 10 100 9999";

        let expected = vec![
            token!(INTEGER(5), 1, 1),
            token!(INTEGER(10), 1, 3),
            token!(INTEGER(100), 1, 6),
            token!(INTEGER(9999), 1, 10),
        ];

        test(input, &expected);
    }

    #[test]
    fn strings() {
        let input = "\"hello\" \"world\"";

        let expected = vec![
            token!(STRING("hello".to_string()), 1, 1),
            token!(STRING("world".to_string()), 1, 9),
        ];

        test(input, &expected);
    }

    #[test]
    fn invalid_token() {
        let inputs = vec!["let a = %", "x = `"];

        let expected = vec![
            vec![
                Ok(token!(LET, 1, 1)),
                Ok(token!(IDENTIFIER("a".to_string()), 1, 5)),
                Ok(token!(ASSIGN, 1, 7)),
                Err(error!(InvalidTokenError, "%", 1, 9)),
            ],
            vec![
                Ok(token!(IDENTIFIER("x".to_string()), 1, 1)),
                Ok(token!(ASSIGN, 1, 3)),
                Err(error!(InvalidTokenError, "`", 1, 5)),
            ],
        ];

        test_error(&inputs, &expected);
    }

    #[test]
    fn invalid_token_sequence() {
        let inputs = vec!["false &| true", "let a = |-"];

        let expected = vec![
            vec![
                Ok(token!(FALSE, 1, 1)),
                Err(error!(InvalidTokenSequenceError, "&|", 1, 7)),
            ],
            vec![
                Ok(token!(LET, 1, 1)),
                Ok(token!(IDENTIFIER("a".to_string()), 1, 5)),
                Ok(token!(ASSIGN, 1, 7)),
                Err(error!(InvalidTokenSequenceError, "|-", 1, 9)),
            ],
        ];

        test_error(&inputs, &expected);
    }
}
