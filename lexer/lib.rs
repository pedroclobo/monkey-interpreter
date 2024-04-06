pub use crate::error::LexerError;
pub use crate::location::Location;
pub use crate::token::{Token, TokenKind};

pub mod error;
pub mod location;
pub mod token;

pub struct Lexer<'a> {
    /// File location information
    location: Location,
    /// Input source code
    input: &'a str,
    /// Index into `input`
    position: usize,
    /// Next index of `input` to be read
    read_position: usize,
    /// Current character, indexed by `position`
    char: u8,
}

impl<'a> Lexer<'a> {
    /// Create new lexer from `input` source code and `file` filename
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

    /// Get the next token, if possible
    pub fn next_token(&mut self) -> Result<Token, LexerError> {
        let mut token = Token {
            location: self.location.clone(),
            ..Default::default()
        };

        match self.char {
            b'=' => {
                if let Some(b'=') = self.peek_char() {
                    self.read_char();
                    self.read_char();
                    token.kind = TokenKind::Equal;
                    Ok(token)
                } else {
                    self.read_char();
                    token.kind = TokenKind::Assign;
                    Ok(token)
                }
            }
            b'!' => {
                if let Some(b'=') = self.peek_char() {
                    self.read_char();
                    self.read_char();
                    token.kind = TokenKind::NotEqual;
                    Ok(token)
                } else {
                    self.read_char();
                    token.kind = TokenKind::Not;
                    Ok(token)
                }
            }

            b'+' => {
                self.read_char();
                token.kind = TokenKind::Plus;
                Ok(token)
            }
            b'-' => {
                self.read_char();
                token.kind = TokenKind::Minus;
                Ok(token)
            }
            b'*' => {
                self.read_char();
                token.kind = TokenKind::Multiplication;
                Ok(token)
            }
            b'/' => {
                self.read_char();
                token.kind = TokenKind::Division;
                Ok(token)
            }
            b'<' => {
                if let Some(b'=') = self.peek_char() {
                    self.read_char();
                    self.read_char();
                    token.kind = TokenKind::LessThanOrEqual;
                    Ok(token)
                } else {
                    self.read_char();
                    token.kind = TokenKind::LessThan;
                    Ok(token)
                }
            }
            b'>' => {
                if let Some(b'=') = self.peek_char() {
                    self.read_char();
                    self.read_char();
                    token.kind = TokenKind::GreaterThanOrEqual;
                    Ok(token)
                } else {
                    self.read_char();
                    token.kind = TokenKind::GreaterThan;
                    Ok(token)
                }
            }
            b'&' => match self.peek_char() {
                Some(b'&') => {
                    self.read_char();
                    self.read_char();
                    token.kind = TokenKind::And;
                    Ok(token)
                }
                Some(lex) => Err(LexerError::InvalidTokenSequence(
                    format!("&{}", lex as char),
                    token.location.clone(),
                )),
                None => Err(LexerError::NoMoreTokens(token.location.clone())),
            },
            b'|' => match self.peek_char() {
                Some(b'|') => {
                    self.read_char();
                    self.read_char();
                    token.kind = TokenKind::Or;
                    Ok(token)
                }
                Some(lex) => Err(LexerError::InvalidTokenSequence(
                    format!("&{}", lex as char),
                    token.location.clone(),
                )),
                None => Err(LexerError::NoMoreTokens(token.location.clone())),
            },

            b',' => {
                self.read_char();
                token.kind = TokenKind::Comma;
                Ok(token)
            }
            b';' => {
                self.read_char();
                token.kind = TokenKind::Semicolon;
                Ok(token)
            }
            b'(' => {
                self.read_char();
                token.kind = TokenKind::LeftParenthesis;
                Ok(token)
            }
            b')' => {
                self.read_char();
                token.kind = TokenKind::RightParenthesis;
                Ok(token)
            }
            b'{' => {
                self.read_char();
                token.kind = TokenKind::LeftBrace;
                Ok(token)
            }
            b'}' => {
                self.read_char();
                token.kind = TokenKind::RightBrace;
                Ok(token)
            }
            b'[' => {
                self.read_char();
                token.kind = TokenKind::LeftBracket;
                Ok(token)
            }
            b']' => {
                self.read_char();
                token.kind = TokenKind::RightBracket;
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
                token.kind = TokenKind::Eof;
                Ok(token)
            }

            lex => Err(LexerError::InvalidToken(
                format!("{}", lex as char),
                token.location.clone(),
            )),
        }
    }

    fn read_char(&mut self) {
        let next = self.peek_char();
        match next {
            Some(c) => self.char = c,
            None => self.char = 0,
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

    fn peek_char(&self) -> Option<u8> {
        self.input.as_bytes().get(self.read_position).copied()
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
            "fn" => TokenKind::Function,
            "let" => TokenKind::Let,
            "true" => TokenKind::True,
            "false" => TokenKind::False,
            "if" => TokenKind::If,
            "else" => TokenKind::Else,
            "return" => TokenKind::Return,
            _ => TokenKind::Identifier(identifier.to_string()),
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
                token.kind = TokenKind::Integer(*integer);
                Ok(token)
            }
            Err(_) => Err(LexerError::InvalidInteger(
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
        token.kind = TokenKind::String(string.to_string());
        token
    }
}

#[cfg(test)]
mod tests {
    use super::*;

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
        ($kind:expr, $lexeme:expr, $line:expr, $column:expr) => {
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
            token!(TokenKind::Assign, 1, 1),
            token!(TokenKind::Plus, 1, 2),
            token!(TokenKind::Minus, 1, 3),
            token!(TokenKind::Multiplication, 1, 4),
            token!(TokenKind::Division, 1, 5),
            token!(TokenKind::LessThan, 1, 6),
            token!(TokenKind::GreaterThan, 1, 7),
            token!(TokenKind::Not, 1, 8),
            token!(TokenKind::Comma, 1, 9),
            token!(TokenKind::Semicolon, 1, 10),
            token!(TokenKind::LeftParenthesis, 1, 11),
            token!(TokenKind::RightParenthesis, 1, 12),
            token!(TokenKind::LeftBrace, 1, 13),
            token!(TokenKind::RightBrace, 1, 14),
            token!(TokenKind::LeftBracket, 1, 15),
            token!(TokenKind::RightBracket, 1, 16),
        ];

        test(input, &expected);
    }

    #[test]
    fn double_char_tokens() {
        let input = "== != <= >= && ||";

        let expected = vec![
            token!(TokenKind::Equal, 1, 1),
            token!(TokenKind::NotEqual, 1, 4),
            token!(TokenKind::LessThanOrEqual, 1, 7),
            token!(TokenKind::GreaterThanOrEqual, 1, 10),
            token!(TokenKind::And, 1, 13),
            token!(TokenKind::Or, 1, 16),
        ];

        test(input, &expected);
    }

    #[test]
    fn identifiers() {
        let input = "five ten add";

        let expected = vec![
            token!(TokenKind::Identifier("five".to_string()), 1, 1),
            token!(TokenKind::Identifier("ten".to_string()), 1, 6),
            token!(TokenKind::Identifier("add".to_string()), 1, 10),
        ];

        test(input, &expected);
    }

    #[test]
    fn keywords() {
        let input = "fn let true false if else return";

        let expected = vec![
            token!(TokenKind::Function, 1, 1),
            token!(TokenKind::Let, 1, 4),
            token!(TokenKind::True, 1, 8),
            token!(TokenKind::False, 1, 13),
            token!(TokenKind::If, 1, 19),
            token!(TokenKind::Else, 1, 22),
            token!(TokenKind::Return, 1, 27),
        ];

        test(input, &expected);
    }

    #[test]
    fn integers() {
        let input = "5 10 100 9999";

        let expected = vec![
            token!(TokenKind::Integer(5), 1, 1),
            token!(TokenKind::Integer(10), 1, 3),
            token!(TokenKind::Integer(100), 1, 6),
            token!(TokenKind::Integer(9999), 1, 10),
        ];

        test(input, &expected);
    }

    #[test]
    fn strings() {
        let input = "\"hello\" \"world\"";

        let expected = vec![
            token!(TokenKind::String("hello".to_string()), 1, 1),
            token!(TokenKind::String("world".to_string()), 1, 9),
        ];

        test(input, &expected);
    }

    #[test]
    fn invalid_token() {
        let inputs = vec!["let a = %", "x = `"];

        let expected = vec![
            vec![
                Ok(token!(TokenKind::Let, 1, 1)),
                Ok(token!(TokenKind::Identifier("a".to_string()), 1, 5)),
                Ok(token!(TokenKind::Assign, 1, 7)),
                Err(error!(LexerError::InvalidToken, "%", 1, 9)),
            ],
            vec![
                Ok(token!(TokenKind::Identifier("x".to_string()), 1, 1)),
                Ok(token!(TokenKind::Assign, 1, 3)),
                Err(error!(LexerError::InvalidToken, "`", 1, 5)),
            ],
        ];

        test_error(&inputs, &expected);
    }

    #[test]
    fn invalid_token_sequence() {
        let inputs = vec!["false &| true", "let a = |-"];

        let expected = vec![
            vec![
                Ok(token!(TokenKind::False, 1, 1)),
                Err(error!(LexerError::InvalidTokenSequence, "&|", 1, 7)),
            ],
            vec![
                Ok(token!(TokenKind::Let, 1, 1)),
                Ok(token!(TokenKind::Identifier("a".to_string()), 1, 5)),
                Ok(token!(TokenKind::Assign, 1, 7)),
                Err(error!(LexerError::InvalidTokenSequence, "|-", 1, 9)),
            ],
        ];

        test_error(&inputs, &expected);
    }
}
