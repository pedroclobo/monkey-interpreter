extern crate lexer;

use std::io::Write;

use lexer::token::Token;
use lexer::Lexer;

fn main() {
	loop {
		print!(">> ");
		std::io::stdout().flush().unwrap();

		let mut input = String::new();
		std::io::stdin().read_line(&mut input).unwrap();

		if input.trim() == r"\q" {
			break;
		}

		let mut lexer = Lexer::new(&input);
		loop {
			let tok = lexer.next_token();
			match tok {
				Token::EOF => break,
				Token::ILLEGAL(_) => {
					println!("{}", tok);
					break;
				}
				_ => println!("{}", tok),
			}
		}
		println!();
	}
}
