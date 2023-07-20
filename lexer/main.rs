extern crate lexer;

use std::io::Write;

use lexer::{Lexer, TokenKind};

fn main() {
	loop {
		print!(">> ");
		std::io::stdout().flush().unwrap();

		let mut input = String::new();
		std::io::stdin().read_line(&mut input).unwrap();

		if input.trim() == r"\q" {
			break;
		}

		let mut lexer = Lexer::new(&input, "stdin");

		loop {
			let tok = lexer.next_token();
			match tok {
				Ok(tok) => match tok.kind {
					TokenKind::EOF => break,
					kind => println!("{:?}", kind),
				},
				Err(e) => {
					eprintln!("{}", e);
					break;
				}
			}
		}
		println!();
	}
}
