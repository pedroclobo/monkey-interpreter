extern crate evaluator;
extern crate lexer;
extern crate parser;

use evaluator::eval;
use lexer::Lexer;
use parser::Parser;

use std::io::Write;

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
		let mut parser = Parser::new(&mut lexer);
		let program = parser.parse_program();
		let evaluator = eval(ast::Node::Program(program));

		println!("{}", evaluator);
		println!();
	}
}
