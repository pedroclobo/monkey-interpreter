extern crate evaluator;
extern crate lexer;
extern crate parser;
extern crate symbol;

use evaluator::eval;
use lexer::Lexer;
use parser::Parser;
use std::cell::RefCell;
use std::rc::Rc;
use symbol::environment::Environment;

use std::io::Write;

fn main() {
	let env = Rc::new(RefCell::new(Environment::new()));

	loop {
		print!(">> ");
		std::io::stdout().flush().unwrap();

		let mut input = String::new();
		std::io::stdin().read_line(&mut input).unwrap();

		if input.trim() == r"\q" {
			break;
		}

		let mut lexer = Lexer::new(&input, "stdin");
		let mut parser = match Parser::new(&mut lexer) {
			Ok(parser) => parser,
			Err(e) => {
				eprintln!("{}", e);
				println!();
				continue;
			}
		};
		let program = match parser.parse_program() {
			Ok(program) => program,
			Err(e) => {
				eprintln!("{}", e);
				println!();
				continue;
			}
		};

		let program = ast::Node::Program(program);
		println!("{}", eval(program, &env));
		println!();
	}
}
