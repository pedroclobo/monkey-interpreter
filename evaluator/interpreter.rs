extern crate evaluator;
extern crate lexer;
extern crate parser;
extern crate symbol;

use evaluator::eval;
use lexer::Lexer;
use parser::{ast, Parser};
use std::cell::RefCell;
use std::env;
use std::fs;
use std::rc::Rc;
use symbol::environment::Environment;

fn main() {
	let args: Vec<String> = env::args().collect();
	if args.len() <= 1 {
		eprintln!("Usage: {} <file>", args[0]);
		return;
	}

	let file_name = &args[1];
	let program = fs::read_to_string(file_name).expect("error reading the file");

	let mut lexer = Lexer::new(&program, file_name);
	let mut parser = match Parser::new(&mut lexer) {
		Ok(parser) => parser,
		Err(e) => {
			eprintln!("{}", e);
			return;
		}
	};
	let program = match parser.parse_program() {
		Ok(program) => program,
		Err(e) => {
			eprintln!("{}", e);
			return;
		}
	};

	let env = Rc::new(RefCell::new(Environment::new()));
	match eval(ast::Node::Program(program), &env) {
		Ok(value) => println!("{}", value),
		Err(e) => eprintln!("{}", e),
	}
}
