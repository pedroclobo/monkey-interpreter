extern crate evaluator;
extern crate lexer;
extern crate parser;

use evaluator::environment::Environment;
use evaluator::eval;
use lexer::Lexer;
use parser::{ast, Parser};
use std::cell::RefCell;
use std::rc::Rc;

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
        match eval(program, &env) {
            Ok(value) => println!("{}", value),
            Err(e) => eprintln!("{}", e),
        }
        println!();
    }
}
