extern crate lexer;
extern crate parser;

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

        let mut lexer = Lexer::new(&input, "stdin");
        let mut parser = match Parser::new(&mut lexer) {
            Ok(parser) => parser,
            Err(e) => {
                eprintln!("{}", e);
                println!();
                continue;
            }
        };

        match parser.parse_program() {
            Ok(program) => println!("{}", program),
            Err(e) => eprintln!("{}", e),
        };

        println!();
    }
}
