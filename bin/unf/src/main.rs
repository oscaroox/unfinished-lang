use ariadne::Source;
use ast::Program;
use clap::{App, Arg, SubCommand};
use interpreter::{Environment, Interpreter};
use parser::{Analyzer, Parser};
use scanner::Scanner;
use std::{
    cell::RefCell,
    io::{self, BufRead, Write},
    process,
    rc::Rc,
};

fn main() {
    let matches = App::new("Unfinished Language")
        .version("0.1")
        .author("Oscar D. <oscr@gmail.com>")
        .about("This is the Unfinished language cli")
        .subcommand(SubCommand::with_name("repl").about("starts the repl"))
        .subcommand(
            SubCommand::with_name("run")
                .about("run a unfinished file")
                .arg(
                    Arg::with_name("INPUT")
                        .help("Sets the input file to use")
                        .required(true)
                        .index(1),
                ),
        )
        .get_matches();

    if let Some(matches) = matches.subcommand_matches("run") {
        let input = matches.value_of("INPUT").unwrap();
        let source = match std::fs::read_to_string(input) {
            Ok(contents) => contents,
            Err(e) => panic!("{}", e),
        };

        let mut interpreter = Interpreter::new();
        match parse_contents(source) {
            Ok(exprs) => match interpreter.run(exprs) {
                Ok(_) => {}
                Err(err) => {
                    println!("{:#?}", err);
                    process::exit(1);
                }
            },
            Err(_) => process::exit(1),
        }
    } else if matches.subcommand_matches("repl").is_some() {
        let stdin = io::stdin();
        let mut stdout = io::stdout();
        let env = Rc::new(RefCell::new(Environment::new()));
        println!("Welcome to the unifished language repl start typing.");
        print!("> ");
        stdout.flush().expect("failed to flush to stdout");
        for line in stdin.lock().lines() {
            let contents = line.expect("failed to read from stdin");
            if contents == "" {
                break;
            }
            let mut interpreter = Interpreter::with_env(env.clone());
            match parse_contents(contents) {
                Ok(exprs) => match interpreter.run(exprs) {
                    Ok(val) => match val {
                        interpreter::Value::Unit => {}
                        _ => println!("{}", val),
                    },
                    Err(err) => {
                        println!("{:#?}", err);
                    }
                },
                Err(_) => {}
            };

            print!("> ");
            io::stdout().flush().expect("failed to flush to stdout");
        }
    }
}

fn parse_contents(source: String) -> Result<Program, String> {
    let scanner = Scanner::new(source.to_string());
    let mut parser = Parser::new(scanner);

    let (exprs, errors) = parser.parse();

    if errors.len() > 0 {
        for mut err in errors.clone() {
            match err.into_report().print(Source::from(source.to_string())) {
                Ok(_) => {}
                Err(err) => println!("{}", err),
            }
        }
        return Err("Parser error".to_string());
    }

    let mut analyzer = Analyzer::new();

    let errors = analyzer.analyze(&exprs);

    if errors.len() > 0 {
        for mut err in errors {
            match err.into_report().print(Source::from(source.to_string())) {
                Ok(_) => {}
                Err(err) => println!("{}", err),
            }
        }
        return Err("Analyzer error".to_string());
    }

    Ok(exprs)
}
