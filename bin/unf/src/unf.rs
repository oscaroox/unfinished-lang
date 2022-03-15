use crate::cli::{Cli, Commands};
use ariadne::Source;
use ast::Program;
use clap::Parser as CParser;
use interpreter::{Environment, Interpreter};
use parser::Parser;
use rustyline::error::ReadlineError;
use rustyline::Editor;
use scanner::Scanner;
use std::{cell::RefCell, process, rc::Rc};

pub struct Unf;

impl Unf {
    pub fn new() -> Unf {
        Unf
    }

    pub fn run(&mut self) {
        let cli = Cli::parse();

        match &cli.command {
            Commands::Run { file } => self.run_file(file.to_string()),
            Commands::Repl => self.run_repl(),
        }
    }

    pub fn run_file(&mut self, file_path: String) {
        let source = match std::fs::read_to_string(file_path) {
            Ok(contents) => contents,
            Err(e) => panic!("{}", e),
        };

        let mut interpreter = Interpreter::new();
        match self.run_contents(source) {
            Ok(exprs) => match interpreter.run(exprs) {
                Ok(_) => {}
                Err(err) => {
                    println!("{:#?}", err);
                    process::exit(1);
                }
            },
            Err(_) => process::exit(1),
        }
    }

    pub fn run_repl(&mut self) {
        let mut rl = Editor::<()>::new();
        let env = Rc::new(RefCell::new(Environment::new()));
        println!("Welcome to the unfinished language repl.");
        loop {
            let readline = rl.readline(">");
            let mut interpreter = Interpreter::with_env(env.clone());
            match readline {
                Ok(line) => match self.run_contents(line) {
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
                },
                Err(ReadlineError::Interrupted) | Err(ReadlineError::Eof) => break,
                Err(err) => {
                    println!("Error: {:#?}", err);
                    break;
                }
            }
        }
    }

    pub fn run_contents(&mut self, source: String) -> Result<Program, String> {
        let scanner = Scanner::new(source.to_string());
        let mut parser = Parser::new(scanner);

        let (exprs, errors) = parser.parse();

        if !errors.is_empty() {
            for mut err in errors.clone() {
                match err.into_report().print(Source::from(source.to_string())) {
                    Ok(_) => {}
                    Err(err) => println!("{}", err),
                }
            }
            return Err("Parser error".to_string());
        }

        // let mut analyzer = Analyzer::new();

        // let errors = analyzer.analyze(&exprs);

        // if errors.len() > 0 {
        //     for mut err in errors {
        //         match err.into_report().print(Source::from(source.to_string())) {
        //             Ok(_) => {}
        //             Err(err) => println!("{}", err),
        //         }
        //     }
        //     return Err("Analyzer error".to_string());
        // }

        typechecker::typecheck(&exprs, None);

        Ok(exprs)
    }
}
