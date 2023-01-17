use crate::cli::{Cli, Commands};
use ariadne::Source;
use ast::Program;
use clap::Parser as CParser;
use interpreter::{Environment, Interpreter};
use rustyline::error::ReadlineError;
use rustyline::Editor;
use std::{cell::RefCell, process, rc::Rc};
use typechecker::TypeChecker;

pub struct Unf;

impl Unf {
    pub fn new() -> Unf {
        Unf
    }

    pub fn run(&mut self) {
        let cli = Cli::parse();

        match &cli.command {
            Commands::Run { file, dump_ast } => self.run_file(file.to_string(), *dump_ast),
            Commands::Repl => match self.run_repl() {
                Err(e) => println!("{}", e),
                _ => {}
            },
        }
    }


    pub fn run_file(&mut self, file_path: String, dump_ast: bool) {
        let source = match std::fs::read_to_string(file_path) {
            Ok(contents) => contents,
            Err(e) => panic!("{}", e),
        };

        let mut interpreter = Interpreter::new();

        match  self.run_contents(source) {
            Ok(exprs) => {
                if dump_ast {
                    println!("{exprs:#?}");
                    return;
                }

                match interpreter.run(exprs) {
                    Ok(_) => {}
                    Err(err) => {
                        println!("{:#?}", err);
                        process::exit(1);
                    }
                }
            },
            Err(_) => process::exit(1),
        }
    }

    pub fn run_repl(&mut self) -> rustyline::Result<()> {
        let mut rl = Editor::<()>::new()?;
        let env = Rc::new(RefCell::new(Environment::new()));
        println!("Welcome to the unfinished language repl.");
        loop {
            let mut interpreter = Interpreter::with_env(env.clone());
            match  rl.readline(">") {
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
        rustyline::Result::Ok(())
    }


    pub fn run_contents(&mut self, source: String) -> Result<Program, String> {
        let mut type_checker = TypeChecker::new();
        let (exprs, errors) = parser::parse(&source);
        
        if !errors.is_empty() {
            let reports: Vec<ariadne::Report> = errors.iter()
            .map(|e| e.clone().into_report())
            .collect();
                
            for report in reports {
                match report.print(Source::from(source.to_string())) {
                    Ok(_) => {}
                    Err(err) => println!("{}", err),
                }
            }

            return Err("Parser error".to_string());
        }

        let (_, type_errors) = type_checker.type_check(&exprs, None);

        if !type_errors.is_empty() {
            for mut err in type_errors.clone() {
                match err.into_report().print(Source::from(source.to_string())) {
                    Ok(_) => {}
                    Err(err) => println!("{}", err),
                }
            }
            return Err("Type check error".to_string());
        }

        Ok(exprs)
    }
}
