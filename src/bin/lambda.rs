#![feature(bindings_after_at)]

use std::io::{self, BufRead};
use peg::error::ParseError;
use lang2::types::{Type, Env};
use lang2::parser::{self, Ident};

#[derive(Default)]
struct Repl {
    defs : Vec<(Ident, Type<Ident>)>,
}

fn main() {
    let mut repl : Repl = Default::default();

    for line in io::stdin().lock().lines() {
        let line = line.unwrap();
        let parsed = parser::type_repl_line(&line);

        match parsed {
            Ok((cmd, id, ty)) => {
                let cmd = cmd.as_ref().map(String::as_str).unwrap_or("parse");
                match (cmd, id, ty) {
                    ("let", Some(id), Some(ty)) => {
                        println!("{}", ty);
                        repl.defs.push((id, ty.clone()));
                    },
                    ("let", _, _) => {
                        println!(":let needs a name and type")
                    },
                    ("parse", id, Some(ty)) => {
                        if let Some(id) = id {
                            println!("type {} = {}", id, ty);
                        } else {
                            println!("type _ = {}", ty);
                        }
                    }
                    ("parse", _, _) => {
                        println!(":parse needs a type")
                    }
                    ("eval", _, Some(ty)) => {
                        match ty.eval_strict(repl.defs.as_slice()) {
                            Ok(ty) => println!("{}", ty),
                            Err(err) => println!("error: {:?}", err),
                        }
                    }
                    ("eval", ..) => {
                        println!(":eval needs a type")
                    }
                    ("?" | "help", _, _) => {
                        println!("command list:
    :parse      check that the type parses
    :let        save the type to the current environment
    :list       list variables in the environment
    :eval       evaluate the expression to normal form
    :help,:?    show this help message
                        ");
                    },
                    (x, _, _) => {
                        println!("unknown command :{}", x);
                        continue
                    }
                }
            },
            Err(ParseError{location, expected}) => {
                println!("expected: {}", expected);
                println!("at: {}", location);
                println!("{}", line);
                for _ in 0..location.offset {
                    print!(" ")
                }
                println!("^")
            }
        }

    }
}

