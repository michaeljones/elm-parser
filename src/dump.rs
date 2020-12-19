extern crate clap;
extern crate logos;
extern crate regex;
extern crate walkdir;

use clap::{App, Arg};
use logos::Logos;
// use walkdir::WalkDir;

// use std::ffi::OsStr;
use std::fs::File;
use std::io::prelude::*;

mod evaluate;
mod lexer;
mod parser;

use lexer::Token;

#[derive(Debug)]
enum Error {
    ParserError,
    EvaluateError,
}

fn dump_file(filename: &str, _quiet: bool) -> Result<(), Error> {
    let mut f = File::open(filename).expect("file not found");

    let mut contents = String::new();
    f.read_to_string(&mut contents)
        .expect("something went wrong reading the file");

    let result = Token::lexer(&contents);

    let module = parser::parse(result).map_err(|err| {
        println!("{:?}", &err);
        Error::ParserError
    })?;

    evaluate::evaluate(&module).map_err(|err| {
        println!("{:?}", &err);
        Error::EvaluateError
    })?;

    Ok(())
}

fn main() -> Result<(), Error> {
    let matches = App::new("elm-parser-dump")
        .version("0.1")
        .arg(Arg::with_name("quiet").short("q").long("quiet"))
        .arg(Arg::with_name("path").index(1))
        .get_matches();

    match matches.value_of("path") {
        Some(path) => {
            let quiet = matches.is_present("quiet");

            let attr = std::fs::metadata(path).expect("file not found");

            if attr.is_dir() {
                Ok(())
            } else {
                dump_file(path, quiet)
            }
        }
        None => Ok(()),
    }
}
