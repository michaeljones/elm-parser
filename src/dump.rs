#[macro_use]
extern crate nom;
extern crate regex;

use nom::types::CompleteStr;
use std::env;
use std::fs::File;
use std::io::prelude::*;

mod ast;

use ast::file;

fn main() {
    let args: Vec<String> = env::args().collect();
    let filename = &args[1];

    let mut f = File::open(filename).expect("file not found");

    let mut contents = String::new();
    f.read_to_string(&mut contents)
        .expect("something went wrong reading the file");

    let result = file(CompleteStr(&contents));

    match result {
        Ok((_, ast)) => println!("{:#?}", ast),
        Err(error) => println!("{:#?}", error),
    }
}
