#[macro_use]
extern crate nom;
extern crate clap;
extern crate regex;
extern crate walkdir;

use clap::{App, Arg};
use nom::types::CompleteStr;
use walkdir::WalkDir;

use std::env;
use std::ffi::OsStr;
use std::fs::File;
use std::io::prelude::*;

mod ast;

use ast::file;

fn dump_file(filename: &str, quiet: bool) {
    let mut f = File::open(filename).expect("file not found");

    let mut contents = String::new();
    f.read_to_string(&mut contents)
        .expect("something went wrong reading the file");

    let result = file(CompleteStr(&contents));

    match result {
        Ok((_, ast)) => {
            if quiet {
                println!("{:?} parsed successfully", filename)
            } else {
                println!("{:#?}", ast)
            }
        }
        Err(error) => {
            if quiet {
                println!("{:?} failed to parse", filename)
            } else {
                println!("{:#?}", error)
            }
        }
    }
}

fn dump_directory(path: &str, quiet: bool) -> walkdir::Result<()> {
    for entry in WalkDir::new(path) {
        let ent = entry?;

        if ent.path().is_dir() {
            continue;
        }

        if ent.path().extension() != Some(OsStr::new("elm")) {
            continue;
        }

        match ent.path().to_str() {
            Some(path) => dump_file(path, quiet),
            None => {}
        }
    }

    Ok(())
}

fn main() {
    let matches = App::new("elm-parser-dump")
        .version("0.1")
        .arg(Arg::with_name("quiet").short("q").long("quiet"))
        .arg(Arg::with_name("path").index(1))
        .get_matches();

    let args: Vec<String> = env::args().collect();

    match matches.value_of("path") {
        Some(path) => {
            let quiet = matches.is_present("quiet");

            let attr = std::fs::metadata(path).expect("file not found");

            if attr.is_dir() {
                dump_directory(path, quiet);
            } else {
                dump_file(path, quiet);
            }
        }
        None => {}
    }
}
