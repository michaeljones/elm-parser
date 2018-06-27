#[macro_use]
extern crate nom;
extern crate regex;

use nom::types::CompleteStr;
use std::env;
use std::fs::File;
use std::io::prelude::*;

mod ast;

use ast::{file, ExportSet, Statement};

fn get_export_set_imports(export_set: &ast::ExportSet) -> Vec<String> {
    match export_set {
        ExportSet::SubsetExport(sub_sets) => {
            sub_sets.iter().flat_map(get_export_set_imports).collect()
        }
        ExportSet::FunctionExport(name) => vec![name.clone()],
        _ => vec![],
    }
}

fn get_imported_names(ast: &[ast::Statement]) -> Vec<String> {
    ast.iter()
        .flat_map(|statement| match statement {
            Statement::ImportStatement(_, _, Some(export_sets)) => {
                get_export_set_imports(export_sets)
            }
            _ => vec![],
        })
        .collect()
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let filename = &args[1];

    let mut f = File::open(filename).expect("file not found");

    let mut contents = String::new();
    f.read_to_string(&mut contents)
        .expect("something went wrong reading the file");

    let result = file(CompleteStr(&contents));

    match result {
        Ok((_, ast)) => {
            let imported_names = get_imported_names(&ast);
            println!("{:#?}", imported_names)
        }
        Err(error) => println!("{:#?}", error),
    }
}
