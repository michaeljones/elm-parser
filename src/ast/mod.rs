use nom::types::CompleteStr;

pub mod binop;
pub mod expression;
pub mod helpers;
pub mod statement;

use ast::statement::{statements, Statement};

named!(pub file<CompleteStr, Vec<Statement>>,
   terminated!(statements, eof!())
);
