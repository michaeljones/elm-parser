mod character;
mod string;
mod float;
mod integer;
mod core;

pub use ast::expression::core::Expression;

use ast::helpers::{lo_name, Name};
use ast::expression::character::character;
use ast::expression::string::string;
use ast::expression::float::float;
use ast::expression::integer::integer;

// use nom;
use nom::{anychar, digit, space1};
use nom::types::CompleteStr;

named!(access<CompleteStr, Expression>,
  do_parse!(
    var: variable >>
    accessors: many1!(
        do_parse!(
            char!('.') >>
            name: lo_name >>
            (name)
        )
    ) >>
    (Expression::Access(Box::new(var), accessors))
  )
);

named!(variable<CompleteStr, Expression>,
  map!(
    alt!(
        map!(lo_name, |v| vec!(v))
      | map!(lo_name, |v| vec!(v))
    ),
    Expression::Variable
  )
);

named!(list<CompleteStr, Expression>,
  map!(
    delimited!(
        char!('['),
        separated_list!(
            char!(','),
            expression
        ),
        char!(']')
    ),
    Expression::List
  )
);

named!(lambda<CompleteStr, Expression>,
  do_parse!(
    char!('\\') >>
    args: separated_nonempty_list!(space1, term) >>
    tag!("->") >>
    body: expression >>
    (Expression::Lambda(args, Box::new(body)))
  )
);

named!(term<CompleteStr, Expression>,
  alt!(
      access
    | variable
    // | accessFunction
    | string
    | float
    | integer
    | character
    // | parens (between_ whitespace (expression ops))
    | list
    // | tuple ops
    // | recordUpdate ops
    // | record ops
    // | simplifiedRecord
  )
);

named!(expression<CompleteStr, Expression>,
  alt!(
    // binary
    // | let_expression
    // | case_expression
    // | if_expression
         lambda
  )
);
