use ast::helpers::{lo_name, Name};

use nom;
use nom::{anychar, digit, space1};
use nom::types::CompleteStr;

#[derive(Debug, PartialEq, Clone)]
pub enum Expression {
    Character(char),
    String(String),
    Integer(String),
    Float(String),
    Variable(Vec<Name>),
    List(Vec<Expression>),
    Tuple(Vec<Expression>),
    Access(Box<Expression>, Vec<Name>),
    AccessFunction(Name),
    Record(Vec<(Name, Expression)>),
    RecordUpdate(Name, Vec<(Name, Expression)>),
    If(Box<Expression>, Box<Expression>, Box<Expression>),
    Let(Vec<(Expression, Expression)>, Box<Expression>),
    Case(Box<Expression>, Vec<(Expression, Expression)>),
    Lambda(Vec<Expression>, Box<Expression>),
    Application(Box<Expression>, Box<Expression>),
    BinOp(Box<Expression>, Box<Expression>, Box<Expression>),
}

named!(character<CompleteStr, Expression>,
  map!(delimited!(char!('\''), anychar, char!('\'')), |c| Expression::Character(c))
);

named!(string<CompleteStr, Expression>,
  map!(
      delimited!(char!('"'), take_until!("\""), char!('"')),
      |c| Expression::String(c.to_string())
  )
);

named!(integer<CompleteStr, Expression>,
  map!(nom::digit, |i| Expression::Integer(i.to_string()))
);

named!(float<CompleteStr, Expression>,
  do_parse!(
    start: digit >>
    char!('.') >>
    end: digit >>
    (Expression::Float(start.0.to_owned() + "." + end.0))
  )
);

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
