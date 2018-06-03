mod character;
mod string;
mod float;
mod integer;
mod access;
mod variable;
mod core;

pub use ast::expression::core::Expression;

use ast::expression::character::character;
use ast::expression::string::string;
use ast::expression::float::float;
use ast::expression::integer::integer;
use ast::expression::variable::variable;
use ast::expression::access::access;
use ast::helpers::{at_least_indent, lo_name, operator};

// use nom;
use nom::{multispace, multispace0, space1};
use nom::types::CompleteStr;

named_args!(tuple(indentation: u32) <CompleteStr, Expression>,
  map!(
    delimited!(
        char!('('),
        separated_list!(
            char!(','),
            delimited!(
                multispace0,
                call!(expression, indentation),
                multispace0
            )
        ),
        char!(')')
    ),
    Expression::Tuple
  )
);

named_args!(list(indentation: u32) <CompleteStr, Expression>,
  map!(
    delimited!(
        char!('['),
        // Not sure why this optional is required
        opt!(
            separated_list!(
                char!(','),
                delimited!(
                    multispace0,
                    call!(expression, indentation),
                    multispace0
                )
            )
        ),
        char!(']')
    ),
    |o| Expression::List(o.unwrap_or(vec![]))
  )
);

named_args!(record(indentation: u32) <CompleteStr, Expression>,
  map!(
    delimited!(
        char!('{'),
        separated_list!(
            char!(','),
            do_parse!(
                multispace0 >>
                name: lo_name >>
                multispace0 >>
                char!('=') >>
                multispace0 >>
                expression: call!(expression, indentation) >>
                multispace0 >>
                ((name, expression))
            )
        ),
        char!('}')
    ),
    Expression::Record
  )
);

named_args!(simplified_record(indentation: u32) <CompleteStr, Expression>,
  map!(
    delimited!(
        char!('{'),
        separated_list!(
            char!(','),
            do_parse!(
                multispace0 >>
                name: lo_name >>
                multispace0 >>
                ((name.clone(), Expression::Variable(vec![name.to_string()])))
            )
        ),
        char!('}')
    ),
    Expression::Record
  )
);

named_args!(record_update(indentation: u32) <CompleteStr, Expression>,
  delimited!(
    char!('{'),
    do_parse!(
      multispace0 >>
      name: lo_name >>
      multispace0 >>
      char!('|') >>
      multispace0 >>
      pairs: separated_list!(
        char!(','),
        do_parse!(
          multispace0 >>
          name: lo_name >>
          multispace0 >>
          char!('=') >>
          multispace0 >>
          expression: call!(expression, indentation) >>
          multispace0 >>
          ((name, expression))
        )
      ) >>
      (Expression::RecordUpdate(name.to_string(), pairs))
    ),
    char!('}')
  )
);

named_args!(term(indentation: u32) <CompleteStr, Expression>,
  alt!(
      access
    | variable
    // | accessFunction
    | string
    | float
    | integer
    | character
    // | parens (between_ whitespace (expression ops))
    | call!(list, indentation)
    | call!(tuple, indentation)
    | call!(record_update, indentation)
    | call!(record, indentation)
    | call!(simplified_record, indentation)
  )
);

named_args!(lambda(indentation: u32) <CompleteStr, Expression>,
  do_parse!(
    char!('\\') >>
    args: separated_nonempty_list!(space1, call!(term, indentation)) >>
    tag!("->") >>
    body: call!(expression, indentation) >>
    (Expression::Lambda(args, Box::new(body)))
  )
);

named_args!(spaces_or_new_line_and_indent(indentation: u32) <CompleteStr, String>,
    alt!(
          map!(is_a!(" "), |s| s.to_string())
        | do_parse!(
            char!('\n') >>
            call!(at_least_indent, indentation) >>
            ("".to_string())
          )
    )
);

named_args!(application_or_var(indentation: u32) <CompleteStr, Expression>,
  map!(
      separated_list!(
          call!(spaces_or_new_line_and_indent, indentation),
          call!(term, indentation)
      ),
      |v| {
          if v.len() == 1 {
              v[0].clone()
          } else {
              Expression::Application(v)
          }
      }
  )
);

// Application followed by possibly a operator + expression
named_args!(binary(indentation: u32) <CompleteStr, Expression>,
   dbg_dmp!(
   do_parse!(
     application_or_var: call!(application_or_var, indentation) >>
     operator_exp: opt!(
       do_parse!(
         operator: operator >>
         multispace >>
         expression: call!(expression, indentation) >>
         (operator, expression)
       )
     ) >>
     (match operator_exp {
         Some((op, exp)) => Expression::BinOp(
             Box::new(Expression::Variable(vec![op.to_string()])),
             Box::new(application_or_var),
             Box::new(exp)
         ),
         None => application_or_var
     })
  )
  )
);

named_args!(pub expression(indentation: u32) <CompleteStr, Expression>,
  alt!(
         call!(binary, indentation)
    // | let_expression
    // | case_expression
    // | if_expression
       | call!(lambda, indentation)
  )
);

#[cfg(test)]
mod tests {

    use ast::expression::*;
    use nom::types::CompleteStr;

    fn var(name: &str) -> Expression {
        Expression::Variable(vec![name.to_string()])
    }

    fn int(text: &str) -> Expression {
        Expression::Integer(text.to_string())
    }

    // Tuples

    #[test]
    fn simple_tuple() {
        assert_eq!(
            tuple(CompleteStr("(a,b)"), 0),
            Ok((CompleteStr(""), Expression::Tuple(vec![var("a"), var("b")])))
        );
    }

    #[test]
    fn simple_tuple_with_formatting() {
        assert_eq!(
            tuple(CompleteStr("( a, b )"), 0),
            Ok((CompleteStr(""), Expression::Tuple(vec![var("a"), var("b")])))
        );
    }

    // Lists

    #[test]
    fn empty_list() {
        assert_eq!(
            list(CompleteStr("[]"), 0),
            Ok((CompleteStr(""), Expression::List(vec![])))
        );
    }

    #[test]
    fn simple_list() {
        assert_eq!(
            list(CompleteStr("[a,b]"), 0),
            Ok((CompleteStr(""), Expression::List(vec![var("a"), var("b")])))
        );
    }

    #[test]
    fn simple_list_with_formatting() {
        assert_eq!(
            list(CompleteStr("[ a, b ]"), 0),
            Ok((CompleteStr(""), Expression::List(vec![var("a"), var("b")])))
        );
    }

    #[test]
    fn simple_int_list() {
        assert_eq!(
            list(CompleteStr("[1,2]"), 0),
            Ok((CompleteStr(""), Expression::List(vec![int("1"), int("2")])))
        );
    }

    #[test]
    fn tuple_list() {
        assert_eq!(
            list(CompleteStr("[(a, b), (a, b)]"), 0),
            Ok((
                CompleteStr(""),
                Expression::List(vec![
                    Expression::Tuple(vec![var("a"), var("b")]),
                    Expression::Tuple(vec![var("a"), var("b")]),
                ])
            ))
        );
    }

    // Application or Var
    #[test]
    fn simple_variable() {
        assert_eq!(
            application_or_var(CompleteStr("abc"), 0),
            Ok((CompleteStr(""), var("abc")))
        );
    }

    #[test]
    fn simple_application() {
        assert_eq!(
            application_or_var(CompleteStr("Just abc"), 0),
            Ok((
                CompleteStr(""),
                Expression::Application(vec![var("Just"), var("abc")])
            ))
        );
    }

    // Record

    #[test]
    fn simple_record() {
        assert_eq!(
            record(CompleteStr("{ a = b }"), 0),
            Ok((
                CompleteStr(""),
                Expression::Record(vec![("a".to_string(), var("b"))])
            ))
        );
    }

    #[test]
    fn simple_record_with_many_fields() {
        assert_eq!(
            record(CompleteStr("{ a = b, b = 2 }"), 0),
            Ok((
                CompleteStr(""),
                Expression::Record(vec![
                    ("a".to_string(), var("b")),
                    ("b".to_string(), int("2")),
                ])
            ))
        );
    }

    #[test]
    fn simple_record_with_many_tuple_fields() {
        assert_eq!(
            record(CompleteStr("{ a = (a, b), b = (a, b) }"), 0),
            Ok((
                CompleteStr(""),
                Expression::Record(vec![
                    ("a".to_string(), Expression::Tuple(vec![var("a"), var("b")])),
                    ("b".to_string(), Expression::Tuple(vec![var("a"), var("b")])),
                ])
            ))
        );
    }

    #[test]
    fn simple_record_with_updated_field() {
        assert_eq!(
            record_update(CompleteStr("{ a | b = 2, c = 3 }"), 0),
            Ok((
                CompleteStr(""),
                Expression::RecordUpdate(
                    "a".to_string(),
                    vec![("b".to_string(), int("2")), ("c".to_string(), int("3"))]
                )
            ))
        );
    }

    #[test]
    fn simple_record_with_advanced_field() {
        assert_eq!(
            record(CompleteStr("{ a = Just 2 }"), 0),
            Ok((
                CompleteStr(""),
                Expression::Record(vec![
                    (
                        "a".to_string(),
                        Expression::Application(vec![var("Just"), int("2")]),
                    ),
                ])
            ))
        );
    }

    #[test]
    fn simple_record_update_with_advanced_field() {
        assert_eq!(
            record_update(CompleteStr("{ a | a = Just 2 }"), 0),
            Ok((
                CompleteStr(""),
                Expression::RecordUpdate(
                    "a".to_string(),
                    vec![
                        (
                            "a".to_string(),
                            Expression::Application(vec![var("Just"), int("2")]),
                        ),
                    ],
                )
            ))
        );
    }

    #[test]
    fn simple_record_destructuring_pattern() {
        assert_eq!(
            simplified_record(CompleteStr("{ a, b }"), 0),
            Ok((
                CompleteStr(""),
                Expression::Record(vec![
                    ("a".to_string(), var("a")),
                    ("b".to_string(), var("b")),
                ],)
            ))
        );
    }
}
