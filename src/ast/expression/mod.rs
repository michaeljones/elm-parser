mod access;
mod access_function;
mod character;
pub mod core;
mod float;
pub mod function;
mod integer;
mod string;
mod variable;

pub use ast::expression::core::Expression;
use ast::expression::core::{Function, FunctionDefinition, FunctionSignature, LetEntry};

use ast::expression::access::access;
use ast::expression::access_function::access_function;
use ast::expression::character::character;
use ast::expression::float::float;
use ast::expression::function::function;
use ast::expression::integer::integer;
use ast::expression::string::string;
use ast::expression::variable::variable;
use ast::helpers::{
    lo_name, operator, opt_spaces_or_new_lines_and_indent, spaces_or_new_lines_and_indent, IR,
};

// use nom;
use nom::types::CompleteStr;
use nom::{multispace, multispace0, space1};

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
        preceded!(char!('['), opt!(call!(spaces_or_new_lines_and_indent, indentation, IR::GTE))),
        // Not sure why this optional is required
        opt!(
            separated_list!(
                delimited!(
                    opt!(call!(spaces_or_new_lines_and_indent, indentation, IR::GTE)),
                    char!(','),
                    opt!(call!(spaces_or_new_lines_and_indent, indentation, IR::GTE))
                ),
                call!(expression, indentation)
            )
        ),
        terminated!(opt!(call!(spaces_or_new_lines_and_indent, indentation, IR::GTE)), char!(']'))
    ),
    |o| Expression::List(o.unwrap_or_else(|| vec![]))
  )
);

named_args!(record(indentation: u32) <CompleteStr, Expression>,
  map!(
    delimited!(
        preceded!(char!('{'), opt!(call!(spaces_or_new_lines_and_indent, indentation, IR::GTE))),
        separated_list!(
            delimited!(
                opt!(call!(spaces_or_new_lines_and_indent, indentation, IR::GTE)),
                char!(','),
                opt!(call!(spaces_or_new_lines_and_indent, indentation, IR::GTE))
            ),
            separated_pair!(
                lo_name,
                delimited!(
                    opt!(call!(spaces_or_new_lines_and_indent, indentation, IR::GTE)),
                    char!('='),
                    opt!(call!(spaces_or_new_lines_and_indent, indentation, IR::GTE))
                ),
                call!(expression, indentation)
            )
        ),
        terminated!(opt!(call!(spaces_or_new_lines_and_indent, indentation, IR::GTE)), char!('}'))
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
    preceded!(char!('{'), call!(opt_spaces_or_new_lines_and_indent, indentation, IR::GTE)),
    do_parse!(
      name: lo_name >>
      call!(opt_spaces_or_new_lines_and_indent, indentation, IR::GTE) >>
      char!('|') >>
      multispace0 >>
      pairs: separated_list!(
        delimited!(
            call!(opt_spaces_or_new_lines_and_indent, indentation, IR::GTE),
            char!(','),
            call!(opt_spaces_or_new_lines_and_indent, indentation, IR::GTE)
        ),
        do_parse!(
          name: lo_name >>
          multispace0 >>
          char!('=') >>
          multispace0 >>
          expression: call!(expression, indentation) >>
          ((name, expression))
        )
      ) >>
      (Expression::RecordUpdate(name.to_string(), pairs))
    ),
    terminated!(call!(spaces_or_new_lines_and_indent, indentation, IR::GTE), char!('}'))
  )
);

named_args!(parens(indentation: u32) <CompleteStr, Expression>,
    delimited!(
        char!('('),
        call!(expression, indentation),
        char!(')')
    )
);

// Term

named_args!(pub term(indentation: u32) <CompleteStr, Expression>,
  alt!(
      access
    | variable
    | access_function
    | string
    | float
    | integer
    | character
    | call!(parens, indentation)
    | call!(list, indentation)
    | call!(tuple, indentation)
    | call!(record_update, indentation)
    | call!(record, indentation)
    | call!(simplified_record, indentation)
  )
);

// Lambda

named_args!(lambda(indentation: u32) <CompleteStr, Expression>,
  do_parse!(
    char!('\\') >>
    args: separated_nonempty_list!(space1, call!(term, indentation)) >>
    call!(spaces_or_new_lines_and_indent, indentation, IR::GTE) >>
    tag!("->") >>
    new_indent: call!(spaces_or_new_lines_and_indent, indentation, IR::GTE) >>
    body: call!(expression, new_indent) >>
    (Expression::Lambda(args, Box::new(body)))
  )
);

// Application

named_args!(application_or_var(indentation: u32) <CompleteStr, Expression>,
  map_res!(
      separated_list!(
          call!(spaces_or_new_lines_and_indent, indentation, IR::GT),
          call!(term, indentation)
      ),
      |v: Vec<Expression>| {
          if v.is_empty() {
              Err("Empty list".to_string())
          }
          else if v.len() == 1 {
              Ok(v[0].clone())
          } else {
              let mut app = Expression::Application(Box::new(v[0].clone()), Box::new(v[1].clone()));
              for entry in v.iter().skip(2) {
                  app = Expression::Application(Box::new(app), Box::new(entry.clone()))
              }
              Ok(app)
          }
      }
  )
);

// Let Expression

named_args!(let_binding_target(indentation: u32) <CompleteStr, Expression>,
  alt!(
      variable
    | call!(list, indentation)
    | call!(tuple, indentation)
    | call!(simplified_record, indentation)
  )
);

named_args!(let_binding(indentation: u32) <CompleteStr, LetEntry>,
   do_parse!(
       binding: call!(let_binding_target, indentation) >>
       call!(spaces_or_new_lines_and_indent, indentation, IR::GTE) >>
       char!('=') >>
       new_indent: call!(spaces_or_new_lines_and_indent, indentation, IR::GTE) >>
       expression: call!(expression, new_indent) >>
       (LetEntry::LetBinding(binding, expression))
   )
);

named_args!(let_bindings(indentation: u32) <CompleteStr, Vec<LetEntry>>,
    separated_nonempty_list!(
        call!(spaces_or_new_lines_and_indent, indentation, IR::EQ),
        alt!(
            call!(let_binding, indentation)
          | map!(call!(function, indentation), LetEntry::LetFunction)
        )
    )
);

named_args!(let_expression(indentation: u32) <CompleteStr, Expression>,
   do_parse!(
       tag!("let") >>
       assignment_indentation: call!(spaces_or_new_lines_and_indent, indentation, IR::GTE) >>
       assignments: call!(let_bindings, assignment_indentation) >>
       call!(spaces_or_new_lines_and_indent, indentation, IR::GTE) >>
       tag!("in") >>
       call!(spaces_or_new_lines_and_indent, indentation, IR::GTE) >>
       expression: call!(expression, indentation) >>
       (Expression::Let(assignments, Box::new(expression)))
   )
);

// Case Expression

named_args!(case(indentation: u32) <CompleteStr, (Expression, Expression)>,
   do_parse!(
       matcher: call!(expression, indentation) >>
       call!(spaces_or_new_lines_and_indent, indentation, IR::GTE) >>
       tag!("->") >>
       new_indent: call!(spaces_or_new_lines_and_indent, indentation, IR::GTE) >>
       expression: call!(expression, new_indent) >>
       (matcher, expression)
   )
);

named_args!(cases(indentation: u32) <CompleteStr, Vec<(Expression, Expression)>>,
    separated_nonempty_list!(
        call!(spaces_or_new_lines_and_indent, indentation, IR::EQ),
        call!(case, indentation)
    )
);

named_args!(case_expression(indentation: u32) <CompleteStr, Expression>,
   do_parse!(
       tag!("case") >>
       call!(spaces_or_new_lines_and_indent, indentation, IR::GTE) >>
       expression: call!(expression, indentation) >>
       call!(spaces_or_new_lines_and_indent, indentation, IR::GTE) >>
       tag!("of") >>
       new_indent: call!(spaces_or_new_lines_and_indent, indentation, IR::GTE) >>
       cases: call!(cases, new_indent) >>
       (Expression::Case(Box::new(expression), cases))
   )
);

// If Expression

named_args!(if_expression(indentation: u32) <CompleteStr, Expression>,
   do_parse!(
       tag!("if") >>
       call!(spaces_or_new_lines_and_indent, indentation, IR::GTE) >>
       test: call!(expression, indentation) >>
       call!(spaces_or_new_lines_and_indent, indentation, IR::GTE) >>
       tag!("then") >>
       new_if_indent: call!(spaces_or_new_lines_and_indent, indentation, IR::GTE) >>
       if_exp: call!(expression, new_if_indent) >>
       call!(spaces_or_new_lines_and_indent, indentation, IR::GTE) >>
       tag!("else") >>
       new_else_indent: call!(spaces_or_new_lines_and_indent, indentation, IR::GTE) >>
       else_exp: call!(expression, new_else_indent) >>
       (Expression::If(Box::new(test), Box::new(if_exp), Box::new(else_exp)))
   )
);

// Binary

named!(operator_or_as<CompleteStr, Expression>,
  map!(
    alt!(
        map!(tag!("as"), |text| text.to_string())
      | operator
    ),
    |op| Expression::Variable(vec![op])
  )
);

named_args!(binary(indentation: u32) <CompleteStr, Expression>,
   // Application followed by possibly a operator + expression
   do_parse!(
     application_or_var: call!(application_or_var, indentation) >>
     operator_exp: opt!(
       do_parse!(
         call!(spaces_or_new_lines_and_indent, indentation, IR::GTE) >>
         operator: operator_or_as >>
         multispace >>
         expression: call!(expression, indentation) >>
         (operator, expression)
       )
     ) >>
     (match operator_exp {
         Some((op, exp)) => Expression::BinOp(
             Box::new(op),
             Box::new(application_or_var),
             Box::new(exp)
         ),
         None => application_or_var
     })
  )
);

named_args!(pub expression(indentation: u32) <CompleteStr, Expression>,
  alt!(
         call!(binary, indentation)
       | call!(let_expression, indentation)
       | call!(case_expression, indentation)
       | call!(if_expression, indentation)
       | call!(lambda, indentation)
  )
);

#[cfg(test)]
mod tests {

    use ast::expression::*;
    use ast::type_::core::Type;

    use nom::types::CompleteStr;

    fn var(name: &str) -> Expression {
        Expression::Variable(vec![name.to_string()])
    }

    fn int(text: &str) -> Expression {
        Expression::Integer(text.to_string())
    }

    fn application(a: Expression, b: Expression) -> Expression {
        Expression::Application(Box::new(a), Box::new(b))
    }

    fn bin_op(op: Expression, left: Expression, right: Expression) -> Expression {
        Expression::BinOp(Box::new(op), Box::new(left), Box::new(right))
    }

    fn if_(test: Expression, if_exp: Expression, else_exp: Expression) -> Expression {
        Expression::If(Box::new(test), Box::new(if_exp), Box::new(else_exp))
    }

    fn tapp(a: Type, b: Type) -> Type {
        Type::TypeApplication(Box::new(a), Box::new(b))
    }

    fn tcon(name: &str, v: Vec<Type>) -> Type {
        Type::TypeConstructor(vec![name.to_string()], v)
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
    fn multi_line_list() {
        assert_eq!(
            list(
                CompleteStr(
                    "[ 1
                     , 2
                     ]"
                ),
                0
            ),
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
            application_or_var(CompleteStr("f a"), 0),
            Ok((
                CompleteStr(""),
                Expression::Application(Box::new(var("f")), Box::new(var("a")))
            ))
        );
    }

    #[test]
    fn curried_application() {
        assert_eq!(
            application_or_var(CompleteStr("f a b"), 0),
            Ok((
                CompleteStr(""),
                Expression::Application(
                    Box::new(Expression::Application(
                        Box::new(var("f")),
                        Box::new(var("a"))
                    )),
                    Box::new(var("b"))
                )
            ))
        );
    }

    #[test]
    fn curried_application_with_parens() {
        assert_eq!(
            application_or_var(CompleteStr("(f a) b"), 0),
            Ok((
                CompleteStr(""),
                Expression::Application(
                    Box::new(Expression::Application(
                        Box::new(var("f")),
                        Box::new(var("a"))
                    )),
                    Box::new(var("b"))
                )
            ))
        );
    }

    #[test]
    fn multiline_application() {
        assert_eq!(
            application_or_var(CompleteStr("f\n   a\n b"), 0),
            Ok((
                CompleteStr(""),
                Expression::Application(
                    Box::new(Expression::Application(
                        Box::new(var("f")),
                        Box::new(var("a"))
                    )),
                    Box::new(var("b"))
                )
            ))
        );
    }

    #[test]
    fn multiline_bug() {
        assert_eq!(
            application_or_var(CompleteStr("f\n (==)"), 0),
            Ok((
                CompleteStr(""),
                Expression::Application(Box::new(var("f")), Box::new(var("=="))),
            ))
        );
    }

    #[test]
    fn same_multiline_bug() {
        assert_eq!(
            application_or_var(CompleteStr("f\n \"I like the symbol =\""), 0),
            Ok((
                CompleteStr(""),
                Expression::Application(
                    Box::new(var("f")),
                    Box::new(Expression::String("I like the symbol =".to_string()))
                ),
            ))
        );
    }

    #[test]
    fn constructor_application() {
        assert_eq!(
            application_or_var(CompleteStr("Cons a Nil"), 0),
            Ok((
                CompleteStr(""),
                Expression::Application(
                    Box::new(Expression::Application(
                        Box::new(var("Cons")),
                        Box::new(var("a"))
                    )),
                    Box::new(var("Nil"))
                )
            ))
        );
    }

    #[test]
    fn application_with_record_update() {
        assert_eq!(
            application_or_var(CompleteStr("a  { r | f = 1 } c"), 0),
            Ok((
                CompleteStr(""),
                application(
                    application(
                        var("a"),
                        Expression::RecordUpdate(
                            "r".to_string(),
                            vec![("f".to_string(), int("1"))]
                        )
                    ),
                    var("c")
                )
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
    fn multi_line_record() {
        assert_eq!(
            record(
                CompleteStr(
                    "{ a = b
                     , b = 2
                     }"
                ),
                1
            ),
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
    fn multi_line_record_with_comments() {
        assert_eq!(
            record(
                CompleteStr(
                    "{ a = b
                       -- First comment
                     , b = 2 -- Second comment
                     }"
                ),
                1
            ),
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
                Expression::Record(vec![("a".to_string(), application(var("Just"), int("2")))])
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
                    vec![("a".to_string(), application(var("Just"), int("2")))],
                )
            ))
        );
    }

    #[test]
    fn record_update_with_comment() {
        assert_eq!(
            record_update(
                CompleteStr(
                    "{ a
                      -- Comment about something
                        | b = 1
                    }"
                ),
                0
            ),
            Ok((
                CompleteStr(""),
                Expression::RecordUpdate("a".to_string(), vec![("b".to_string(), int("1"))],)
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

    // Binary Ops

    #[test]
    fn simple_binary_op() {
        assert_eq!(
            binary(CompleteStr("x + 1"), 0),
            Ok((
                CompleteStr(""),
                Expression::BinOp(Box::new(var("+")), Box::new(var("x")), Box::new(int("1")),)
            ))
        );
    }

    // Let Expressions

    #[test]
    fn let_single_binding() {
        assert_eq!(
            let_binding(CompleteStr("a = 42"), 0),
            Ok((CompleteStr(""), LetEntry::LetBinding(var("a"), int("42"))))
        );
    }

    #[test]
    fn let_group_single_binding() {
        assert_eq!(
            let_bindings(CompleteStr("a = 42"), 0),
            Ok((
                CompleteStr(""),
                vec![LetEntry::LetBinding(var("a"), int("42"))]
            ))
        );
    }

    #[test]
    fn let_group_single_function() {
        assert_eq!(
            let_bindings(CompleteStr("a b = 42"), 0),
            Ok((
                CompleteStr(""),
                vec![LetEntry::LetFunction(Function {
                    signature: None,
                    definition: FunctionDefinition {
                        name: "a".to_string(),
                        args: vec![var("b")],
                        body: int("42"),
                    },
                })]
            ))
        );
    }

    #[test]
    fn let_group_single_function_and_signature() {
        assert_eq!(
            let_bindings(CompleteStr("a : Int -> Int\na b = 42"), 0),
            Ok((
                CompleteStr(""),
                vec![LetEntry::LetFunction(Function {
                    signature: Some(FunctionSignature {
                        name: "a".to_string(),
                        type_: tapp(tcon("Int", vec![]), tcon("Int", vec![])),
                    }),
                    definition: FunctionDefinition {
                        name: "a".to_string(),
                        args: vec![var("b")],
                        body: int("42"),
                    },
                })]
            ))
        );
    }

    #[test]
    fn let_group_two_functions() {
        assert_eq!(
            let_bindings(CompleteStr("a b = 42\nc d = 24"), 0),
            Ok((
                CompleteStr(""),
                vec![
                    LetEntry::LetFunction(Function {
                        signature: None,
                        definition: FunctionDefinition {
                            name: "a".to_string(),
                            args: vec![var("b")],
                            body: int("42"),
                        },
                    }),
                    LetEntry::LetFunction(Function {
                        signature: None,
                        definition: FunctionDefinition {
                            name: "c".to_string(),
                            args: vec![var("d")],
                            body: int("24"),
                        },
                    }),
                ]
            ))
        );
    }

    #[test]
    fn let_block_with_single_binding() {
        assert_eq!(
            let_expression(CompleteStr("let a = 42 in a"), 0),
            Ok((
                CompleteStr(""),
                Expression::Let(
                    vec![LetEntry::LetBinding(var("a"), int("42"))],
                    Box::new(var("a"))
                )
            ))
        );
    }

    #[test]
    fn let_block_bind_to_underscore() {
        assert_eq!(
            let_expression(CompleteStr("let _ = 42 in 24"), 0),
            Ok((
                CompleteStr(""),
                Expression::Let(
                    vec![LetEntry::LetBinding(var("_"), int("42"))],
                    Box::new(int("24"))
                )
            ))
        );
    }

    #[test]
    fn let_block_can_start_with_a_tag_name() {
        assert_eq!(
            let_expression(CompleteStr("let letter = 1 \n in letter"), 0),
            Ok((
                CompleteStr(""),
                Expression::Let(
                    vec![LetEntry::LetBinding(var("letter"), int("1"))],
                    Box::new(var("letter"))
                )
            ))
        );
    }

    #[test]
    fn let_block_function_1() {
        assert_eq!(
            let_expression(
                CompleteStr(
                    "let
 f x = x + 1
in
 f 4"
                ),
                0
            ),
            Ok((
                CompleteStr(""),
                Expression::Let(
                    vec![LetEntry::LetFunction(Function {
                        signature: None,
                        definition: FunctionDefinition {
                            name: "f".to_string(),
                            args: vec![var("x")],
                            body: bin_op(var("+"), var("x"), int("1")),
                        },
                    })],
                    Box::new(application(var("f"), int("4")))
                )
            ))
        );
    }

    #[test]
    fn let_block_function_2() {
        assert_eq!(
            let_expression(
                CompleteStr(
                    "let
  f x = x + 1
  g x = x + 1
in
  f 4"
                ),
                0
            ),
            Ok((
                CompleteStr(""),
                Expression::Let(
                    vec![
                        LetEntry::LetFunction(Function {
                            signature: None,
                            definition: FunctionDefinition {
                                name: "f".to_string(),
                                args: vec![var("x")],
                                body: bin_op(var("+"), var("x"), int("1")),
                            },
                        }),
                        LetEntry::LetFunction(Function {
                            signature: None,
                            definition: FunctionDefinition {
                                name: "g".to_string(),
                                args: vec![var("x")],
                                body: bin_op(var("+"), var("x"), int("1")),
                            },
                        }),
                    ],
                    Box::new(application(var("f"), int("4")))
                )
            ))
        );
    }

    #[test]
    fn let_block_multiple_bindings() {
        assert_eq!(
            let_expression(
                CompleteStr(
                    "let
  a = 42
  b = a + 1
in
  b"
                ),
                0
            ),
            Ok((
                CompleteStr(""),
                Expression::Let(
                    vec![
                        LetEntry::LetBinding(var("a"), int("42")),
                        LetEntry::LetBinding(var("b"), bin_op(var("+"), var("a"), int("1"))),
                    ],
                    Box::new(var("b"))
                )
            ))
        );
    }

    #[test]
    fn let_block_function_with_function_signature() {
        assert_eq!(
            let_expression(
                CompleteStr(
                    "let
 f : Int -> Int
 f x = x + 1
in
 f 4"
                ),
                0
            ),
            Ok((
                CompleteStr(""),
                Expression::Let(
                    vec![LetEntry::LetFunction(Function {
                        signature: Some(FunctionSignature {
                            name: "f".to_string(),
                            type_: tapp(
                                tcon("Int", vec![]),
                                tcon("Int", vec![]),
                            ),
                        }),
                        definition: FunctionDefinition {
                            name: "f".to_string(),
                            args: vec![var("x")],
                            body: bin_op(var("+"), var("x"), int("1")),
                        },
                    })],
                    Box::new(application(var("f"), int("4")))
                )
            ))
        );
    }

    #[test]
    fn let_block_destructuring() {
        assert_eq!(
            expression(CompleteStr("let (a,b) = (1,2) in a"), 0),
            Ok((
                CompleteStr(""),
                Expression::Let(
                    vec![LetEntry::LetBinding(
                        Expression::Tuple(vec![var("a"), var("b")]),
                        Expression::Tuple(vec![int("1"), int("2")]),
                    )],
                    Box::new(var("a")),
                )
            ))
        );
    }

    // Case Expressions

    #[test]
    fn case_simple_statement() {
        assert_eq!(
            case_expression(
                CompleteStr(
                    "case x of
  Nothing ->
    0
  Just y ->
    y"
                ),
                0
            ),
            Ok((
                CompleteStr(""),
                Expression::Case(
                    Box::new(var("x")),
                    vec![
                        (var("Nothing"), int("0")),
                        (application(var("Just"), var("y")), var("y")),
                    ],
                )
            ))
        );
    }

    #[test]
    fn case_simple_statement_with_blank_line() {
        assert_eq!(
            case_expression(
                CompleteStr(
                    "case x of
  Nothing ->
    0

  Just y ->
    y"
                ),
                0
            ),
            Ok((
                CompleteStr(""),
                Expression::Case(
                    Box::new(var("x")),
                    vec![
                        (var("Nothing"), int("0")),
                        (application(var("Just"), var("y")), var("y")),
                    ],
                )
            ))
        );
    }

    #[test]
    fn case_binding_to_underscore() {
        assert_eq!(
            case_expression(
                CompleteStr(
                    "case x of
  _ ->
    42"
                ),
                0
            ),
            Ok((
                CompleteStr(""),
                Expression::Case(Box::new(var("x")), vec![(var("_"), int("42"))])
            ))
        );
    }

    #[test]
    fn case_nested() {
        assert_eq!(
            case_expression(
                CompleteStr(
                    "case x of
  a -> a
  b ->
    case y of
      a1 -> a1
      b1 -> b1
  c -> c"
                ),
                0
            ),
            Ok((
                CompleteStr(""),
                Expression::Case(
                    Box::new(var("x")),
                    vec![
                        (var("a"), var("a")),
                        (
                            var("b"),
                            Expression::Case(
                                Box::new(var("y")),
                                vec![(var("a1"), var("a1")), (var("b1"), var("b1"))],
                            ),
                        ),
                        (var("c"), var("c")),
                    ]
                )
            ))
        );
    }

    // If Expressions

    #[test]
    fn if_statement() {
        assert_eq!(
            if_expression(
                CompleteStr(
                    "if a then
  1
else 
  2"
                ),
                0
            ),
            Ok((CompleteStr(""), if_(var("a"), int("1"), int("2"))))
        );
    }

    #[test]
    fn if_statement_with_tuple() {
        assert_eq!(
            if_expression(
                CompleteStr(
                    "if (a, b) == (1, 2) then
  1
else 
  2"
                ),
                0
            ),
            Ok((
                CompleteStr(""),
                if_(
                    bin_op(
                        var("=="),
                        Expression::Tuple(vec![var("a"), var("b")]),
                        Expression::Tuple(vec![int("1"), int("2")])
                    ),
                    int("1"),
                    int("2")
                )
            ))
        );
    }

    #[test]
    fn if_statement_else_if_else() {
        assert_eq!(
            if_expression(
                CompleteStr(
                    "if a then
  1
else if b then
  2
else
  3"
                ),
                0
            ),
            Ok((
                CompleteStr(""),
                if_(var("a"), int("1"), if_(var("b"), int("2"), int("3")))
            ))
        );
    }

    // Expressions

    #[test]
    fn operator_passed_to_map() {
        assert_eq!(
            expression(CompleteStr("reduce (+) list"), 0),
            Ok((
                CompleteStr(""),
                application(application(var("reduce"), var("+")), var("list"))
            ))
        );
    }

    #[test]
    fn partial_application() {
        assert_eq!(
            expression(CompleteStr("(+) 2"), 0),
            Ok((CompleteStr(""), application(var("+"), int("2"))))
        );
    }

    #[test]
    fn case_with_as() {
        assert_eq!(
            expression(CompleteStr("case a of \nT _ as x -> 1"), 0),
            Ok((
                CompleteStr(""),
                Expression::Case(
                    Box::new(var("a")),
                    vec![(
                        Expression::BinOp(
                            Box::new(var("as")),
                            Box::new(application(var("T"), var("_"))),
                            Box::new(var("x")),
                        ),
                        int("1"),
                    )]
                )
            ))
        );
    }

    #[test]
    fn lambda_destructuring() {
        assert_eq!(
            expression(CompleteStr("\\(a,b) acc -> 1"), 0),
            Ok((
                CompleteStr(""),
                Expression::Lambda(
                    vec![Expression::Tuple(vec![var("a"), var("b")]), var("acc")],
                    Box::new(int("1")),
                )
            ))
        );
    }

    #[test]
    fn access() {
        assert_eq!(
            expression(CompleteStr("Module.a"), 0),
            Ok((
                CompleteStr(""),
                Expression::Access(Box::new(var("Module")), vec!["a".to_string()])
            ))
        );
    }

    #[test]
    fn access_function() {
        assert_eq!(
            expression(CompleteStr("map .a list"), 0),
            Ok((
                CompleteStr(""),
                application(
                    application(var("map"), Expression::AccessFunction("a".to_string())),
                    var("list")
                )
            ))
        );
    }
}
