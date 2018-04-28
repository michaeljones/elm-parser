use nom;
use nom::{alphanumeric, digit, multispace};
use nom::types::CompleteStr;

/// Tests if byte is ASCII alphabetic: A-Z, a-z
#[inline]
pub fn is_lowercase(chr: char) -> bool {
    (chr >= 'A' && chr <= 'Z')
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expression {
    FunctionCall(Vec<Expression>),
    Dotted(Vec<Expression>),
    InfixCall(InfixDetails),
    Int(String),
    Float(String),
    Variable(String),
    Bracketed(Vec<Expression>),
}

#[derive(Debug, PartialEq, Clone)]
pub struct InfixDetails {
    operator: String,
    left: Box<Expression>,
    right: Box<Expression>,
}

fn combine_dot(first: Expression, mut rest: Vec<Expression>) -> Expression {
    if rest.is_empty() {
        first
    } else {
        rest.insert(0, first);
        Expression::Dotted(rest)
    }
}

named!(int<CompleteStr, Expression>,
  map!(nom::digit, |v| Expression::Int(v.0.to_string()))
);

named!(float<CompleteStr, Expression>,
  do_parse!(
    start: digit >>
    char!('.') >>
    end: digit >>
    (Expression::Float(start.0.to_owned() + "." + end.0))
  )
);

named!(variable<CompleteStr, Expression>,
  do_parse!(
    start: take_while!(is_lowercase) >>
    rest: alphanumeric >>
    (Expression::Variable(start.0.to_owned() + rest.0))
  )
);

named!(single_name<CompleteStr, Expression>,
  alt!(
       int
     | float
     | variable
  )
);

named!(dot_expression<CompleteStr, Expression>,
  do_parse!(
      first: single_name >>
      rest: many0!(
          do_parse!(
              tag!(".") >>
              other: single_name >>
              (other)
          )
      ) >>
      (combine_dot(first, rest))
  )
);

named!(operator<CompleteStr, String>,
    map!(is_a!("+-/<|>*$."), |c| c.0.to_string())
);

named!(bracketed_expression<CompleteStr, Expression>,
  do_parse!(
      tag!("(") >>
      subexpression: expression_choice >>
      tag!(")") >>
      (Expression::Bracketed(subexpression))
  )
);

named!(non_infix_expression<CompleteStr, Vec<Expression>>,
  separated_list!(
      multispace,
      alt!(
            bracketed_expression
          | float
          | int
          | dot_expression 
      )
  )
);

named!(infix_expression<CompleteStr, Expression>,
    do_parse!(
        left: non_infix_expression >>
        multispace >>
        operator: operator >>
        multispace >>
        right: expression_choice >>
        (Expression::InfixCall(InfixDetails {
            operator: operator,
            left: Box::new(choose_expression(left)),
            right: Box::new(choose_expression(right))
        }))
    )
);

named!(expression_choice<CompleteStr, Vec<Expression>>,
  alt!(
        map!(infix_expression, |e| vec![e])
      | non_infix_expression
  )
);

fn choose_expression(data: Vec<Expression>) -> Expression {
    if data.len() == 1 {
        data[0].clone()
    } else {
        Expression::FunctionCall(data)
    }
}

named!(pub expression<CompleteStr, Expression>,
  map!(expression_choice, choose_expression)
);

#[cfg(test)]
fn s(s_: &str) -> String {
    s_.to_string()
}

#[test]
fn parse_int() {
    assert_eq!(
        expression(CompleteStr("1")),
        Ok((CompleteStr(""), Expression::Int(s("1"))))
    );
}

#[test]
fn parse_float() {
    assert_eq!(
        expression(CompleteStr("1.01")),
        Ok((CompleteStr(""), Expression::Float(s("1.01"))))
    );
}

#[test]
fn parse_function_call() {
    assert_eq!(
        expression(CompleteStr("func value")),
        Ok((
            CompleteStr(""),
            Expression::FunctionCall(vec![
                Expression::Variable("func".to_string()),
                Expression::Variable("value".to_string()),
            ])
        ))
    );
}

#[test]
fn parse_bracketed() {
    assert_eq!(
        expression(CompleteStr("func1 (func2 value)")),
        Ok((
            CompleteStr(""),
            Expression::FunctionCall(vec![
                Expression::Variable("func1".to_string()),
                Expression::Bracketed(vec![
                    Expression::Variable("func2".to_string()),
                    Expression::Variable("value".to_string()),
                ]),
            ])
        ))
    );
}

#[test]
fn parse_simple_plus() {
    assert_eq!(
        expression(CompleteStr("1 + 2")),
        Ok((
            CompleteStr(""),
            Expression::InfixCall(InfixDetails {
                operator: "+".to_string(),
                left: Box::new(Expression::Int(s("1"))),
                right: Box::new(Expression::Int(s("2"))),
            })
        ))
    );
}

#[test]
fn parse_simple_pipeline() {
    assert_eq!(
        expression(CompleteStr("1 |> 2 |> 3")),
        Ok((
            CompleteStr(""),
            Expression::InfixCall(InfixDetails {
                operator: "|>".to_string(),
                left: Box::new(Expression::Int(s("1"))),
                right: Box::new(Expression::InfixCall(InfixDetails {
                    operator: "|>".to_string(),
                    left: Box::new(Expression::Int(s("2"))),
                    right: Box::new(Expression::Int(s("3"))),
                })),
            })
        ))
    );
}

#[test]
fn parse_dotted_pipeline() {
    assert_eq!(
        expression(CompleteStr("Just 1 |> Maybe.withDefault 2")),
        Ok((
            CompleteStr(""),
            Expression::InfixCall(InfixDetails {
                operator: "|>".to_string(),
                left: Box::new(Expression::FunctionCall(vec![
                    Expression::Variable("Just".to_string()),
                    Expression::Int(s("1")),
                ])),
                right: Box::new(Expression::FunctionCall(vec![
                    Expression::Dotted(vec![
                        Expression::Variable("Maybe".to_string()),
                        Expression::Variable("withDefault".to_string()),
                    ]),
                    Expression::Int(s("2")),
                ])),
            })
        ))
    );
}

#[test]
fn parse_expression() {
    assert_eq!(
        expression(CompleteStr(
            "Just 1 |> Maybe.map myFunc |> Maybe.withDefault 3"
        )),
        Ok((
            CompleteStr(""),
            Expression::InfixCall(InfixDetails {
                operator: "|>".to_string(),
                left: Box::new(Expression::FunctionCall(vec![
                    Expression::Variable("Just".to_string()),
                    Expression::Int("1".to_string()),
                ])),
                right: Box::new(Expression::InfixCall(InfixDetails {
                    operator: "|>".to_string(),
                    left: Box::new(Expression::FunctionCall(vec![
                        Expression::Dotted(vec![
                            Expression::Variable("Maybe".to_string()),
                            Expression::Variable("map".to_string()),
                        ]),
                        Expression::Variable("myFunc".to_string()),
                    ])),
                    right: Box::new(Expression::FunctionCall(vec![
                        Expression::Dotted(vec![
                            Expression::Variable("Maybe".to_string()),
                            Expression::Variable("withDefault".to_string()),
                        ]),
                        Expression::Int("3".to_string()),
                    ])),
                })),
            })
        ))
    );
}
