use nom;
use nom::multispace;
use nom::types::CompleteStr;

#[derive(Debug, PartialEq, Clone)]
pub enum Expression {
    SingleValue(String),
    FunctionCall(Vec<Expression>),
    Dotted(Vec<Expression>),
    InfixCall(InfixDetails),
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

named!(single_name<CompleteStr, Expression>,
     map!(nom::alphanumeric, |v| Expression::SingleValue(v.0.to_string()))
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

fn combine(first: Expression, rest: Vec<Vec<Expression>>) -> Vec<Expression> {
    if rest.is_empty() {
        vec![first]
    } else {
        let mut flattened: Vec<Expression> = rest.iter().flat_map(|e| e.clone()).collect();
        flattened.insert(0, first);
        flattened
    }
}

named!(inner_expression<CompleteStr, Vec<Expression>>,
  do_parse!(
      first: dot_expression >>
      rest: many0!(
          do_parse!(
              tag!(" ") >>
              other: non_infix_expression >>
              (other)
          )
      ) >>
      (combine(first, rest))
  )
);

named!(operator<CompleteStr, String>,
    map!(is_a!("+-/<|>*$."), |c| c.0.to_string())
);

named!(infix_expression<CompleteStr, Expression>,
    do_parse!(
        left: inner_expression >>
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

named!(non_infix_expression<CompleteStr, Vec<Expression>>,
  alt!(
        inner_expression
      | do_parse!(
            tag!("(") >>
            subexpression: inner_expression >>
            tag!(")") >>
            rest: many0!(
                do_parse!(
                    tag!(" ") >>
                    other: expression_choice >>
                    (other)
                )
            ) >>
            (combine(Expression::FunctionCall(subexpression), rest))
        )
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

#[test]
fn parse_expression() {
    assert_eq!(
        expression(CompleteStr("1")),
        Ok((CompleteStr(""), Expression::SingleValue("1".to_string())))
    );

    assert_eq!(
        expression(CompleteStr("func value")),
        Ok((
            CompleteStr(""),
            Expression::FunctionCall(vec![
                Expression::SingleValue("func".to_string()),
                Expression::SingleValue("value".to_string()),
            ])
        ))
    );

    assert_eq!(
        expression(CompleteStr("func1 (func2 value)")),
        Ok((
            CompleteStr(""),
            Expression::FunctionCall(vec![
                Expression::SingleValue("func1".to_string()),
                Expression::FunctionCall(vec![
                    Expression::SingleValue("func2".to_string()),
                    Expression::SingleValue("value".to_string()),
                ]),
            ])
        ))
    );

    assert_eq!(
        expression(CompleteStr("1 + 2")),
        Ok((
            CompleteStr(""),
            Expression::InfixCall(InfixDetails {
                operator: "+".to_string(),
                left: Box::new(Expression::SingleValue("1".to_string())),
                right: Box::new(Expression::SingleValue("2".to_string())),
            })
        ))
    );

    assert_eq!(
        expression(CompleteStr("1 |> 2 |> 3")),
        Ok((
            CompleteStr(""),
            Expression::InfixCall(InfixDetails {
                operator: "|>".to_string(),
                left: Box::new(Expression::SingleValue("1".to_string())),
                right: Box::new(Expression::InfixCall(InfixDetails {
                    operator: "|>".to_string(),
                    left: Box::new(Expression::SingleValue("2".to_string())),
                    right: Box::new(Expression::SingleValue("3".to_string())),
                })),
            })
        ))
    );

    assert_eq!(
        expression(CompleteStr("Just 1 |> Maybe.withDefault 2")),
        Ok((
            CompleteStr(""),
            Expression::InfixCall(InfixDetails {
                operator: "|>".to_string(),
                left: Box::new(Expression::FunctionCall(vec![
                    Expression::SingleValue("Just".to_string()),
                    Expression::SingleValue("1".to_string()),
                ])),
                right: Box::new(Expression::FunctionCall(vec![
                    Expression::Dotted(vec![
                        Expression::SingleValue("Maybe".to_string()),
                        Expression::SingleValue("withDefault".to_string()),
                    ]),
                    Expression::SingleValue("2".to_string()),
                ])),
            })
        ))
    );

    assert_eq!(
        expression(CompleteStr(
            "Just 1 |> Maybe.map myFunc |> Maybe.withDefault 3"
        )),
        Ok((
            CompleteStr(""),
            Expression::InfixCall(InfixDetails {
                operator: "|>".to_string(),
                left: Box::new(Expression::FunctionCall(vec![
                    Expression::SingleValue("Just".to_string()),
                    Expression::SingleValue("1".to_string()),
                ])),
                right: Box::new(Expression::InfixCall(InfixDetails {
                    operator: "|>".to_string(),
                    left: Box::new(Expression::FunctionCall(vec![
                        Expression::Dotted(vec![
                            Expression::SingleValue("Maybe".to_string()),
                            Expression::SingleValue("map".to_string()),
                        ]),
                        Expression::SingleValue("myFunc".to_string()),
                    ])),
                    right: Box::new(Expression::FunctionCall(vec![
                        Expression::Dotted(vec![
                            Expression::SingleValue("Maybe".to_string()),
                            Expression::SingleValue("withDefault".to_string()),
                        ]),
                        Expression::SingleValue("3".to_string()),
                    ])),
                })),
            })
        ))
    );
}
