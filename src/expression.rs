use nom;
// use nom::alphanumeric;
use nom::types::CompleteStr;

#[derive(Debug, PartialEq, Clone)]
pub enum Expression {
    SingleValue(String),
    FunctionCall(Vec<Expression>),
    InfixCall(InfixDetails),
}

#[derive(Debug, PartialEq, Clone)]
pub struct InfixDetails {
    operator: String,
    left: Box<Expression>,
    right: Box<Expression>,
}

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
      first: map!(nom::alphanumeric, |v| Expression::SingleValue(v.0.to_string())) >>
      rest: many0!(
          do_parse!(
              tag!(" ") >>
              other: expression_choice >>
              (other)
          )
      ) >>
      (combine(first, rest))
  )
);

named!(expression_choice<CompleteStr, Vec<Expression>>,
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
}
