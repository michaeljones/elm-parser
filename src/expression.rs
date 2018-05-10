use nom;
use nom::{alphanumeric, digit, multispace};
use nom::types::CompleteStr;

/// Tests if byte is ASCII: a-z
#[inline]
pub fn is_lowercase(chr: char) -> bool {
    (chr >= 'a' && chr <= 'z')
}

/// Tests if byte is ASCII: A-Z
#[inline]
pub fn is_uppercase(chr: char) -> bool {
    (chr >= 'A' && chr <= 'Z')
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expression {
    FunctionCall(Vec<Expression>),
    Dotted(Vec<Expression>),
    InfixCall(InfixDetails),
    LetBlock(Vec<AssignmentDetails>),
    Int(String),
    Float(String),
    Variable(String),
    Module(String),
    TypeConstructor(String),
    Bracketed(Vec<Expression>),
}

#[derive(Debug, PartialEq, Clone)]
pub struct InfixDetails {
    operator: String,
    left: Box<Expression>,
    right: Box<Expression>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct AssignmentDetails {
    name: String,
    expression: Box<Expression>,
}

fn at_least(spaces: CompleteStr, indentation: u32) -> Result<u32, String> {
    let length = spaces.0.len() as u32;
    // Expect to have more than the indentation
    if length > indentation {
        Ok(length)
    } else {
        Err(spaces.to_string())
    }
}

fn exactly(spaces: CompleteStr, indentation: u32) -> Result<u32, String> {
    let length = spaces.len() as u32;
    // Expect to have exactly the indentation
    if length == indentation {
        Ok(length)
    } else {
        Err(spaces.to_string())
    }
}

named_args!(at_least_indent(indentation: u32) <CompleteStr, u32>,
  map_res!(is_a!(" "), |s| at_least(s, indentation))
);

named_args!(exactly_indent(indentation: u32) <CompleteStr, u32>,
  map_res!(is_a!(" "), |s| exactly(s, indentation))
);

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

named!(variable_name<CompleteStr, String>,
  do_parse!(
    start: take_while_m_n!(1, 1, is_lowercase) >>
    rest: alphanumeric >>
    (start.0.to_owned() + rest.0)
  )
);

named!(variable<CompleteStr, Expression>,
   map!(variable_name, Expression::Variable)
);

named!(module_name<CompleteStr, String>,
  do_parse!(
    start: take_while_m_n!(1, 1, is_uppercase) >>
    rest: alphanumeric >>
    (start.0.to_owned() + rest.0)
  )
);

named!(module<CompleteStr, Expression>,
   map!(module_name, Expression::Module)
);

named!(type_constructor_name<CompleteStr, String>,
  do_parse!(
    start: take_while_m_n!(1, 1, is_uppercase) >>
    rest: alphanumeric >>
    (start.0.to_owned() + rest.0)
  )
);

named!(type_constructor<CompleteStr, Expression>,
   map!(type_constructor_name, Expression::TypeConstructor)
);

fn combine_dot(
    mut modules: Vec<Expression>,
    mut variables: Vec<Expression>,
    variable_or_type: Expression,
) -> Expression {
    if modules.len() > 0 || variables.len() > 0 {
        let mut result = vec![];
        result.append(&mut modules);
        result.append(&mut variables);
        result.push(variable_or_type);
        Expression::Dotted(result)
    } else {
        variable_or_type
    }
}

named!(dot_expression<CompleteStr, Expression>,
  do_parse!(
      modules: many0!(
        do_parse!(name: module >> char!('.') >> (name))
      ) >>
      variables: many0!(
        do_parse!(name: variable >> char!('.') >> (name))
      ) >>
      variable_or_type: 
          alt!(
                variable
              | type_constructor
          ) >>
      (combine_dot(modules, variables, variable_or_type))
  )
);

named!(operator<CompleteStr, String>,
    map!(is_a!("+-/<|>*$."), |c| c.0.to_string())
);

named_args!(bracketed_expression(indentation: u32) <CompleteStr, Expression>,
  do_parse!(
      tag!("(") >>
      subexpression: call!(expression_choice, indentation) >>
      tag!(")") >>
      (Expression::Bracketed(subexpression))
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

named_args!(non_infix_expression(indentation: u32) <CompleteStr, Vec<Expression>>,
  separated_list!(
      call!(spaces_or_new_line_and_indent, indentation),
      alt!(
            call!(bracketed_expression, indentation)
          | float
          | int
          | dot_expression 
      )
  )
);

named_args!(infix_expression(indentation: u32) <CompleteStr, Expression>,
    do_parse!(
        left: map_res!(call!(non_infix_expression, indentation), choose_expression) >>
        multispace >>
        operator: operator >>
        multispace >>
        right: map_res!(call!(expression_choice, indentation), choose_expression) >>
        (Expression::InfixCall(InfixDetails {
            operator: operator,
            left: Box::new(left),
            right: Box::new(right)
        }))
    )
);

named_args!(assignment(indentation: u32) <CompleteStr, AssignmentDetails>,
   do_parse!(
       name: variable_name >>
       tag!(" =\n") >>
       expression_indentation: call!(at_least_indent, indentation) >>
       expression: call!(expression, expression_indentation) >>
       (AssignmentDetails {
           name: name,
           expression: Box::new(expression)
       })
   )
);

named_args!(new_line_and_exact_indent(indentation: u32) <CompleteStr, (char, u32)>, 
    tuple!(
        char!('\n'),
        call!(exactly_indent, indentation)
    )
);

named_args!(assignments(indentation: u32) <CompleteStr, Vec<AssignmentDetails>>,
    separated_list!(call!(new_line_and_exact_indent, indentation), call!(assignment, indentation))
);

named_args!(let_block(indentation: u32) <CompleteStr, Expression>,
   do_parse!(
       tag!("let") >>
       char!('\n') >>
       assignment_indentation: call!(at_least_indent, indentation) >>
       assignments: call!(assignments, assignment_indentation) >>
       char!('\n') >>
       call!(exactly_indent, indentation) >>
       tag!("in") >>
       (Expression::LetBlock(assignments))
   )
);

named_args!(expression_choice(indentation: u32) <CompleteStr, Vec<Expression>>,
  alt!(
        map!(call!(let_block, indentation), |e| vec![e])
      | map!(call!(infix_expression, indentation), |e| vec![e])
      | call!(non_infix_expression, indentation)
  )
);

fn choose_expression(data: Vec<Expression>) -> Result<Expression, String> {
    if data.len() == 1 {
        Ok(data[0].clone())
    } else if data.is_empty() {
        Err("Empty expression".to_string())
    } else {
        Ok(Expression::FunctionCall(data))
    }
}

named_args!(pub expression(indentation: u32) <CompleteStr, Expression>,
  map_res!(call!(expression_choice, indentation), choose_expression)
);

#[cfg(test)]
fn s(s_: &str) -> String {
    s_.to_string()
}

#[test]
fn parse_int() {
    assert_eq!(
        int(CompleteStr("1")),
        Ok((CompleteStr(""), Expression::Int(s("1"))))
    );
}

#[test]
fn parse_float() {
    assert_eq!(
        float(CompleteStr("1.01")),
        Ok((CompleteStr(""), Expression::Float(s("1.01"))))
    );
}

#[test]
fn parse_variable() {
    assert_eq!(
        variable(CompleteStr("value")),
        Ok((CompleteStr(""), Expression::Variable("value".to_string()),))
    );
}

#[test]
fn fails_uppercase_variable() {
    assert!(variable(CompleteStr("Value")).is_err());
}

#[test]
fn parse_function_call() {
    assert_eq!(
        expression(CompleteStr("func value"), 0),
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
        expression(CompleteStr("func1 (func2 value)"), 0),
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
        expression(CompleteStr("1 + 2"), 0),
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
        expression(CompleteStr("1 |> 2 |> 3"), 0),
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
        expression(CompleteStr("Just 1 |> Maybe.withDefault 2"), 0),
        Ok((
            CompleteStr(""),
            Expression::InfixCall(InfixDetails {
                operator: "|>".to_string(),
                left: Box::new(Expression::FunctionCall(vec![
                    Expression::TypeConstructor("Just".to_string()),
                    Expression::Int(s("1")),
                ])),
                right: Box::new(Expression::FunctionCall(vec![
                    Expression::Dotted(vec![
                        Expression::Module("Maybe".to_string()),
                        Expression::Variable("withDefault".to_string()),
                    ]),
                    Expression::Int(s("2")),
                ])),
            })
        ))
    );
}

#[test]
fn parse_complex_pipeline() {
    assert_eq!(
        expression(
            CompleteStr("Just 1 |> Maybe.map myFunc |> Maybe.withDefault 3"),
            0
        ),
        Ok((
            CompleteStr(""),
            Expression::InfixCall(InfixDetails {
                operator: "|>".to_string(),
                left: Box::new(Expression::FunctionCall(vec![
                    Expression::TypeConstructor("Just".to_string()),
                    Expression::Int("1".to_string()),
                ])),
                right: Box::new(Expression::InfixCall(InfixDetails {
                    operator: "|>".to_string(),
                    left: Box::new(Expression::FunctionCall(vec![
                        Expression::Dotted(vec![
                            Expression::Module("Maybe".to_string()),
                            Expression::Variable("map".to_string()),
                        ]),
                        Expression::Variable("myFunc".to_string()),
                    ])),
                    right: Box::new(Expression::FunctionCall(vec![
                        Expression::Dotted(vec![
                            Expression::Module("Maybe".to_string()),
                            Expression::Variable("withDefault".to_string()),
                        ]),
                        Expression::Int("3".to_string()),
                    ])),
                })),
            })
        ))
    );
}

#[test]
fn parse_simple_assignment() {
    assert_eq!(
        assignment(CompleteStr("myvar =\n     1"), 4),
        Ok((
            CompleteStr(""),
            AssignmentDetails {
                name: s("myvar"),
                expression: Box::new(Expression::Int(s("1"))),
            },
        ))
    );
}

#[test]
fn parse_simple_assignments() {
    assert_eq!(
        assignments(
            CompleteStr(
                "myvar =
        1
    myother =
        bob"
            ),
            4
        ),
        Ok((
            CompleteStr(""),
            vec![
                AssignmentDetails {
                    name: s("myvar"),
                    expression: Box::new(Expression::Int(s("1"))),
                },
                AssignmentDetails {
                    name: s("myother"),
                    expression: Box::new(Expression::Variable(s("bob"))),
                },
            ],
        ))
    );
}

#[test]
fn parse_simple_let_block() {
    assert_eq!(
        let_block(
            CompleteStr(
                "let
        myvar =
            1
        myother =
            2
    in"
            ),
            4
        ),
        Ok((
            CompleteStr(""),
            Expression::LetBlock(vec![
                AssignmentDetails {
                    name: s("myvar"),
                    expression: Box::new(Expression::Int(s("1"))),
                },
                AssignmentDetails {
                    name: s("myother"),
                    expression: Box::new(Expression::Int(s("2"))),
                },
            ])
        ))
    );
}
