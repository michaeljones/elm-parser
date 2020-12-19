use lexer::Token;
use logos::Lexer;

#[derive(Debug)]
pub struct Module<'a> {
    name: &'a str,
    imports: Vec<Import<'a>>,
    pub statements: Vec<Stmt<'a>>,
}

#[derive(Debug)]
pub struct Import<'a> {
    module_name: &'a str,
}

#[derive(Debug)]
pub enum Stmt<'a> {
    Function { name: &'a str, expr: Expr<'a> },
}

#[derive(Debug)]
pub enum Expr<'a> {
    Integer(i32),
    BinOp {
        operator: &'a str,
        left: Box<Expr<'a>>,
        right: Box<Expr<'a>>,
    },
}

#[derive(Debug)]
pub enum Error<'a> {
    UnexpectedExpressionToken,
    UnexpectedToken {
        expected: Token<'a>,
        found: Token<'a>,
    },
    ExpectedSpace(Token<'a>),
    UnexpectedEnd,
    Indent,
    TokensRemaining,
    NoOperand,
    NoOperator,
}

pub fn parse<'a>(tokens: Lexer<'a, Token<'a>>) -> Result<Module<'a>, Error> {
    let mut iter = tokens.peekable();
    matches(&iter.next(), Token::Module)?;
    matches_space(&iter.next())?;
    let name = extract_type_or_module_name(&iter.next())?;
    matches_space(&iter.next())?;
    matches(&iter.next(), Token::Exposing)?;
    matches_space(&iter.next())?;
    matches(&iter.next(), Token::OpenParen)?;
    matches(&iter.next(), Token::Ellipsis)?;
    matches(&iter.next(), Token::CloseParen)?;

    consume_til_line_start(&mut iter);

    let imports = parse_imports(&mut iter)?;

    consume_til_line_start(&mut iter);

    let statements = parse_statements(&mut iter)?;

    if iter.next() == None {
        Ok(Module {
            name,
            imports,
            statements,
        })
    } else {
        Err(Error::TokensRemaining)
    }
}

type TokenIter<'a> = std::iter::Peekable<logos::Lexer<'a, Token<'a>>>;

fn consume_til_line_start<'a>(mut iter: &mut TokenIter<'a>) {
    while let Some(token) = iter.peek() {
        match token {
            Token::NewLine => {
                iter.next();
                consume_spaces(&mut iter);
            }
            _ => return,
        }
    }
}

fn consume_spaces(iter: &mut TokenIter) {
    while matches!(iter.peek(), Some(Token::Space(_))) {
        iter.next();
    }
}

// Imports
fn parse_imports<'a>(mut iter: &mut TokenIter<'a>) -> Result<Vec<Import<'a>>, Error<'a>> {
    let mut imports = vec![];

    loop {
        if !matches!(iter.peek(), Some(Token::Import)) {
            break;
        }

        matches(&iter.next(), Token::Import)?;
        matches_space(&iter.next())?;
        let module_name = extract_type_or_module_name(&iter.next())?;

        imports.push(Import { module_name });

        consume_til_line_start(&mut iter);
    }

    Ok(imports)
}

struct Context {
    base_indent: usize,
    current_indent: usize,
}

impl Context {
    pub fn new() -> Context {
        Context {
            base_indent: 0,
            current_indent: 0,
        }
    }

    pub fn consume_white_space<'a>(&mut self, iter: &mut TokenIter<'a>) -> Result<(), Error<'a>> {
        while let Some(ref token) = iter.peek() {
            match token {
                Token::NewLine => {
                    iter.next();
                    self.current_indent = 0
                }
                Token::Space(ref count) => {
                    self.current_indent += count;
                    iter.next();
                }
                _ => {
                    if self.current_indent > self.base_indent {
                        return Ok(());
                    } else {
                        return Err(Error::Indent);
                    }
                }
            }
        }

        Ok(())
    }
}

// Statements
fn parse_statements<'a>(mut iter: &mut TokenIter<'a>) -> Result<Vec<Stmt<'a>>, Error<'a>> {
    let mut statements = vec![];

    loop {
        if !matches!(iter.peek(), Some(Token::VarName(_))) {
            break;
        }

        let name = extract_var_name(&iter.next())?;
        matches_space(&iter.next())?;
        matches(&iter.next(), Token::Equals)?;
        let mut context = Context::new();
        context.consume_white_space(&mut iter)?;

        let expr = parse_expression(&mut iter, &mut context)?;

        statements.push(Stmt::Function { name, expr });

        consume_til_line_start(&mut iter);
    }

    Ok(statements)
}

// Expressions
//
// Shunting yard approach based on:
//   - https://eli.thegreenplace.net/2009/03/20/a-recursive-descent-parser-with-an-infix-expression-evaluator
//   - http://www.engr.mun.ca/~theo/Misc/exp_parsing.htm
//
fn parse_expression<'a>(
    mut iter: &mut TokenIter<'a>,
    context: &mut Context,
) -> Result<Expr<'a>, Error<'a>> {
    let expr = parse_singular_expression(&mut iter)?;
    context.consume_white_space(&mut iter)?;

    let mut operator_stack = Vec::new();
    let mut operand_stack = vec![expr];

    while matches!(iter.peek(), Some(Token::Operator(_))) {
        let operator = extract_operator(&iter.next())?;
        context.consume_white_space(&mut iter)?;

        manip_stacks(operator, &mut operator_stack, &mut operand_stack)?;

        let right_hand_expr = parse_singular_expression(&mut iter)?;
        context.consume_white_space(&mut iter)?;
        operand_stack.push(right_hand_expr);
    }

    while operator_stack.len() > 0 {
        let operator = operator_stack.pop().ok_or(Error::NoOperator)?;
        let right_hand_expr = operand_stack.pop().ok_or(Error::NoOperand)?;
        let left_hand_expr = operand_stack.pop().ok_or(Error::NoOperand)?;

        operand_stack.push(Expr::BinOp {
            operator,
            left: Box::new(left_hand_expr),
            right: Box::new(right_hand_expr),
        })
    }

    assert!(operand_stack.len() == 1);
    operand_stack.pop().ok_or(Error::NoOperand)
}

fn manip_stacks<'a>(
    operator: &'a str,
    mut operator_stack: &mut Vec<&'a str>,
    mut operand_stack: &mut Vec<Expr<'a>>,
) -> Result<(), Error<'a>> {
    if has_greater_precendence(operator, &operator_stack) {
        operator_stack.push(operator);
    } else {
        let right_hand_expr = operand_stack.pop().ok_or(Error::NoOperand)?;
        let left_hand_expr = operand_stack.pop().ok_or(Error::NoOperand)?;
        let stored_operator = operator_stack.pop().ok_or(Error::NoOperator)?;

        operand_stack.push(Expr::BinOp {
            operator: stored_operator,
            left: Box::new(left_hand_expr),
            right: Box::new(right_hand_expr),
        });

        manip_stacks(operator, &mut operator_stack, &mut operand_stack)?;
    };

    Ok(())
}

fn has_greater_precendence<'a>(operator_a: &'a str, operator_stack: &Vec<&'a str>) -> bool {
    if operator_stack.is_empty() {
        true
    } else {
        let precedence_a = precendence(operator_a);
        let precedence_b = operator_stack.last().map(|op| precendence(op)).unwrap_or(0);

        precedence_a > precedence_b
    }
}

fn precendence<'a>(operator: &'a str) -> usize {
    match operator {
        "*" | "/" => 7,
        "+" | "-" => 6,
        _ => 0,
    }
}

fn parse_singular_expression<'a>(iter: &mut TokenIter<'a>) -> Result<Expr<'a>, Error<'a>> {
    match iter.next() {
        Some(Token::LiteralInteger(int)) => Ok(Expr::Integer(int)),
        _ => Err(Error::UnexpectedExpressionToken),
    }
}

fn matches<'a>(stream_token: &Option<Token<'a>>, match_token: Token<'a>) -> Result<(), Error<'a>> {
    match stream_token {
        Some(token) => {
            if token == &match_token {
                Ok(())
            } else {
                Err(Error::UnexpectedToken {
                    found: token.clone(),
                    expected: match_token.clone(),
                })
            }
        }
        None => Err(Error::UnexpectedEnd),
    }
}

fn matches_space<'a>(stream_token: &Option<Token<'a>>) -> Result<(), Error<'a>> {
    match stream_token {
        Some(Token::Space(_)) => Ok(()),
        Some(token) => Err(Error::ExpectedSpace(token.clone())),
        None => Err(Error::UnexpectedEnd),
    }
}

fn extract_type_or_module_name<'a>(stream_token: &Option<Token<'a>>) -> Result<&'a str, Error<'a>> {
    match stream_token {
        Some(Token::TypeOrModuleName(name)) => Ok(name),
        Some(token) => Err(Error::UnexpectedToken {
            found: token.clone(),
            expected: Token::TypeOrModuleName(""),
        }),
        None => Err(Error::UnexpectedEnd),
    }
}

fn extract_var_name<'a>(stream_token: &Option<Token<'a>>) -> Result<&'a str, Error<'a>> {
    match stream_token {
        Some(Token::VarName(name)) => Ok(name),
        Some(token) => Err(Error::UnexpectedToken {
            found: token.clone(),
            expected: Token::VarName(""),
        }),
        None => Err(Error::UnexpectedEnd),
    }
}

fn extract_operator<'a>(stream_token: &Option<Token<'a>>) -> Result<&'a str, Error<'a>> {
    match stream_token {
        Some(Token::Operator(op)) => Ok(op),
        Some(token) => Err(Error::UnexpectedToken {
            found: token.clone(),
            expected: Token::Operator(""),
        }),
        None => Err(Error::UnexpectedEnd),
    }
}
