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
    Function { name: &'a str, expr: Expr },
}

#[derive(Debug)]
pub enum Expr {
    Integer(i32),
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
}

pub fn parse<'a>(tokens: Lexer<'a, Token<'a>>) -> Result<Module<'a>, Error> {
    let mut iter = tokens.peekable();
    while let Some(token) = iter.next() {
        matches(&Some(token), Token::Module)?;
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

        return Ok(Module {
            name,
            imports,
            statements,
        });
    }

    Err(Error::UnexpectedEnd)
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
        matches_space(&iter.next())?;
        let expr = parse_expression(&mut iter)?;

        statements.push(Stmt::Function { name, expr });

        consume_til_line_start(&mut iter);

        // Consume everything else
        while let Some(_token) = iter.next() {}
    }

    Ok(statements)
}

// Expressions
fn parse_expression<'a>(iter: &mut TokenIter<'a>) -> Result<Expr, Error<'a>> {
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
        Some(token) => Err(Error::ExpectedSpace(token.clone())),
        None => Err(Error::UnexpectedEnd),
    }
}
