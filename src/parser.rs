use lexer::Token;
use logos::Lexer;

#[derive(Debug)]
pub struct Module<'a> {
    name: &'a str,
    imports: Vec<Import<'a>>,
}

#[derive(Debug)]
pub struct Import<'a> {
    module_name: &'a str,
}

#[derive(Debug)]
pub enum Error<'a> {
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

        return Ok(Module { name, imports });
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
        Some(token) => Err(Error::ExpectedSpace(token.clone())),
        None => Err(Error::UnexpectedEnd),
    }
}
