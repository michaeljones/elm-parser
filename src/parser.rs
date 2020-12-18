use lexer::Token;
use logos::Lexer;

#[derive(Debug)]
pub struct Module<'a> {
    name: &'a str,
}

#[derive(Debug)]
pub enum Error<'a> {
    UnexpectedToken {
        found: Token<'a>,
        expected: Token<'a>,
    },
    ExpectedSpace(Token<'a>),
    UnexpectedEnd,
}

pub fn parse<'a>(tokens: Lexer<'a, Token<'a>>) -> Result<Module<'a>, Error> {
    let mut iter = tokens.peekable();
    while let Some(token) = iter.next() {
        println!("{:?}", &token);
        matches(&Some(token), Token::Module)?;
        matches_space(&iter.next())?;
        let name = extract_type_or_module_name(&iter.next())?;
        matches_space(&iter.next())?;
        matches(&iter.next(), Token::Exposing)?;
        matches_space(&iter.next())?;
        matches(&iter.next(), Token::OpenParen)?;
        matches(&iter.next(), Token::Ellipsis)?;
        matches(&iter.next(), Token::CloseParen)?;

        return Ok(Module { name });
    }

    Err(Error::UnexpectedEnd)
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
