use combine::parser::char::{char, string};
use combine::Parser;

type Input<'a> = &'a str;

pub fn many1_spaces<'a>() -> impl Parser<Input<'a>, Output = ()> {
    combine::parser::range::take_while1(|c: char| c == ' ').map(|_| ())
}

pub fn n_spaces<'a>(n: usize) -> impl Parser<Input<'a>, Output = Vec<char>> {
    combine::count_min_max::<Vec<_>, _, _>(n, n, combine::token(' '))
}

pub fn real_new_line<'a>() -> impl Parser<Input<'a>, Output = String> {
    (combine::optional(char('\r')), string("\n")).map(|(r, n): (Option<char>, &'static str)| {
        (r.map(|c| c.to_string()).unwrap_or("".to_string())) + n
    })
}

pub fn until_new_line_token<'a>() -> impl Parser<Input<'a>, Output = &'a str> {
    combine::parser::range::take_while(|c: char| c != '\n' && c != '\r')
}

#[cfg(test)]
mod tests {

    use super::*;
    use combine::Parser;

    #[test]
    fn n_spaces_1() {
        assert_eq!(
            n_spaces(5).parse("     "),
            Ok((vec![' ', ' ', ' ', ' ', ' '], ""))
        );
    }

    #[test]
    fn n_spaces_2() {
        assert_eq!(n_spaces(1).parse("  "), Ok((vec![' '], " ")));
    }

    #[test]
    fn real_new_line_1() {
        assert_eq!(real_new_line().parse("\n"), Ok(("\n".to_string(), "")));
    }

    #[test]
    fn real_new_line_2() {
        assert_eq!(real_new_line().parse("\r\n"), Ok(("\r\n".to_string(), "")));
    }

    #[test]
    fn real_new_line_3() {
        assert!(real_new_line().parse("abc").is_err());
    }

    #[test]
    fn until_new_line_token_1() {
        assert_eq!(until_new_line_token().parse("abc\n"), Ok(("abc", "\n")));
    }

    #[test]
    fn many1_spaces_1() {
        assert_eq!(many1_spaces().parse("  "), Ok(((), "")));
    }

    #[test]
    fn many1_spaces_2() {
        assert_eq!(many1_spaces().parse("  \n"), Ok(((), "\n")));
    }
}

/*
module Elm.Parser.Whitespace exposing (many1Spaces, manySpaces, nSpaces, realNewLine, untilNewlineToken)

import Combine exposing (Parser)
import Parser as Core exposing ((|.), (|=), Step(..))


nSpaces : Int -> Parser s String
nSpaces x =
    let
        helper : Int -> Core.Parser (Step Int String)
        helper n =
            if n == 0 then
                Core.succeed (Done (String.repeat x " "))

            else
                Core.succeed (\_ -> Loop (n - 1))
                    |= Core.token " "
    in
    Core.loop x helper
        |> Combine.fromCore


manySpaces : Parser s ()
manySpaces =
    Combine.fromCore (Core.chompWhile (\c -> c == ' '))


many1Spaces : Parser s ()
many1Spaces =
    Core.token " "
        |. Core.chompWhile (\c -> c == ' ')
        |> Combine.fromCore


realNewLine : Parser s String
realNewLine =
    Core.getChompedString
        (Core.succeed ()
            |. Core.oneOf [ Core.chompIf ((==) '\u{000D}'), Core.succeed () ]
            |. Core.symbol "\n"
        )
        |> Combine.fromCore


untilNewlineToken : Parser s String
untilNewlineToken =
    Core.getChompedString (Core.chompWhile (\c -> c /= '\u{000D}' && c /= '\n'))
        |> Combine.fromCore
        */
