use combine::error::ParseError;
use combine::parser::char::string;
use combine::{Parser, RangeStream};

use super::whitespace::until_new_line_token;

pub fn single_line_comment<'a, I>() -> impl Parser<Input = I, Output = &'a str> + 'a
where
    I: 'a,
    I: RangeStream<Item = char, Range = &'a str>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    string("--").with(until_new_line_token())
}

pub fn multi_line_comment<'a, I>() -> impl Parser<Input = I, Output = &'a str> + 'a
where
    I: 'a,
    I: RangeStream<Item = char, Range = &'a str>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    // FIX
    single_line_comment()
}

#[cfg(test)]
mod tests {

    use super::*;
    use combine::Parser;

    #[test]
    fn single_line_comment_1() {
        assert_eq!(
            single_line_comment().parse("-- bob \n"),
            Ok((" bob ", "\n"))
        );
    }
}
/*
module Elm.Parser.Comments exposing (multilineComment, singleLineComment)

import Combine exposing (Parser, count, lazy, modifyState, string, succeed)
import Elm.Parser.Node as Node
import Elm.Parser.Ranges exposing (withRange)
import Elm.Parser.State exposing (State, addComment)
import Elm.Parser.Whitespace exposing (untilNewlineToken)
import Elm.Syntax.Node exposing (Node)
import Parser as Core exposing (Nestable(..))


addCommentToState : Parser State (Node String) -> Parser State ()
addCommentToState p =
    p |> Combine.andThen (\pair -> modifyState (addComment pair) |> Combine.continueWith (succeed ()))


parseComment : Parser State String -> Parser State ()
parseComment commentParser =
    Node.parser commentParser |> addCommentToState


singleLineComment : Parser State ()
singleLineComment =
    parseComment
        (succeed (++)
            |> Combine.andMap (string "--")
            |> Combine.andMap untilNewlineToken
        )


multilineCommentInner : Parser State String
multilineCommentInner =
    Core.getChompedString (Core.multiComment "{-" "-}" Nestable)
        |> Combine.fromCore


multilineComment : Parser State ()
multilineComment =
    lazy (\() -> parseComment multilineCommentInner)
    */
