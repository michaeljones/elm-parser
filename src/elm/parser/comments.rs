use combine::parser::char::string;
use combine::Parser;

use super::whitespace::until_new_line_token;

type Input<'a> = &'a str;

pub fn single_line_comment<'a>() -> impl Parser<Input<'a>, Output = &'a str> {
    string("--").with(until_new_line_token())
}

pub fn multi_line_comment<'a>() -> impl Parser<Input<'a>, Output = &'a str> {
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
