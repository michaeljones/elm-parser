use combine::ParseError;
use combine::Parser;

use super::comments;
use super::state::StateStream;
use super::whitespace;

/*
module Elm.Parser.Layout exposing (LayoutStatus(..), anyComment, around, compute, layout, layoutAndNewLine, layoutStrict, maybeAroundBothSides, optimisticLayout, optimisticLayoutWith)

import Combine exposing (Parser, choice, fail, many, many1, maybe, or, succeed, withLocation, withState)
import Elm.Parser.Comments as Comments
import Elm.Parser.State as State exposing (State)
import Elm.Parser.Whitespace exposing (many1Spaces, realNewLine)


anyComment : Combine.Parser State ()
anyComment =
    or
        Comments.singleLineComment
        Comments.multilineComment
*/

pub fn any_comment<Input>() -> impl Parser<StateStream<Input>, Output = ()>
where
    Input: combine::Stream<Token = char>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    comments::single_line_comment()
        .or(comments::multi_line_comment())
        .map(|_| ())
}

/*
layout : Parser State ()
layout =
    many1
        (choice
            [ anyComment
            , many1 realNewLine
                |> Combine.continueWith
                    (choice
                        [ many1Spaces
                        , anyComment
                        ]
                    )
            , many1Spaces
            ]
        )
        |> Combine.continueWith (verifyIndent (\stateIndent current -> stateIndent < current))
*/

pub fn layout<Input>() -> impl Parser<StateStream<Input>, Output = ()>
where
    Input: combine::Stream<Token = char>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    combine::many1(combine::choice((
        any_comment(),
        combine::many1(
            whitespace::real_new_line()
                .with(combine::choice((whitespace::many1_spaces(), any_comment()))),
        ),
        whitespace::many1_spaces(),
    )))
}

/*
type LayoutStatus
    = Strict
    | Indented


optimisticLayoutWith : (() -> Parser State a) -> (() -> Parser State a) -> Parser State a
optimisticLayoutWith onStrict onIndented =
    optimisticLayout
        |> Combine.andThen
            (\ind ->
                case ind of
                    Strict ->
                        onStrict ()

                    Indented ->
                        onIndented ()
            )


optimisticLayout : Parser State LayoutStatus
optimisticLayout =
    many
        (choice
            [ anyComment
            , many1 realNewLine
                |> Combine.continueWith
                    (choice
                        [ many1Spaces
                        , anyComment
                        , succeed ()
                        ]
                    )
            , many1Spaces
            ]
        )
        |> Combine.continueWith compute


compute : Parser State LayoutStatus
compute =
    withState
        (\s ->
            withLocation
                (\l ->
                    let
                        known =
                            1 :: State.storedColumns s
                    in
                    if List.member l.column known then
                        succeed Strict

                    else
                        succeed Indented
                )
        )


layoutStrict : Parser State ()
layoutStrict =
    many1
        (choice
            [ anyComment
            , many1 realNewLine |> Combine.continueWith (succeed ())
            , many1Spaces
            ]
        )
        |> Combine.continueWith (verifyIndent (\stateIndent current -> stateIndent == current))
*/

pub fn layout_strict<Input>() -> impl Parser<StateStream<Input>, Output = ()>
where
    Input: combine::Stream<Token = char>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    combine::many1(combine::choice((
        any_comment(),
        combine::skip_many1(whitespace::real_new_line()).with(combine::value(())),
        whitespace::many1_spaces(),
    )))
}

/*
verifyIndent : (Int -> Int -> Bool) -> Parser State ()
verifyIndent f =
    withState
        (\s ->
            withLocation
                (\l ->
                    if f (State.expectedColumn s) l.column then
                        succeed ()

                    else
                        fail ("Expected higher indent than " ++ String.fromInt l.column)
                )
        )
*/

/*
around : Parser State b -> Parser State b
around x =
    layout
        |> Combine.continueWith x
        |> Combine.ignore layout
*/

pub fn around<Input, P>(p: P) -> impl Parser<StateStream<Input>, Output = P::Output>
where
    Input: combine::Stream<Token = char>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
    P: Parser<StateStream<Input>>,
{
    layout().with(p).skip(layout())
}

/*
maybeAroundBothSides : Parser State b -> Parser State b
maybeAroundBothSides x =
    maybe layout
        |> Combine.continueWith x
        |> Combine.ignore (maybe layout)
*/

pub fn optional_around_both_sides<Input, P>(
    p: P,
) -> impl Parser<StateStream<Input>, Output = P::Output>
where
    Input: combine::Stream<Token = char>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
    P: Parser<StateStream<Input>>,
{
    combine::optional(layout())
        .with(p)
        .skip(combine::optional(layout()))
}

/*
layoutAndNewLine : Combine.Parser State ()
layoutAndNewLine =
    maybe layout
        |> Combine.ignore (many1 realNewLine)
        |> Combine.continueWith (succeed ())
*/

#[cfg(test)]
mod tests {

    use super::*;
    use combine::Parser;

    #[test]
    fn any_comment_1() {
        assert_eq!(any_comment().parse("-- bob"), Ok(((), "")));
    }

    #[test]
    fn any_comment_2() {
        assert_eq!(any_comment().parse("-- bob\n "), Ok(((), "\n ")));
    }

    #[test]
    fn layout_1() {
        assert_eq!(layout().parse(" "), Ok(((), "")));
    }

    #[test]
    fn layout_strict_1() {
        assert_eq!(layout_strict().parse("\n"), Ok(((), "")));
    }

    // #[test]
    // fn around_1() {
    //     assert_eq!(
    //         around(combine::parser::char::string(":")).parse(" : "),
    //         Ok((":", ""))
    //     );
    // }
}
