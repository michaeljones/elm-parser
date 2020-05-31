use combine::{ParseError, Parser, RangeStream};

pub fn port_declaration<'a, I>() -> impl Parser<StateStream<Input> = I, Output = &'static str> + 'a
where
    I: 'a,
    I: RangeStream<Item = char, Range = &'a str>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    tokens::port_token()
}

/*
module Elm.Parser.Numbers exposing (forgivingNumber, number)

import Combine exposing (Parser)
import Elm.Parser.State exposing (State)
import Parser as Core exposing ((|.), (|=))


raw : (Float -> a) -> (Int -> a) -> (Int -> a) -> Core.Parser a
raw floatf intf hexf =
    Core.number
        { int = Just intf
        , hex = Just hexf
        , octal = Nothing
        , binary = Nothing
        , float = Just floatf
        }


{-| Strange case that a number is consumes and does not function in a `oneOf`
-}
forgivingNumber : (Float -> a) -> (Int -> a) -> (Int -> a) -> Parser State a
forgivingNumber floatf intf hexf =
    Core.backtrackable (raw floatf intf hexf)
        |> Combine.fromCore


number : (Float -> a) -> (Int -> a) -> (Int -> a) -> Parser State a
number floatf intf hexf =
    raw floatf intf hexf
        |> Combine.fromCore
        */
