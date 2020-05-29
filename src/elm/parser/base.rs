use combine::parser::char::char;
use combine::{sep_by1, Parser};

use elm::syntax::modulename::ModuleName;

use super::tokens::type_name;

type Input<'a> = &'a str;

pub fn module_name<'a>() -> impl Parser<Input<'a>, Output = ModuleName> {
    sep_by1(type_name(), char('.'))
}

#[cfg(test)]
mod tests {

    use super::*;
    use combine::Parser;

    #[test]
    fn module_name_1() {
        assert_eq!(
            module_name().parse("Aaa"),
            Ok((vec!["Aaa".to_string()], ""))
        );
    }

    #[test]
    fn module_name_2() {
        assert_eq!(
            module_name().parse("Aaa.Bb.Cd"),
            Ok((
                vec!["Aaa".to_string(), "Bb".to_string(), "Cd".to_string()],
                ""
            ))
        );
    }
}

/*
module Elm.Parser.Base exposing (moduleName, typeIndicator)

import Combine exposing (Parser, sepBy1, string)
import Elm.Parser.Ranges exposing (withRange)
import Elm.Parser.State exposing (State)
import Elm.Parser.Tokens as Tokens
import Elm.Syntax.ModuleName exposing (ModuleName)


moduleName : Parser s ModuleName
moduleName =
    sepBy1 (string ".") Tokens.typeName


typeIndicator : Parser s ( ModuleName, String )
typeIndicator =
    let
        helper ( n, xs ) =
            Combine.choice
                [ string "."
                    |> Combine.continueWith Tokens.typeName
                    |> Combine.andThen (\t -> helper ( t, n :: xs ))
                , Combine.succeed ( n, xs )
                ]
    in
    Tokens.typeName
        |> Combine.andThen (\t -> helper ( t, [] ))
        |> Combine.map (\( t, xs ) -> ( List.reverse xs, t ))
*/
