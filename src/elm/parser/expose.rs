use combine::parser::char::{char, space, spaces, string};
use combine::parser::range::take_while1;
use combine::{between, choice, sep_by, value, Parser};

use super::tokens::{exposing_token, function_name, type_name};
use elm::syntax::exposing::{Exposing, TopLevelExpose};

type Input<'a> = &'a str;

pub fn expose_definition<'a>() -> impl Parser<Input<'a>, Output = Exposing> {
    let type_expose = type_name().map(TopLevelExpose::TypeOrAliasExpose);
    let function_expose = function_name().map(TopLevelExpose::FunctionExpose);
    let infix_expose = between(char('('), char(')'), take_while1(|c: char| c != ')'))
        .map(|name: &str| TopLevelExpose::InfixExpose(name.to_string()));

    let exposable = choice((type_expose, infix_expose, function_expose));

    let list = sep_by::<Vec<_>, _, _, _>(exposable, char(',')).map(Exposing::Explicit);

    let all = string("..").with(value(Exposing::All));

    exposing_token()
        .skip(space())
        .skip(spaces())
        .skip(char('('))
        .with(all.or(list))
        .skip(char(')'))
}

#[cfg(test)]
mod tests {

    use super::*;
    use combine::Parser;

    #[test]
    fn exposing_all() {
        assert_eq!(
            expose_definition().parse("exposing (..)"),
            Ok((Exposing::All, ""))
        );
    }

    #[test]
    fn exposing_types() {
        assert_eq!(
            expose_definition().parse("exposing (Abc)"),
            Ok((
                Exposing::Explicit(vec![TopLevelExpose::TypeOrAliasExpose("Abc".to_string())]),
                ""
            ))
        );
    }

    #[test]
    fn exposing_functions() {
        assert_eq!(
            expose_definition().parse("exposing (abc,def)"),
            Ok((
                Exposing::Explicit(vec![
                    TopLevelExpose::FunctionExpose("abc".to_string()),
                    TopLevelExpose::FunctionExpose("def".to_string())
                ]),
                ""
            ))
        );
    }

    #[test]
    fn exposing_infixes() {
        assert_eq!(
            expose_definition().parse("exposing ((++),(--))"),
            Ok((
                Exposing::Explicit(vec![
                    TopLevelExpose::InfixExpose("++".to_string()),
                    TopLevelExpose::InfixExpose("--".to_string())
                ]),
                ""
            ))
        );
    }

    #[test]
    fn exposing_mix() {
        assert_eq!(
            expose_definition().parse("exposing (Abc,abc,(--))"),
            Ok((
                Exposing::Explicit(vec![
                    TopLevelExpose::TypeOrAliasExpose("Abc".to_string()),
                    TopLevelExpose::FunctionExpose("abc".to_string()),
                    TopLevelExpose::InfixExpose("--".to_string())
                ]),
                ""
            ))
        );
    }
}

/*
module Elm.Parser.Expose exposing (exposable, exposeDefinition, exposingListInner, infixExpose, typeExpose)

import Combine exposing (Parser, choice, maybe, or, parens, sepBy, string, succeed, while)
import Combine.Char exposing (char)
import Elm.Parser.Layout as Layout
import Elm.Parser.Node as Node
import Elm.Parser.Ranges exposing (withRange)
import Elm.Parser.State exposing (State)
import Elm.Parser.Tokens exposing (exposingToken, functionName, typeName)
import Elm.Syntax.Exposing exposing (ExposedType, Exposing(..), TopLevelExpose(..))
import Elm.Syntax.Node as Node exposing (Node)


exposeDefinition : Parser State Exposing
exposeDefinition =
    exposingToken
        |> Combine.continueWith (maybe Layout.layout)
        |> Combine.continueWith exposeListWith


exposeListWith : Parser State Exposing
exposeListWith =
    parens (Layout.optimisticLayout |> Combine.continueWith exposingListInner |> Combine.ignore Layout.optimisticLayout)


exposingListInner : Parser State Exposing
exposingListInner =
    Combine.lazy
        (\() ->
            or (withRange (succeed All |> Combine.ignore (Layout.maybeAroundBothSides (string ".."))))
                (Combine.map Explicit (sepBy (char ',') (Layout.maybeAroundBothSides exposable)))
        )


exposable : Parser State (Node TopLevelExpose)
exposable =
    Combine.lazy
        (\() ->
            choice
                [ typeExpose
                , infixExpose
                , functionExpose
                ]
        )


infixExpose : Parser State (Node TopLevelExpose)
infixExpose =
    Combine.lazy
        (\() ->
            Node.parser (Combine.map InfixExpose (parens (while ((/=) ')'))))
        )


typeExpose : Parser State (Node TopLevelExpose)
typeExpose =
    Combine.lazy
        (\() ->
            Node.parser exposedType
        )


exposedType : Parser State TopLevelExpose
exposedType =
    succeed identity
        |> Combine.andMap typeName
        |> Combine.ignore (maybe Layout.layout)
        |> Combine.andThen
            (\tipe ->
                Combine.choice
                    [ Node.parser (parens (Layout.maybeAroundBothSides (string "..")))
                        |> Combine.map (Node.range >> Just >> (\v -> ExposedType tipe v) >> TypeExpose)
                    , Combine.succeed (TypeOrAliasExpose tipe)
                    ]
            )


functionExpose : Parser State (Node TopLevelExpose)
functionExpose =
    Node.parser (Combine.map FunctionExpose functionName)
    */
