use combine::error::ParseError;
use combine::parser::char::{space, spaces, string};
use combine::{Parser, Stream};

use super::base::module_name;
use super::expose::expose_definition;
use super::tokens::import_token;
use elm::syntax::exposing::Exposing;
use elm::syntax::import::Import;

pub fn import_definition<I>() -> impl Parser<Input = I, Output = Import>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    import_token()
        .skip(space())
        .skip(spaces())
        .with(module_name())
        .skip(space())
        .skip(spaces())
        .skip(string("exposing (..)"))
        .map(|name| Import {
            module_name: name,
            module_alias: None,
            exposing_list: Some(Exposing::All),
        })
}

#[cfg(test)]
mod tests {

    use super::*;
    use combine::Parser;

    #[test]
    fn simple() {
        assert_eq!(
            import_definition().parse("import Test exposing (..)"),
            Ok((
                Import {
                    module_name: vec!["Test".to_string()],
                    module_alias: None,
                    exposing_list: Some(Exposing::All),
                },
                ""
            ))
        )
    }
}

/*
module Elm.Parser.Imports exposing (importDefinition)

import Combine exposing (Parser, maybe, succeed)
import Elm.Parser.Base exposing (moduleName)
import Elm.Parser.Expose exposing (exposable, exposeDefinition)
import Elm.Parser.Layout as Layout
import Elm.Parser.Node as Node
import Elm.Parser.State exposing (State)
import Elm.Parser.Tokens exposing (asToken, importToken)
import Elm.Syntax.Import exposing (Import)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Range as Range exposing (Range)


importDefinition : Parser State (Node Import)
importDefinition =
    let
        importAndModuleName =
            importToken
                |> Combine.continueWith Layout.layout
                |> Combine.continueWith (Node.parser moduleName)

        asDefinition : Parser State (Node ModuleName)
        asDefinition =
            asToken
                |> Combine.continueWith Layout.layout
                |> Combine.continueWith (Node.parser moduleName)

        parseExposingDefinition : Node ModuleName -> Maybe (Node ModuleName) -> Parser State Import
        parseExposingDefinition mod asDef =
            Combine.choice
                [ Node.parser exposeDefinition
                    |> Combine.map (Just >> Import mod asDef)
                , Combine.succeed (Import mod asDef Nothing)
                ]

        parseAsDefinition mod =
            Combine.choice
                [ asDefinition
                    |> Combine.ignore Layout.optimisticLayout
                    |> Combine.andThen (Just >> parseExposingDefinition mod)
                , parseExposingDefinition mod Nothing
                ]
    in
    Node.parser (Combine.succeed ())
        |> Combine.andThen
            (\(Node start ()) ->
                importAndModuleName
                    |> Combine.ignore Layout.optimisticLayout
                    |> Combine.andThen parseAsDefinition
                    |> Combine.map (setupNode start)
            )


setupNode : Range -> Import -> Node Import
setupNode start imp =
    let
        allRanges =
            [ Just start
            , Just (Node.range imp.moduleName)
            , Maybe.map Node.range imp.exposingList
            , Maybe.map Node.range imp.moduleAlias
            ]
    in
    Node
        (Range.combine (List.filterMap identity allRanges))
        imp
        */