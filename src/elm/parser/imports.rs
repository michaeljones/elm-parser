use combine::error::ParseError;
use combine::{optional, Parser, RangeStream};

use super::base::module_name;
use super::expose::expose_definition;
use super::tokens::{as_token, import_token, type_name};
use super::whitespace::many1_spaces;
use elm::syntax::import::Import;

pub fn import_definition<'a, I>() -> impl Parser<Input = I, Output = Import> + 'a
where
    I: 'a,
    I: RangeStream<Item = char, Range = &'a str>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    let as_definition = as_token().skip(many1_spaces()).with(type_name());

    struct_parser!(Import {
        _: import_token().skip(many1_spaces()),
        module_name: module_name().skip(many1_spaces()),
        module_alias: optional(as_definition.skip(many1_spaces())),
        exposing_list: optional(expose_definition())
    })
}

#[cfg(test)]
mod tests {

    use super::*;
    use combine::Parser;
    use elm::syntax::exposing::*;

    #[test]
    fn import() {
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

    #[test]
    fn import_with_alias() {
        assert_eq!(
            import_definition().parse("import Test as Alias exposing (..)"),
            Ok((
                Import {
                    module_name: vec!["Test".to_string()],
                    module_alias: Some("Alias".to_string()),
                    exposing_list: Some(Exposing::All),
                },
                ""
            ))
        )
    }

    #[test]
    fn import_with_exposing() {
        assert_eq!(
            import_definition().parse("import Test as Alias exposing (Abc,def)"),
            Ok((
                Import {
                    module_name: vec!["Test".to_string()],
                    module_alias: Some("Alias".to_string()),
                    exposing_list: Some(Exposing::Explicit(vec![
                        TopLevelExpose::TypeOrAliasExpose("Abc".to_string()),
                        TopLevelExpose::FunctionExpose("def".to_string())
                    ]))
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
