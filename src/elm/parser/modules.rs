use combine::error::ParseError;
use combine::parser::char::{space, spaces, string};
use combine::{Parser, Stream};

use elm::syntax::module::Module;

pub fn module_definition<I>() -> impl Parser<Input = I, Output = Module>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    string("module")
        .skip(space())
        .skip(spaces())
        .with(string("Test"))
        .skip(space())
        .skip(spaces())
        .skip(string("exposing (..)"))
        .map(|name| Module {
            name: vec![name.to_string()],
        })
}

#[cfg(test)]
mod tests {

    use combine::Parser;
    use elm::parser::modules::module_definition;

    #[test]
    fn simple() {
        assert!(
            module_definition()
                .parse("module Test exposing (..)")
                .is_ok()
        );
    }
}

/*
module Elm.Parser.Modules exposing (moduleDefinition)

import Combine exposing (Parser, between, choice, sepBy1, string, succeed)
import Elm.Parser.Base exposing (moduleName)
import Elm.Parser.Expose exposing (exposable, exposeDefinition)
import Elm.Parser.Layout as Layout
import Elm.Parser.Node as Node
import Elm.Parser.State exposing (State)
import Elm.Parser.Tokens exposing (functionName, moduleToken, portToken, typeName)
import Elm.Syntax.Module exposing (DefaultModuleData, Module(..))
import Elm.Syntax.Node as Node exposing (Node)


moduleDefinition : Parser State Module
moduleDefinition =
    choice
        [ normalModuleDefinition
        , portModuleDefinition
        , effectModuleDefinition
        ]


effectWhereClause : Parser State ( String, Node String )
effectWhereClause =
    succeed Tuple.pair
        |> Combine.andMap functionName
        |> Combine.andMap (Layout.maybeAroundBothSides (string "=") |> Combine.continueWith (Node.parser typeName))


whereBlock : Parser State { command : Maybe (Node String), subscription : Maybe (Node String) }
whereBlock =
    between
        (string "{")
        (string "}")
        (sepBy1 (string ",")
            (Layout.maybeAroundBothSides effectWhereClause)
        )
        |> Combine.map
            (\pairs ->
                { command = pairs |> List.filter (Tuple.first >> (==) "command") |> List.head |> Maybe.map Tuple.second
                , subscription = pairs |> List.filter (Tuple.first >> (==) "subscription") |> List.head |> Maybe.map Tuple.second
                }
            )


effectWhereClauses : Parser State { command : Maybe (Node String), subscription : Maybe (Node String) }
effectWhereClauses =
    string "where"
        |> Combine.continueWith Layout.layout
        |> Combine.continueWith whereBlock


effectModuleDefinition : Parser State Module
effectModuleDefinition =
    let
        createEffectModule name whereClauses exp =
            EffectModule
                { moduleName = name
                , exposingList = exp
                , command = whereClauses.command
                , subscription = whereClauses.subscription
                }
    in
    succeed createEffectModule
        |> Combine.ignore (string "effect")
        |> Combine.ignore Layout.layout
        |> Combine.ignore moduleToken
        |> Combine.ignore Layout.layout
        |> Combine.andMap (Node.parser moduleName)
        |> Combine.ignore Layout.layout
        |> Combine.andMap effectWhereClauses
        |> Combine.ignore Layout.layout
        |> Combine.andMap (Node.parser exposeDefinition)


normalModuleDefinition : Parser State Module
normalModuleDefinition =
    Combine.map NormalModule
        (succeed DefaultModuleData
            |> Combine.ignore moduleToken
            |> Combine.ignore Layout.layout
            |> Combine.andMap (Node.parser moduleName)
            |> Combine.ignore Layout.layout
            |> Combine.andMap (Node.parser exposeDefinition)
        )


portModuleDefinition : Parser State Module
portModuleDefinition =
    Combine.map PortModule
        (succeed DefaultModuleData
            |> Combine.ignore portToken
            |> Combine.ignore Layout.layout
            |> Combine.ignore moduleToken
            |> Combine.ignore Layout.layout
            |> Combine.andMap (Node.parser moduleName)
            |> Combine.ignore Layout.layout
            |> Combine.andMap (Node.parser exposeDefinition)
        )
*/
