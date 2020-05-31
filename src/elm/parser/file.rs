use combine::error::ParseError;
use combine::{many, Parser};

use elm::parser::declarations::declaration;
use elm::parser::imports::import_definition;
use elm::parser::layout;
use elm::parser::modules::module_definition;
use elm::syntax::file::File;

use super::state::StateStream;

pub fn file<Input>() -> impl Parser<StateStream<Input>, Output = File>
where
    Input: combine::Stream<Token = char> + combine::RangeStreamOnce,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
    <Input as combine::StreamOnce>::Range: combine::stream::Range,
{
    struct_parser!(File {
        _: combine::optional(layout::layout_strict()),
        module_definition: module_definition(),
        _: combine::optional(layout::layout_strict()),
        imports: many(import_definition()),
        _: combine::optional(layout::layout_strict()),
        declarations: many(declaration()),
    })
}

#[cfg(test)]
mod tests {

    use combine::Parser;
    use elm::parser::file::file;

    #[test]
    fn file_1() {
        assert!(file().parse("module Test exposing (..)").is_ok());
    }

    #[test]
    fn file_2() {
        assert!(file()
            .parse(
                "module Test exposing (..)
import Abc"
            )
            .is_ok());
    }
}

/*
module Elm.Parser.File exposing (file)

import Combine exposing (Parser, many, maybe, sepBy, succeed, withState)
import Elm.Parser.Declarations exposing (declaration)
import Elm.Parser.Imports exposing (importDefinition)
import Elm.Parser.Layout as Layout
import Elm.Parser.Modules exposing (moduleDefinition)
import Elm.Parser.Node as Node
import Elm.Parser.Ranges
import Elm.Parser.State as State exposing (State)
import Elm.Syntax.Declaration exposing (Declaration)
import Elm.Syntax.File exposing (File)
import Elm.Syntax.Node exposing (Node)


file : Parser State File
file =
    succeed File
        |> Combine.ignore (maybe Layout.layoutStrict)
        |> Combine.andMap (Node.parser moduleDefinition)
        |> Combine.ignore (maybe Layout.layoutStrict)
        |> Combine.andMap (many (importDefinition |> Combine.ignore Layout.optimisticLayout))
        |> Combine.ignore (maybe Layout.layoutStrict)
        |> Combine.andMap fileDeclarations
        |> Combine.andMap collectComments
        |> Combine.ignore Layout.optimisticLayout


collectComments : Parser State (List (Node String))
collectComments =
    withState (State.getComments >> succeed)


fileDeclarations : Parser State (List (Node Declaration))
fileDeclarations =
    many
        (declaration
            |> Combine.ignore (maybe Layout.layoutStrict)
        )
        */
