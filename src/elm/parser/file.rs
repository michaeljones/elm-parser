use combine::error::ParseError;
use combine::{Parser, RangeStream};

use elm::parser::modules::module_definition;
use elm::syntax::file::File;

pub fn file<'a, I>() -> impl Parser<Input = I, Output = File> + 'a
where
    I: 'a,
    I: RangeStream<Item = char, Range = &'a str>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    struct_parser!(File {
        module_definition: module_definition()
    })
}

#[cfg(test)]
mod tests {

    use combine::Parser;
    use elm::parser::file::file;

    #[test]
    fn simple_file() {
        let result = file().parse("module Test exposing (..)");
        assert!(result.is_ok());
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
