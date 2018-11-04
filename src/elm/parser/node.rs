/*
module Elm.Parser.Node exposing (parser)

import Combine exposing (ParseLocation, Parser, andMap, succeed, withLocation)
import Elm.Parser.State exposing (State)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Range exposing (Location, Range)


parser : Parser State a -> Parser State (Node a)
parser p =
    withLocation
        (\start ->
            succeed (\v r -> Node r v)
                |> Combine.andMap p
                |> Combine.andMap
                    (withLocation
                        (\end ->
                            succeed <|
                                { start = asPointerLocation start
                                , end = asPointerLocation end
                                }
                        )
                    )
        )


asPointerLocation : ParseLocation -> Location
asPointerLocation { line, column } =
    { row = line, column = column }
*/
