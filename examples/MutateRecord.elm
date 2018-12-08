module MutateRecord exposing (..)

type alias Model =
  { name: String
  }

renameToSteve : Model -> Model
renameToSteve model =
  { model | name = "Steve" }
