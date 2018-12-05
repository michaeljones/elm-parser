module BrowserElement exposing (..)

import Browser exposing (..)
import Html exposing (..)
import Html.Events exposing (onClick)

type alias Model =
  { toggleCount: Int
  , isToggled: Bool
  }

type Msg
  = Toggle
  | Reset

type alias Flags = {}

view : Model -> Html Msg
view model =
  div []
    [ button [ onClick Toggle ] [ text "Toggle" ]
    , button [ onClick Reset ] [ text "Reset" ]
    , text (if model.isToggled then "Yup" else "Nope")
    ]

update : Msg -> Model -> (Model, Cmd msg)
update msg model =
  case msg of
    Toggle ->
      ({ model | isToggled = not model.isToggled, toggleCount = model.toggleCount + 1}, Cmd.none)
    Reset ->
      ({ model | isToggled = False, toggleCount = 0 }, Cmd.none)

init : Flags -> (Model, Cmd msg)
init flags =
  ({ toggleCount = 0, isToggled = False }, Cmd.none)

main = Browser.element
  { init = init
  , update = update
  , subscriptions = (\_ -> Sub.none)
  , view = view
  }
