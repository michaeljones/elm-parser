module Lists exposing (..)

isListLengthFour : (List a) -> Bool
isListLengthFour list =
  case List.length list of
    4 -> True
    _ -> False
