module Types.GameName exposing (GameName, dict, fromString, set, toString)

import Any.Dict as Dict
import Any.Set as Set


type GameName
    = GameName String


toString : GameName -> String
toString (GameName str) =
    str


fromString : String -> GameName
fromString str =
    let
        cutSpaces s =
            if String.contains "  " s then
                cutSpaces (String.replace "  " " " s)

            else
                s
    in
    String.toLower str
        |> cutSpaces
        |> String.replace "-" " "
        |> GameName


set : Set.Interface GameName b output String comparable
set =
    Set.makeInterface
        { toComparable = toString
        , fromComparable = fromString
        }


dict : Dict.Interface GameName v v2 output String
dict =
    Dict.makeInterface
        { toComparable = toString
        , fromComparable = fromString
        }
