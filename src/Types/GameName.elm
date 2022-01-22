module Types.GameName exposing (Dict, GameName, Set, dict, fromString, set, toString)

import Any.Dict as Dict
import Any.Set as Set


type GameName
    = GameName String


type alias Set =
    Set.Set GameName String


type alias Dict v =
    Dict.Dict GameName v String


toString : GameName -> String
toString (GameName str) =
    str


fromString : String -> GameName
fromString str =
    let
        cutSpaces s =
            let
                repl =
                    String.replace "  " " " s
            in
            if s == repl then
                s

            else
                cutSpaces repl
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
