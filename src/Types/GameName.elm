module Types.GameName exposing (GameName, fromString, toString)


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
