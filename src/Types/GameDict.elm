module Types.GameDict exposing (GameDict, addPlayerToGame, empty, getGameFor, getIdsFor, remove)

import Types.GameName as GameName exposing (GameName)
import Types.Id as Id exposing (Id)


type GameDict
    = GameDict (Id.Dict GameName) (GameName.Dict Id.Set)


empty : GameDict
empty =
    GameDict Id.dict.empty GameName.dict.empty


getGameFor : Id -> GameDict -> Maybe GameName
getGameFor id (GameDict forward _) =
    Id.dict.get id forward


addPlayerToGame : Id -> GameName -> GameDict -> GameDict
addPlayerToGame id gameName (GameDict forward backward) =
    GameDict
        (Id.dict.insert id gameName forward)
        (GameName.dict.map
            (\gn ->
                if gn == gameName then
                    Id.set.insert id

                else
                    Id.set.remove id
            )
            backward
        )


getIdsFor : GameName -> GameDict -> Id.Set
getIdsFor gameName (GameDict _ backward) =
    GameName.dict.get gameName backward
        |> Maybe.withDefault Id.set.empty


remove : Id -> GameDict -> GameDict
remove id ((GameDict forward backward) as original) =
    case Id.dict.get id forward of
        Nothing ->
            original

        Just gameName ->
            GameDict (Id.dict.remove id forward) (GameName.dict.remove gameName backward)
