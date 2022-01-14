module Types.GameDict exposing (GameDict, addPlayerToGame, empty, getGameFor)

import Any.Dict
import Any.Set
import Types.GameName as GameName exposing (GameName)
import Types.Id as Id exposing (Id)


type GameDict
    = GameDict (Any.Dict.Dict Id GameName String) (Any.Dict.Dict GameName (Any.Set.Set Id String) String)


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
