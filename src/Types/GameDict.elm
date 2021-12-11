module Types.GameDict exposing (GameDict, empty, getGameFor)

import Dict exposing (Dict)
import Set exposing (Set)
import Types.GameName exposing (GameName)
import Types.Id as Id exposing (Id)


type GameDict
    = GameDict (Dict String GameName) (Dict String (Set String))


empty : GameDict
empty =
    GameDict Dict.empty Dict.empty


getGameFor : Id -> GameDict -> Maybe GameName
getGameFor id (GameDict forward _) =
    Dict.get (Id.toSessionId id) forward
