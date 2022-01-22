module Types.Id exposing (Dict, Id, Set, dict, fromSessionId, set, toSessionId)

import Any.Dict as Dict
import Any.Set as Set
import Lamdera exposing (SessionId)


type Id
    = Id SessionId


type alias Set =
    Set.Set Id String


type alias Dict v =
    Dict.Dict Id v String


fromSessionId : SessionId -> Id
fromSessionId =
    Id


toSessionId : Id -> SessionId
toSessionId (Id id) =
    id


set : Set.Interface Id b output SessionId comparable2
set =
    Set.makeInterface
        { toComparable = toSessionId
        , fromComparable = fromSessionId
        }


dict : Dict.Interface Id v v2 output SessionId
dict =
    Dict.makeInterface
        { toComparable = toSessionId
        , fromComparable = fromSessionId
        }
