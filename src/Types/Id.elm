module Types.Id exposing (Id, dict, fromSessionId, set, toSessionId)

import Any.Dict as Dict
import Any.Set as Set
import Lamdera exposing (SessionId)


type Id
    = Id SessionId


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
