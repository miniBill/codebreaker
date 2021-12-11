module Types.Id exposing (Id, fromSessionId, toSessionId)

import Lamdera exposing (SessionId)


type Id
    = Id SessionId


fromSessionId : SessionId -> Id
fromSessionId =
    Id


toSessionId : Id -> SessionId
toSessionId (Id id) =
    id
