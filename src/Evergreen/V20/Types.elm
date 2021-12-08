module Evergreen.V20.Types exposing (..)

import Browser
import Browser.Navigation
import Dict
import Lamdera
import Set
import Time
import Url


type GameName
    = GameName String


type alias HomepageModel =
    { username : String
    , gameName : GameName
    }


type alias PreparingSharedModel =
    { colors : Int
    , codeLength : String
    }


type alias Id =
    String


type alias Color =
    Int


type alias Code =
    List Color


type alias PreparingUser =
    { code : Code
    , ready : Bool
    , username : String
    }


type alias PreparingFrontendModel =
    { shared : PreparingSharedModel
    , gameName : GameName
    , me : ( Id, PreparingUser )
    , players :
        Dict.Dict
            Id
            { username : String
            , ready : Bool
            }
    }


type alias Answer =
    { black : Int
    , white : Int
    }


type alias PlayerMoves =
    List ( Code, Answer )


type PlayerModel
    = Guessing
        { current : Code
        }
    | Won
        { winTime : Time.Posix
        }


type alias PlayingPlayerModel =
    { username : String
    , history : PlayerMoves
    , model : PlayerModel
    }


type alias PlayingSharedModel =
    { colors : Int
    , codeLength : Int
    , startTime : Time.Posix
    , players : Dict.Dict Lamdera.ClientId PlayingPlayerModel
    }


type alias PlayingFrontendModel =
    { shared : PlayingSharedModel
    , gameName : GameName
    , code : Maybe Code
    , me : Lamdera.ClientId
    }


type alias AdminModel =
    {}


type InnerFrontendModel
    = FrontendConnecting GameName
    | FrontendHomepage HomepageModel
    | FrontendPreparing PreparingFrontendModel
    | FrontendPlaying PlayingFrontendModel
    | FrontendAdmin AdminModel


type alias FrontendModel =
    { key : Browser.Navigation.Key
    , inner : InnerFrontendModel
    , error : String
    , colorblindMode : Bool
    , rootUrl : String
    }


type alias PreparingBackendModel =
    { shared : PreparingSharedModel
    , players : Dict.Dict Id PreparingUser
    , lastAction : Time.Posix
    }


type alias PlayingBackendModel =
    { shared : PlayingSharedModel
    , codes : Dict.Dict Lamdera.ClientId Code
    , lastAction : Time.Posix
    }


type GameModel
    = BackendPreparing PreparingBackendModel
    | BackendPlaying PlayingBackendModel


type alias BackendModel =
    { inGame : Dict.Dict Id GameName
    , games : Dict.Dict String GameModel
    , connected : Dict.Dict Lamdera.SessionId (Set.Set Lamdera.ClientId)
    , adminSessions : Set.Set Lamdera.ClientId
    }


type FrontendMsg
    = UrlClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | HomepageMsg HomepageModel
    | UpsertGame
    | SetGameSettings PreparingSharedModel
    | SetCode Code
    | Submit
    | ColorblindMode Bool
    | NewGame
    | Home
    | FrontendNoop


type ToBackend
    = TBUpsertGame HomepageModel
    | TBCode Code
    | TBSubmit
    | TBNewGame
    | TBHome
    | TBGameSettings PreparingSharedModel


type BackendMsg
    = Timed Time.Posix Id ToBackend
    | ClientConnected Lamdera.SessionId Lamdera.ClientId
    | ClientDisconnected Lamdera.SessionId Lamdera.ClientId


type ToFrontend
    = TFReplaceModel InnerFrontendModel
    | TFError String
