module Evergreen.V1.Types exposing (..)

import Browser
import Browser.Navigation
import Dict
import Lamdera
import Time
import Url


type alias GameName =
    String


type alias HomepageModel =
    { username : String
    , gameName : GameName
    , colors : String
    , codeLength : String
    }


type alias PreparingSharedModel =
    { colors : Int
    , codeLength : Int
    }


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
    , gameName : String
    , me : PreparingUser
    , players :
        Dict.Dict
            Lamdera.ClientId
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


type alias PlayingSharedModel =
    { colors : Int
    , codeLength : Int
    , startTime : Time.Posix
    , players :
        Dict.Dict
            Lamdera.ClientId
            { username : String
            , history : PlayerMoves
            , model : PlayerModel
            }
    }


type alias PlayingFrontendModel =
    { shared : PlayingSharedModel
    , gameName : String
    , code : Maybe Code
    , me : Lamdera.ClientId
    }


type InnerFrontendModel
    = FrontendConnecting
    | FrontendHomepage HomepageModel
    | FrontendPreparing PreparingFrontendModel
    | FrontendPlaying PlayingFrontendModel


type alias FrontendModel =
    { key : Browser.Navigation.Key
    , inner : InnerFrontendModel
    , error : String
    , colorblindMode : Bool
    }


type alias PreparingBackendModel =
    { shared : PreparingSharedModel
    , players : Dict.Dict Lamdera.ClientId PreparingUser
    }


type alias PlayingBackendModel =
    { shared : PlayingSharedModel
    , codes : Dict.Dict Lamdera.ClientId Code
    }


type GameModel
    = BackendPreparing PreparingBackendModel
    | BackendPlaying PlayingBackendModel


type alias BackendModel =
    { inGame : Dict.Dict Lamdera.ClientId GameName
    , games : Dict.Dict GameName GameModel
    }


type FrontendMsg
    = UrlClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | HomepageMsg HomepageModel
    | UpsertGame
    | SetCode Code
    | Submit
    | ColorblindMode Bool


type ToBackend
    = TBUpsertGame HomepageModel
    | TBCode Code
    | TBSubmit


type BackendMsg
    = StartGame Lamdera.ClientId Time.Posix
    | GotWinTime Lamdera.ClientId Time.Posix
    | ClientConnected Lamdera.SessionId Lamdera.ClientId
    | ClientDisconnected Lamdera.SessionId Lamdera.ClientId


type ToFrontend
    = TFReplaceModel InnerFrontendModel
    | TFError String
