module Types exposing
    ( AdminMsg(..)
    , Answer
    , BackendGameModel(..)
    , BackendModel
    , BackendMsg(..)
    , BackendPlayingModel
    , BackendPreparingModel
    , Code
    , Color
    , Context
    , FrontendAdminModel
    , FrontendHomepageModel
    , FrontendModel
    , FrontendMsg(..)
    , FrontendPlayingModel
    , FrontendPreparingModel
    , InnerFrontendModel(..)
    , PlayerModel(..)
    , PlayerMoves
    , PreparingUser
    , SharedPlayingModel
    , SharedPlayingPlayerModel
    , SharedPreparingModel
    , ToBackend(..)
    , ToFrontend(..)
    , sharedPreparingParse
    )

import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Dict exposing (Dict)
import Lamdera exposing (ClientId, SessionId)
import Set exposing (Set)
import Time
import Types.GameDict as GameDict exposing (GameDict)
import Types.GameName as GameName exposing (GameName)
import Types.Id as Id exposing (Id)
import Url exposing (Url)


type alias FrontendModel =
    { key : Key
    , inner : InnerFrontendModel
    , error : String
    , colorblindMode : Bool
    , rootUrl : String
    }


type InnerFrontendModel
    = FrontendConnecting GameName
    | FrontendHomepage FrontendHomepageModel
    | FrontendPreparing FrontendPreparingModel
    | FrontendPlaying FrontendPlayingModel
    | FrontendAdminAuthenticating String
    | FrontendAdminAuthenticated FrontendAdminModel


type alias FrontendHomepageModel =
    { username : String
    , gameName : String
    }


type alias SharedPreparingModel =
    { colors : Int
    , codeLength : String
    , canonicalName : String
    }


sharedPreparingParse : { a | codeLength : String, colors : Int } -> { codeLength : Int, colors : Int }
sharedPreparingParse shared =
    { codeLength = Maybe.withDefault 4 <| String.toInt shared.codeLength
    , colors = shared.colors
    }


type alias FrontendPreparingModel =
    { shared : SharedPreparingModel
    , gameName : GameName
    , me : ( Id, PreparingUser )
    , players : Dict SessionId { username : String, ready : Bool }
    }


type alias PreparingUser =
    { code : Code
    , ready : Bool
    , username : String
    }


type alias BackendPreparingModel =
    { shared : SharedPreparingModel
    , players : Dict Id PreparingUser
    , lastAction : Time.Posix
    }


type alias SharedPlayingModel =
    { colors : Int
    , codeLength : Int
    , startTime : Time.Posix
    , players : Dict ClientId SharedPlayingPlayerModel
    , canonicalName : String
    }


type alias SharedPlayingPlayerModel =
    { username : String
    , history : PlayerMoves
    , model : PlayerModel
    , opponentId : String
    }


type alias FrontendPlayingModel =
    { shared : SharedPlayingModel
    , gameName : GameName
    , code : Maybe Code
    , me : ClientId
    }


type alias BackendPlayingModel =
    { shared : SharedPlayingModel
    , codes : Dict ClientId Code
    , lastAction : Time.Posix
    }


type alias FrontendAdminModel =
    { preparing : List ( Time.Posix, FrontendPreparingModel )
    , playing : List ( Time.Posix, FrontendPlayingModel )
    }


type alias Context =
    { colorblindMode : Bool }


type alias BackendModel =
    { inGame : GameDict
    , games : Dict String BackendGameModel
    , connected : Dict SessionId (Set ClientId)
    , adminSessions : Set ClientId
    }


type BackendGameModel
    = BackendPreparing BackendPreparingModel
    | BackendPlaying BackendPlayingModel


type alias Code =
    List Color


{-| -1 is an empty slot, numbers from 0 to n are the colors
-}
type alias Color =
    Int


type PlayerModel
    = Guessing { current : Code }
    | Won { winTime : Time.Posix }


{-| The first element in the list is the first guess
-}
type alias PlayerMoves =
    List ( Code, Answer )


{-| Black means correct color, correct place. White means correct color, wrong place.
-}
type alias Answer =
    { black : Int
    , white : Int
    }


type FrontendMsg
    = UrlClicked UrlRequest
    | UrlChanged Url
    | HomepageMsg FrontendHomepageModel
    | UpsertGame
    | SetGameSettings SharedPreparingModel
    | SetCode Code
    | Submit
    | ColorblindMode Bool
    | NewGame
    | Home
    | FrontendNoop
    | AdminPassword String
    | AdminLogin
    | AdminMsg AdminMsg


type AdminMsg
    = AdminDelete GameName


type ToBackend
    = TBUpsertGame FrontendHomepageModel
    | TBCode Code
    | TBSubmit
    | TBNewGame
    | TBHome
    | TBGameSettings SharedPreparingModel
    | TBAdminAuthenticate String
    | TBAdmin AdminMsg


type BackendMsg
    = Timed Time.Posix Id ToBackend
    | ClientConnected SessionId ClientId
    | ClientDisconnected SessionId ClientId


type ToFrontend
    = TFReplaceModel InnerFrontendModel
    | TFError String
