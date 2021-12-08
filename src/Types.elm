module Types exposing (..)

import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Dict exposing (Dict)
import Lamdera exposing (ClientId)
import Time
import Url exposing (Url)


type alias FrontendModel =
    { key : Key
    , inner : InnerFrontendModel
    , error : String
    , colorblindMode : Bool
    }


type InnerFrontendModel
    = FrontendConnecting GameName
    | FrontendHomepage HomepageModel
    | FrontendPreparing PreparingFrontendModel
    | FrontendPlaying PlayingFrontendModel


type alias HomepageModel =
    { username : String
    , gameName : GameName
    }


type alias PreparingSharedModel =
    { colors : String
    , codeLength : String
    }


preparingSharedParse : { a | codeLength : String, colors : String } -> { codeLength : Int, colors : Int }
preparingSharedParse shared =
    { codeLength = Maybe.withDefault 4 <| String.toInt shared.codeLength
    , colors = Maybe.withDefault 8 <| String.toInt shared.colors
    }


type alias Id =
    String


type alias PreparingFrontendModel =
    { shared : PreparingSharedModel
    , gameName : GameName
    , me : ( Id, PreparingUser )
    , players : Dict Id { username : String, ready : Bool }
    }


type alias PreparingUser =
    { code : Code
    , ready : Bool
    , username : String
    }


type alias PreparingBackendModel =
    { shared : PreparingSharedModel
    , players : Dict Id PreparingUser
    }


type alias PlayingSharedModel =
    { colors : Int
    , codeLength : Int
    , startTime : Time.Posix
    , players : Dict ClientId { username : String, history : PlayerMoves, model : PlayerModel }
    }


type alias PlayingFrontendModel =
    { shared : PlayingSharedModel
    , gameName : GameName
    , code : Maybe Code
    , me : ClientId
    }


type alias PlayingBackendModel =
    { shared : PlayingSharedModel
    , codes : Dict ClientId Code
    }


type alias Context =
    { colorblindMode : Bool }


type alias BackendModel =
    { inGame : Dict ClientId GameName
    , games : Dict String GameModel
    }


type GameName
    = GameName String


normalizeGameName : GameName -> String
normalizeGameName (GameName str) =
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


rawGameName : GameName -> String
rawGameName (GameName str) =
    str


type GameModel
    = BackendPreparing PreparingBackendModel
    | BackendPlaying PlayingBackendModel


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
    | HomepageMsg HomepageModel
    | UpsertGame
    | SetGameSettings PreparingSharedModel
    | SetCode Code
    | Submit
    | ColorblindMode Bool
    | NewGame
    | Home


type ToBackend
    = TBUpsertGame HomepageModel
    | TBCode Code
    | TBSubmit
    | TBNewGame
    | TBHome
    | TBGameSettings PreparingSharedModel


type BackendMsg
    = StartGame ClientId Time.Posix
    | GotWinTime ClientId Time.Posix
    | ClientConnected Lamdera.SessionId ClientId
    | ClientDisconnected Lamdera.SessionId ClientId


type ToFrontend
    = TFReplaceModel InnerFrontendModel
    | TFError String
