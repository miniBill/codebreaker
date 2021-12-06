module Backend exposing (..)

import Dict
import Lamdera exposing (ClientId, SessionId)
import List.Extra
import Task
import Theme
import Time
import Types exposing (..)


app :
    { init : ( BackendModel, Cmd BackendMsg )
    , update : BackendMsg -> BackendModel -> ( BackendModel, Cmd BackendMsg )
    , updateFromFrontend : SessionId -> ClientId -> ToBackend -> BackendModel -> ( BackendModel, Cmd BackendMsg )
    , subscriptions : BackendModel -> Sub BackendMsg
    }
app =
    Lamdera.backend
        { init = init
        , update = update
        , updateFromFrontend = updateFromFrontend
        , subscriptions = subscriptions
        }


init : ( BackendModel, Cmd BackendMsg )
init =
    ( { inGame = Dict.empty
      , games = Dict.empty
      }
    , Cmd.none
    )


update : BackendMsg -> BackendModel -> ( BackendModel, Cmd BackendMsg )
update msg model =
    case msg of
        ClientConnected sessionId clientId ->
            let
                id =
                    toId sessionId clientId
            in
            ( model
            , (case Dict.get id model.inGame of
                Nothing ->
                    FrontendHomepage defaultHomepageModel

                Just gameName ->
                    case Dict.get gameName model.games of
                        Nothing ->
                            FrontendHomepage defaultHomepageModel

                        Just game ->
                            toInnerFrontendModel id gameName game
              )
                |> TFReplaceModel
                |> Lamdera.sendToFrontend id
            )

        ClientDisconnected _ _ ->
            ( model, Cmd.none )

        StartGame id now ->
            updateGame id
                model
                (\newGame ->
                    ( BackendPlaying
                        { codes = Dict.map (\_ { code } -> code) newGame.players
                        , shared =
                            { colors = newGame.shared.colors
                            , codeLength = newGame.shared.codeLength
                            , startTime = now
                            , players =
                                Dict.map
                                    (\_ { username } ->
                                        { username = username
                                        , history = []
                                        , model = Guessing { current = [] }
                                        }
                                    )
                                    newGame.players
                            }
                        }
                    , Cmd.none
                    )
                )
                (\playing -> ( BackendPlaying playing, Cmd.none ))

        GotWinTime id now ->
            updateGame id
                model
                (\preparing ->
                    ( BackendPreparing preparing
                    , Cmd.none
                    )
                )
                (\({ shared } as playing) ->
                    ( BackendPlaying
                        { playing
                            | shared =
                                { shared
                                    | players =
                                        shared.players
                                            |> Dict.map
                                                (\_ player ->
                                                    case ( player.model, player.history ) of
                                                        ( Guessing _, ( _, { black } ) :: _ ) ->
                                                            if black == shared.codeLength then
                                                                { player | model = Won { winTime = now } }

                                                            else
                                                                player

                                                        _ ->
                                                            player
                                                )
                                }
                        }
                    , Cmd.none
                    )
                )


toId : SessionId -> ClientId -> SessionId
toId sessionId _ =
    sessionId


defaultHomepageModel : HomepageModel
defaultHomepageModel =
    { codeLength = ""
    , colors = ""
    , gameName = ""
    , username = ""
    }


type alias Id =
    String


toInnerFrontendModel : Id -> GameName -> GameModel -> InnerFrontendModel
toInnerFrontendModel id gameName game =
    case game of
        BackendPreparing preparing ->
            let
                player =
                    Dict.get id preparing.players
                        |> Maybe.withDefault
                            { username = ""
                            , code = []
                            , ready = False
                            }
            in
            FrontendPreparing
                { shared = preparing.shared
                , gameName = gameName
                , me = player
                , players =
                    Dict.map
                        (\_ { username, ready } ->
                            { username = username
                            , ready = ready
                            }
                        )
                        preparing.players
                }

        BackendPlaying playing ->
            FrontendPlaying
                { shared = playing.shared
                , me = id
                , code = Dict.get id playing.codes
                , gameName = gameName
                }


updateFromFrontend : SessionId -> ClientId -> ToBackend -> BackendModel -> ( BackendModel, Cmd BackendMsg )
updateFromFrontend sessionId clientId msg model =
    let
        id =
            toId sessionId clientId
    in
    case msg of
        TBUpsertGame data ->
            let
                withDefault d v =
                    if String.isEmpty v then
                        d

                    else
                        v

                checkInRange from to input =
                    String.toInt input
                        |> Maybe.andThen
                            (\i ->
                                if from <= i && i <= to then
                                    Just i

                                else
                                    Nothing
                            )
            in
            case
                ( checkInRange 2 (List.length Theme.colors) <| withDefault "8" data.colors
                , checkInRange 2 20 <| withDefault "4" data.codeLength
                )
            of
                ( Just colors, Just codeLength ) ->
                    let
                        newGame =
                            case Dict.get data.gameName model.games of
                                Just game ->
                                    case game of
                                        BackendPreparing preparing ->
                                            BackendPreparing
                                                { preparing
                                                    | players =
                                                        Dict.insert id
                                                            { ready = False
                                                            , code = []
                                                            , username = data.username
                                                            }
                                                            preparing.players
                                                }

                                        BackendPlaying _ ->
                                            game

                                Nothing ->
                                    BackendPreparing
                                        { shared =
                                            { colors = colors
                                            , codeLength = codeLength
                                            }
                                        , players =
                                            Dict.singleton id
                                                { ready = False
                                                , code = []
                                                , username = data.username
                                                }
                                        }
                    in
                    ( { inGame = Dict.insert id data.gameName model.inGame
                      , games = Dict.insert data.gameName newGame model.games
                      }
                    , Lamdera.sendToFrontend id <| TFReplaceModel <| toInnerFrontendModel id data.gameName newGame
                    )

                ( Nothing, _ ) ->
                    ( model, Lamdera.sendToFrontend id <| TFError "Invalid number of colors" )

                ( _, Nothing ) ->
                    ( model, Lamdera.sendToFrontend id <| TFError "Invalid code length" )

        TBCode code ->
            updateGame id
                model
                (\preparing ->
                    let
                        setCode player =
                            if player.ready then
                                player

                            else
                                { player | code = code }
                    in
                    ( { preparing
                        | players =
                            Dict.update
                                id
                                (Maybe.map setCode)
                                preparing.players
                      }
                        |> BackendPreparing
                    , Cmd.none
                    )
                )
                (\playing ->
                    let
                        shared =
                            playing.shared

                        setCode player =
                            case player.model of
                                Guessing _ ->
                                    { player | model = Guessing { current = code } }

                                Won _ ->
                                    player
                    in
                    ( { playing
                        | shared =
                            { shared
                                | players =
                                    Dict.update
                                        id
                                        (Maybe.map setCode)
                                        shared.players
                            }
                      }
                        |> BackendPlaying
                    , Cmd.none
                    )
                )

        TBSubmit ->
            updateGame id
                model
                (\preparing ->
                    let
                        setReady player =
                            { player | ready = True }

                        newGame =
                            { preparing
                                | players =
                                    Dict.update
                                        id
                                        (Maybe.map setReady)
                                        preparing.players
                            }
                    in
                    ( BackendPreparing newGame
                    , if List.all .ready <| Dict.values newGame.players then
                        Time.now |> Task.perform (StartGame id)

                      else
                        Cmd.none
                    )
                )
                (\({ shared } as playing) ->
                    case Dict.get id shared.players of
                        Nothing ->
                            ( BackendPlaying playing, Cmd.none )

                        Just player ->
                            case player.model of
                                Won _ ->
                                    ( BackendPlaying playing, Cmd.none )

                                Guessing { current } ->
                                    let
                                        code =
                                            playing.codes
                                                |> Dict.toList
                                                |> List.Extra.filterNot (\( i, _ ) -> i == id)
                                                |> List.head
                                                |> Maybe.map Tuple.second
                                                |> Maybe.withDefault []

                                        answer =
                                            getAnswer code current

                                        newPlayer =
                                            { player
                                                | history = ( current, answer ) :: player.history
                                                , model = Guessing { current = [] }
                                            }

                                        newGame =
                                            { playing
                                                | shared =
                                                    { shared
                                                        | players = Dict.insert id newPlayer shared.players
                                                    }
                                            }
                                    in
                                    ( BackendPlaying newGame
                                    , Time.now |> Task.perform (GotWinTime id)
                                    )
                )


getAnswer : Code -> Code -> Answer
getAnswer code guess =
    List.map2 Tuple.pair code guess
        |> List.foldl
            (\( codeDigit, guessDigit ) acc ->
                if codeDigit == guessDigit then
                    { acc | black = acc.black + 1 }

                else
                    { acc
                        | extraCode = codeDigit :: acc.extraCode
                        , extraGuess = guessDigit :: acc.extraGuess
                    }
            )
            { black = 0
            , extraCode = []
            , extraGuess = []
            }
        |> (\{ black, extraCode, extraGuess } ->
                let
                    aggregateCount list =
                        list
                            |> List.Extra.gatherEquals
                            |> List.map (\( h, t ) -> ( h, 1 + List.length t ))

                    extraCodeDict =
                        extraCode
                            |> aggregateCount
                            |> Dict.fromList
                in
                { black = black
                , white =
                    extraGuess
                        |> aggregateCount
                        |> List.map
                            (\( h, count ) ->
                                min
                                    (Maybe.withDefault 0 <| Dict.get h extraCodeDict)
                                    count
                            )
                        |> List.sum
                }
           )


updateGame :
    Id
    -> BackendModel
    -> (PreparingBackendModel -> ( GameModel, Cmd msg ))
    -> (PlayingBackendModel -> ( GameModel, Cmd msg ))
    -> ( BackendModel, Cmd msg )
updateGame id model updatePreparing updatePlaying =
    case Dict.get id model.inGame of
        Nothing ->
            ( model, Cmd.none )

        Just gameName ->
            case Dict.get gameName model.games of
                Nothing ->
                    ( model, Cmd.none )

                Just game ->
                    let
                        ( ( newGame, additionalCmd ), playerIds ) =
                            case game of
                                BackendPreparing preparing ->
                                    ( updatePreparing preparing
                                    , Dict.keys preparing.players
                                    )

                                BackendPlaying playing ->
                                    ( updatePlaying playing
                                    , Dict.keys playing.shared.players
                                    )

                        send cid =
                            toInnerFrontendModel cid gameName newGame
                                |> TFReplaceModel
                                |> Lamdera.sendToFrontend cid
                    in
                    ( { model | games = Dict.insert gameName newGame model.games }
                    , playerIds
                        |> List.map send
                        |> (::) additionalCmd
                        |> Cmd.batch
                    )


subscriptions : BackendModel -> Sub BackendMsg
subscriptions _ =
    Sub.batch
        [ Lamdera.onConnect ClientConnected
        , Lamdera.onDisconnect ClientDisconnected
        ]
