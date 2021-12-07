module Backend exposing (..)

import Dict
import Lamdera exposing (ClientId, SessionId)
import List.Extra
import Task
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
                    case Dict.get (normalizeGameName gameName) model.games of
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
                            { colors = Maybe.withDefault 4 <| String.toInt newGame.shared.colors
                            , codeLength = Maybe.withDefault 8 <| String.toInt newGame.shared.codeLength
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
                    let
                        newShared =
                            { shared | players = Dict.map updatePlayer shared.players }

                        updatePlayer _ player =
                            case ( player.model, player.history ) of
                                ( Guessing _, ( _, { black } ) :: _ ) ->
                                    if black == shared.codeLength then
                                        { player | model = Won { winTime = now } }

                                    else
                                        player

                                _ ->
                                    player
                    in
                    ( BackendPlaying { playing | shared = newShared }
                    , Cmd.none
                    )
                )


toId : SessionId -> ClientId -> SessionId
toId sessionId _ =
    sessionId


defaultHomepageModel : HomepageModel
defaultHomepageModel =
    { gameName = GameName ""
    , username = ""
    }


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
                , me = ( id, player )
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
            if String.isEmpty data.username then
                ( model, Lamdera.sendToFrontend id <| TFError "Invalid empty user name" )

            else if String.isEmpty (normalizeGameName data.gameName) then
                ( model, Lamdera.sendToFrontend id <| TFError "Invalid empty game name" )

            else
                let
                    newGame =
                        case Dict.get (normalizeGameName data.gameName) model.games of
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
                                        { colors = ""
                                        , codeLength = ""
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
                  , games = Dict.insert (normalizeGameName data.gameName) newGame model.games
                  }
                , Lamdera.sendToFrontend id <| TFReplaceModel <| toInnerFrontendModel id data.gameName newGame
                )

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
                    , if Dict.size newGame.players >= 2 && List.all .ready (Dict.values newGame.players) then
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
                                        codesList =
                                            Dict.toList playing.codes

                                        code =
                                            let
                                                go x =
                                                    case x of
                                                        [] ->
                                                            []

                                                        [ ( _, q ) ] ->
                                                            q

                                                        ( h1, _ ) :: ((( _, h2 ) :: _) as t) ->
                                                            if h1 == id then
                                                                h2

                                                            else
                                                                go t
                                            in
                                            go (codesList ++ List.take 1 codesList)

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

        TBNewGame ->
            updateGame id
                model
                (\preparing -> ( BackendPreparing preparing, Cmd.none ))
                (\({ shared } as playing) ->
                    if
                        List.all
                            (\player ->
                                case player.model of
                                    Won _ ->
                                        True

                                    Guessing _ ->
                                        False
                            )
                            (Dict.values shared.players)
                    then
                        ( BackendPreparing
                            { shared =
                                { codeLength = String.fromInt playing.shared.codeLength
                                , colors = String.fromInt playing.shared.colors
                                }
                            , players =
                                Dict.map
                                    (\_ { username } ->
                                        { code = []
                                        , ready = False
                                        , username = username
                                        }
                                    )
                                    playing.shared.players
                            }
                        , Cmd.none
                        )

                    else
                        ( BackendPlaying playing, Cmd.none )
                )

        TBHome ->
            ( { model | inGame = Dict.remove id model.inGame }
            , Lamdera.sendToFrontend id <| TFReplaceModel (FrontendHomepage defaultHomepageModel)
            )

        TBGameSettings shared ->
            updateGame id
                model
                (\preparing ->
                    ( BackendPreparing
                        { preparing
                            | shared = shared
                            , players = Dict.map (\_ player -> { player | ready = False, code = [] }) preparing.players
                        }
                    , Cmd.none
                    )
                )
                (\playing -> ( BackendPlaying playing, Cmd.none ))


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
            case Dict.get (normalizeGameName gameName) model.games of
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
                    ( { model | games = Dict.insert (normalizeGameName gameName) newGame model.games }
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
