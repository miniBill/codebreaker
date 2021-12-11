module Backend exposing (app)

import Dict exposing (Dict)
import Env
import Lamdera exposing (ClientId, SessionId)
import List.Extra
import Set
import Task
import Time
import Types exposing (..)
import Types.GameDict as GameDict
import Types.GameName as GameName


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
    ( { inGame = GameDict.empty
      , games = Dict.empty
      , connected = Dict.empty
      , adminSessions = Set.empty
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
            ( { model
                | connected =
                    Dict.update sessionId
                        (Maybe.withDefault Set.empty >> Set.insert clientId >> Just)
                        model.connected
              }
            , (case GameDict.getGameFor id model.inGame of
                Nothing ->
                    if Set.member id model.adminSessions then
                        FrontendAdminAuthenticated (toAdminModel model.games)

                    else
                        FrontendHomepage defaultHomepageModel

                Just gameName ->
                    case Dict.get (GameName.toString gameName) model.games of
                        Nothing ->
                            FrontendHomepage defaultHomepageModel

                        Just game ->
                            Tuple.second <| toInnerFrontendModel id gameName game
              )
                |> TFReplaceModel
                |> Lamdera.sendToFrontend id
            )

        ClientDisconnected sessionId clientId ->
            ( { model
                | connected =
                    Dict.update sessionId
                        (Maybe.andThen
                            (\clientIds ->
                                let
                                    newClientIds =
                                        Set.remove clientId clientIds
                                in
                                if Set.isEmpty newClientIds then
                                    Nothing

                                else
                                    Just newClientIds
                            )
                        )
                        model.connected
                , adminSessions = Set.remove clientId model.adminSessions
              }
            , Cmd.none
            )

        Timed now id submsg ->
            let
                ( model_, cmd, shouldSendToAdmin ) =
                    fromFrontendTimed now id submsg model
            in
            ( model_
            , if shouldSendToAdmin then
                Cmd.batch <| cmd :: List.map (sendToAdmin model_) (Set.toList model_.adminSessions)

              else
                cmd
            )


fromFrontendTimed : Time.Posix -> Id -> ToBackend -> BackendModel -> ( BackendModel, Cmd BackendMsg, Bool )
fromFrontendTimed now id msg model =
    case msg of
        TBUpsertGame data ->
            if String.isEmpty data.username then
                ( model, Lamdera.sendToFrontend id <| TFError "Invalid empty user name", False )

            else if String.isEmpty data.gameName then
                ( model, Lamdera.sendToFrontend id <| TFError "Invalid empty game name", False )

            else
                let
                    newGame =
                        case Dict.get (GameName.fromString data.gameName) model.games of
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
                                                , lastAction = now
                                            }

                                    BackendPlaying _ ->
                                        game

                            Nothing ->
                                BackendPreparing
                                    { shared =
                                        { colors = 8
                                        , codeLength = ""
                                        }
                                    , players =
                                        Dict.singleton id
                                            { ready = False
                                            , code = []
                                            , username = data.username
                                            }
                                    , lastAction = now
                                    }
                in
                ( { model
                    | inGame = Dict.insert id data.gameName model.inGame
                    , games = Dict.insert (normalizeGameName data.gameName) newGame model.games
                  }
                , Lamdera.sendToFrontend id <| TFReplaceModel <| Tuple.second <| toInnerFrontendModel id data.gameName newGame
                , True
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
                        , lastAction = now
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
                        , lastAction = now
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
                                , lastAction = now
                            }
                    in
                    if Dict.size newGame.players >= 2 && List.all .ready (Dict.values newGame.players) then
                        let
                            shared =
                                sharedPreparingParse newGame.shared
                        in
                        ( { codes = Dict.map (\_ { code } -> code) newGame.players
                          , shared =
                                { colors = shared.colors
                                , codeLength = shared.codeLength
                                , startTime = now
                                , players =
                                    Dict.map
                                        (\pid { username } ->
                                            { username = username
                                            , history = []
                                            , model = Guessing { current = [] }
                                            , opponentId = getOpponent pid newGame.players
                                            }
                                        )
                                        newGame.players
                                }
                          , lastAction = now
                          }
                            |> BackendPlaying
                        , Cmd.none
                        )

                    else
                        ( BackendPreparing newGame, Cmd.none )
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
                                            Dict.get player.opponentId playing.codes |> Maybe.withDefault []

                                        answer =
                                            getAnswer code current

                                        newPlayer =
                                            { player
                                                | history = ( current, answer ) :: player.history
                                                , model =
                                                    if answer.black == shared.codeLength then
                                                        Won { winTime = now }

                                                    else
                                                        Guessing { current = [] }
                                            }
                                    in
                                    ( { playing
                                        | shared =
                                            { shared
                                                | players = Dict.insert id newPlayer shared.players
                                            }
                                        , lastAction = now
                                      }
                                        |> BackendPlaying
                                    , Cmd.none
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
                        ( { shared =
                                { codeLength = String.fromInt playing.shared.codeLength
                                , colors = playing.shared.colors
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
                          , lastAction = now
                          }
                            |> BackendPreparing
                        , Cmd.none
                        )

                    else
                        ( BackendPlaying playing, Cmd.none )
                )

        TBHome ->
            ( { model | inGame = Dict.remove id model.inGame }
            , Lamdera.sendToFrontend id <| TFReplaceModel (FrontendHomepage defaultHomepageModel)
            , False
            )

        TBGameSettings shared ->
            updateGame id
                model
                (\preparing ->
                    ( { preparing
                        | shared = shared
                        , players = Dict.map (\_ player -> { player | ready = False, code = [] }) preparing.players
                        , lastAction = now
                      }
                        |> BackendPreparing
                    , Cmd.none
                    )
                )
                (\playing -> ( BackendPlaying playing, Cmd.none ))

        TBAdminAuthenticate password ->
            if password == Env.adminPassword then
                ( { model | adminSessions = Set.insert id model.adminSessions }
                , sendToAdmin model id
                , False
                )

            else
                ( model, Cmd.none, False )

        TBAdmin (AdminDelete gameName) ->
            let
                normalized =
                    normalizeGameName gameName

                ids =
                    model.inGame
                        |> Dict.toList
                        |> List.filter (\( _, name ) -> normalizeGameName name == normalized)
                        |> List.map Tuple.first

                cmds =
                    ids
                        |> List.map
                            (\pid ->
                                Lamdera.sendToFrontend pid (TFReplaceModel (FrontendHomepage defaultHomepageModel))
                            )
                        |> Cmd.batch
            in
            ( { model | games = Dict.remove normalized model.games }
            , cmds
            , True
            )


sendToAdmin : BackendModel -> Id -> Cmd msg
sendToAdmin model id =
    Lamdera.sendToFrontend id <|
        TFReplaceModel <|
            FrontendAdminAuthenticated <|
                toAdminModel model.games


toAdminModel : Dict String BackendGameModel -> FrontendAdminModel
toAdminModel games =
    games
        |> Dict.toList
        |> List.foldl
            (\( gameName, game ) acc ->
                case toInnerFrontendModel "admin" (GameName gameName) game of
                    ( lastAction, FrontendPreparing preparing ) ->
                        { acc | preparing = ( lastAction, preparing ) :: acc.preparing }

                    ( lastAction, FrontendPlaying playing ) ->
                        { acc | playing = ( lastAction, playing ) :: acc.playing }

                    _ ->
                        acc
            )
            { preparing = []
            , playing = []
            }


getOpponent : Id -> Dict Id a -> Id
getOpponent id dict =
    let
        keys =
            Dict.keys dict

        go x =
            case x of
                [] ->
                    ""

                [ q ] ->
                    q

                h1 :: ((h2 :: _) as t) ->
                    if h1 == id then
                        h2

                    else
                        go t
    in
    go (keys ++ List.take 1 keys)


toId : SessionId -> ClientId -> SessionId
toId sessionId _ =
    sessionId


defaultHomepageModel : FrontendHomepageModel
defaultHomepageModel =
    { gameName = GameName ""
    , username = ""
    }


toInnerFrontendModel : Id -> GameName -> BackendGameModel -> ( Time.Posix, InnerFrontendModel )
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
            ( preparing.lastAction
            , FrontendPreparing
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
            )

        BackendPlaying playing ->
            ( playing.lastAction
            , FrontendPlaying
                { shared = playing.shared
                , me = id
                , code = Dict.get id playing.codes
                , gameName = gameName
                }
            )


updateFromFrontend : SessionId -> ClientId -> ToBackend -> BackendModel -> ( BackendModel, Cmd BackendMsg )
updateFromFrontend sessionId clientId msg model =
    let
        id =
            toId sessionId clientId
    in
    ( model, Time.now |> Task.perform (\now -> Timed now id msg) )


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
    -> (BackendPreparingModel -> ( BackendGameModel, Cmd msg ))
    -> (BackendPlayingModel -> ( BackendGameModel, Cmd msg ))
    -> ( BackendModel, Cmd msg, Bool )
updateGame id model updatePreparing updatePlaying =
    case Dict.get id model.inGame of
        Nothing ->
            ( model, Cmd.none, False )

        Just gameName ->
            case Dict.get (normalizeGameName gameName) model.games of
                Nothing ->
                    ( model, Cmd.none, False )

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
                                |> Tuple.second
                                |> TFReplaceModel
                                |> Lamdera.sendToFrontend cid
                    in
                    ( { model | games = Dict.insert (normalizeGameName gameName) newGame model.games }
                    , playerIds
                        |> List.map send
                        |> (::) additionalCmd
                        |> Cmd.batch
                    , True
                    )


subscriptions : BackendModel -> Sub BackendMsg
subscriptions _ =
    Sub.batch
        [ Lamdera.onConnect ClientConnected
        , Lamdera.onDisconnect ClientDisconnected
        ]
