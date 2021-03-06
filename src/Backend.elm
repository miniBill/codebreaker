module Backend exposing (app)

import Any.Dict
import Dict
import Env
import Lamdera exposing (ClientId, SessionId)
import List.Extra
import Set
import Task
import Time
import Types exposing (..)
import Types.GameDict as GameDict
import Types.GameName as GameName exposing (GameName)
import Types.Id as Id exposing (Id)


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
      , games = GameName.dict.empty
      , connected = Dict.empty
      , adminSessions = Id.set.empty
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
                    if Id.set.member id model.adminSessions then
                        FrontendAdminAuthenticated (toAdminModel model.games)

                    else
                        FrontendHomepage defaultHomepageModel

                Just gameName ->
                    case GameName.dict.get gameName model.games of
                        Nothing ->
                            FrontendHomepage defaultHomepageModel

                        Just game ->
                            Tuple.second <| toInnerFrontendModel id gameName game
              )
                |> TFReplaceModel
                |> sendToFrontend id
            )

        ClientDisconnected sessionId clientId ->
            let
                id =
                    toId sessionId clientId
            in
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
                , adminSessions = Id.set.remove id model.adminSessions
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
                Cmd.batch <| cmd :: List.map (sendToAdmin model_) (Id.set.toList model_.adminSessions)

              else
                cmd
            )


sendToFrontend : Id -> ToFrontend -> Cmd msg
sendToFrontend id =
    Lamdera.sendToFrontend (Id.toSessionId id)


fromFrontendTimed : Time.Posix -> Id -> ToBackend -> BackendModel -> ( BackendModel, Cmd BackendMsg, Bool )
fromFrontendTimed now id msg model =
    case msg of
        TBUpsertGame data ->
            if String.isEmpty data.username then
                ( model, sendToFrontend id <| TFError "Invalid empty user name", False )

            else if String.isEmpty data.gameName then
                ( model, sendToFrontend id <| TFError "Invalid empty game name", False )

            else
                let
                    gameName =
                        GameName.fromString data.gameName

                    newGame =
                        case GameName.dict.get gameName model.games of
                            Just game ->
                                case game of
                                    BackendPreparing preparing ->
                                        BackendPreparing
                                            { preparing
                                                | players =
                                                    Id.dict.insert id
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
                                        , canonicalName = data.gameName
                                        }
                                    , players =
                                        Id.dict.singleton id
                                            { ready = False
                                            , code = []
                                            , username = data.username
                                            }
                                    , lastAction = now
                                    }
                in
                ( { model
                    | inGame = GameDict.addPlayerToGame id gameName model.inGame
                    , games = GameName.dict.insert gameName newGame model.games
                  }
                , sendToFrontend id <|
                    TFReplaceModel <|
                        Tuple.second <|
                            toInnerFrontendModel id gameName newGame
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
                            Id.dict.update
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
                                    Id.dict.update
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
                                    Id.dict.update
                                        id
                                        (Maybe.map setReady)
                                        preparing.players
                                , lastAction = now
                            }
                    in
                    if Id.dict.size newGame.players >= 2 && List.all .ready (Id.dict.values newGame.players) then
                        let
                            shared =
                                sharedPreparingParse newGame.shared

                            newShared : SharedPlayingModel
                            newShared =
                                { colors = shared.colors
                                , codeLength = shared.codeLength
                                , startTime = now
                                , canonicalName = shared.canonicalName
                                , players =
                                    Id.dict.map
                                        (\pid { username } ->
                                            { username = username
                                            , history = []
                                            , model = Guessing { current = [] }
                                            , opponentId = getOpponent pid newGame.players
                                            }
                                        )
                                        newGame.players
                                }

                            playing : BackendPlayingModel
                            playing =
                                { codes = Id.dict.map (\_ { code } -> code) newGame.players
                                , shared = newShared
                                , lastAction = now
                                }
                        in
                        ( BackendPlaying playing
                        , Cmd.none
                        )

                    else
                        ( BackendPreparing newGame, Cmd.none )
                )
                (\({ shared } as playing) ->
                    case Id.dict.get id shared.players of
                        Nothing ->
                            ( BackendPlaying playing, Cmd.none )

                        Just player ->
                            case player.model of
                                Won _ ->
                                    ( BackendPlaying playing, Cmd.none )

                                Guessing { current } ->
                                    let
                                        code =
                                            Id.dict.get player.opponentId playing.codes |> Maybe.withDefault []

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
                                                | players = Id.dict.insert id newPlayer shared.players
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
                            (Id.dict.values shared.players)
                    then
                        ( { shared =
                                { codeLength = String.fromInt playing.shared.codeLength
                                , colors = playing.shared.colors
                                , canonicalName = shared.canonicalName
                                }
                          , players =
                                Id.dict.map
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
            ( { model | inGame = GameDict.remove id model.inGame }
            , sendToFrontend id <| TFReplaceModel (FrontendHomepage defaultHomepageModel)
            , False
            )

        TBGameSettings shared ->
            updateGame id
                model
                (\preparing ->
                    ( { preparing
                        | shared = shared
                        , players =
                            Id.dict.map
                                (\_ player -> { player | ready = False, code = [] })
                                preparing.players
                        , lastAction = now
                      }
                        |> BackendPreparing
                    , Cmd.none
                    )
                )
                (\playing -> ( BackendPlaying playing, Cmd.none ))

        TBAdminAuthenticate password ->
            if password == Env.adminPassword then
                ( { model | adminSessions = Id.set.insert id model.adminSessions }
                , sendToAdmin model id
                , False
                )

            else
                ( model, Cmd.none, False )

        TBAdmin (AdminDelete gameName) ->
            let
                ids =
                    GameDict.getIdsFor gameName model.inGame

                cmds =
                    ids
                        |> Id.set.toList
                        |> List.map
                            (\pid ->
                                sendToFrontend pid (TFReplaceModel (FrontendHomepage defaultHomepageModel))
                            )
                        |> Cmd.batch
            in
            ( { model | games = GameName.dict.remove gameName model.games }
            , cmds
            , True
            )


sendToAdmin : BackendModel -> Id -> Cmd msg
sendToAdmin model id =
    sendToFrontend id <|
        TFReplaceModel <|
            FrontendAdminAuthenticated <|
                toAdminModel model.games


toAdminModel : Any.Dict.Dict GameName BackendGameModel String -> FrontendAdminModel
toAdminModel games =
    games
        |> GameName.dict.toList
        |> List.foldl
            (\( gameName, game ) acc ->
                case toInnerFrontendModel (Id.fromSessionId "admin") gameName game of
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


getOpponent : Id -> Id.Dict a -> Id
getOpponent id dict =
    let
        keys =
            Id.dict.keys dict

        go x =
            case x of
                [] ->
                    Id.fromSessionId ""

                [ q ] ->
                    q

                h1 :: ((h2 :: _) as t) ->
                    if h1 == id then
                        h2

                    else
                        go t
    in
    go (keys ++ List.take 1 keys)


toId : SessionId -> ClientId -> Id
toId sessionId _ =
    Id.fromSessionId sessionId


defaultHomepageModel : FrontendHomepageModel
defaultHomepageModel =
    { gameName = ""
    , username = ""
    }


toInnerFrontendModel : Id -> GameName -> BackendGameModel -> ( Time.Posix, InnerFrontendModel )
toInnerFrontendModel id gameName game =
    case game of
        BackendPreparing preparing ->
            let
                player =
                    Id.dict.get id preparing.players
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
                    Id.dict.map
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
                , code = Id.dict.get id playing.codes
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
    case GameDict.getGameFor id model.inGame of
        Nothing ->
            ( model, Cmd.none, False )

        Just gameName ->
            case GameName.dict.get gameName model.games of
                Nothing ->
                    ( model, Cmd.none, False )

                Just game ->
                    let
                        ( ( newGame, additionalCmd ), playerIds ) =
                            case game of
                                BackendPreparing preparing ->
                                    ( updatePreparing preparing
                                    , Id.dict.keys preparing.players
                                    )

                                BackendPlaying playing ->
                                    ( updatePlaying playing
                                    , Id.dict.keys playing.shared.players
                                    )

                        send cid =
                            toInnerFrontendModel cid gameName newGame
                                |> Tuple.second
                                |> TFReplaceModel
                                |> sendToFrontend cid
                    in
                    ( { model | games = GameName.dict.insert gameName newGame model.games }
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
