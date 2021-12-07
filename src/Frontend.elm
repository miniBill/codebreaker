module Frontend exposing (..)

import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav exposing (Key)
import Dict
import Element.WithContext as Element exposing (alignBottom, alignRight, alignTop, centerX, centerY, el, fill, height, inFront, padding, paddingXY, px, spacing, text, width)
import Element.WithContext.Background as Background
import Element.WithContext.Border as Border
import Element.WithContext.Font as Font
import Element.WithContext.Input as Input exposing (username)
import Lamdera
import List.Extra
import Theme exposing (Attribute, Element)
import Types exposing (..)
import Url


app :
    { init : Lamdera.Url -> Key -> ( FrontendModel, Cmd FrontendMsg )
    , view : FrontendModel -> Browser.Document FrontendMsg
    , update : FrontendMsg -> FrontendModel -> ( FrontendModel, Cmd FrontendMsg )
    , updateFromBackend : ToFrontend -> FrontendModel -> ( FrontendModel, Cmd FrontendMsg )
    , subscriptions : FrontendModel -> Sub FrontendMsg
    , onUrlRequest : UrlRequest -> FrontendMsg
    , onUrlChange : Url.Url -> FrontendMsg
    }
app =
    Lamdera.frontend
        { init = init
        , onUrlRequest = UrlClicked
        , onUrlChange = UrlChanged
        , update = update
        , updateFromBackend = updateFromBackend
        , subscriptions = \_ -> Sub.none
        , view = outerView
        }


init : Url.Url -> Nav.Key -> ( FrontendModel, Cmd FrontendMsg )
init _ key =
    ( { key = key
      , inner = FrontendConnecting
      , error = ""
      , colorblindMode = False
      }
    , Cmd.none
    )


update : FrontendMsg -> FrontendModel -> ( FrontendModel, Cmd FrontendMsg )
update msg model =
    case msg of
        UrlClicked urlRequest ->
            case urlRequest of
                Internal url ->
                    ( model
                    , Nav.pushUrl model.key (Url.toString url)
                    )

                External url ->
                    ( model
                    , Nav.load url
                    )

        UrlChanged _ ->
            ( model, Cmd.none )

        HomepageMsg homepage ->
            case model.inner of
                FrontendHomepage _ ->
                    ( { model | inner = FrontendHomepage homepage }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        UpsertGame ->
            case model.inner of
                FrontendHomepage homepage ->
                    ( model, Lamdera.sendToBackend <| TBUpsertGame homepage )

                _ ->
                    ( model, Cmd.none )

        SetCode code ->
            ( model, Lamdera.sendToBackend <| TBCode code )

        Submit ->
            ( model, Lamdera.sendToBackend TBSubmit )

        ColorblindMode colorblindMode ->
            ( { model | colorblindMode = colorblindMode }, Cmd.none )

        NewGame ->
            ( model, Lamdera.sendToBackend TBNewGame )

        Home ->
            ( model, Lamdera.sendToBackend TBHome )


updateFromBackend : ToFrontend -> FrontendModel -> ( FrontendModel, Cmd FrontendMsg )
updateFromBackend msg model =
    case msg of
        TFReplaceModel inner ->
            ( { model | inner = inner }, Cmd.none )

        TFError error ->
            ( { model | error = error }, Cmd.none )


outerView : FrontendModel -> Browser.Document FrontendMsg
outerView model =
    { title = title model
    , body =
        [ Element.layout
            { colorblindMode = model.colorblindMode }
            [ Theme.fontSizes.normal
            , width fill
            , height fill
            ]
            (view model)
        ]
    }


view : FrontendModel -> Element FrontendMsg
view model =
    let
        header =
            Theme.row
                [ width fill
                , Font.bold
                , Font.center
                , Theme.fontSizes.big
                , padding 0
                , inFront <|
                    case model.inner of
                        FrontendConnecting ->
                            Element.none

                        FrontendHomepage _ ->
                            Element.none

                        FrontendPreparing _ ->
                            Theme.button []
                                { onPress = Home
                                , label = text "🏠"
                                }

                        FrontendPlaying _ ->
                            Theme.button []
                                { onPress = Home
                                , label = text "🏠"
                                }
                ]
                [ el [ centerX ] <| text <| title model
                ]

        body =
            case model.inner of
                FrontendConnecting ->
                    [ text "Connecting to server" ]

                FrontendHomepage homepage ->
                    viewHomepage model.error homepage

                FrontendPreparing preparing ->
                    viewPreparing preparing

                FrontendPlaying playing ->
                    viewPlaying playing

        accessibility =
            [ el [ centerX ] <|
                Input.checkbox []
                    { onChange = ColorblindMode
                    , checked = model.colorblindMode
                    , label = Input.labelRight [] <| text "Colorblind mode"
                    , icon = Input.defaultCheckbox
                    }
            ]
    in
    Theme.column [ width fill, height fill ]
        (header :: body ++ accessibility)


viewPlaying : PlayingFrontendModel -> List (Element FrontendMsg)
viewPlaying playingModel =
    let
        config =
            { codeLength = playingModel.shared.codeLength
            , colors = playingModel.shared.colors
            }

        meViews =
            case Dict.get playingModel.me playingModel.shared.players of
                Just me ->
                    viewMePlaying config me

                Nothing ->
                    []

        othersViews =
            playingModel.shared.players
                |> Dict.toList
                |> List.filter (\( i, _ ) -> i /= playingModel.me)
                |> List.map Tuple.second
                |> List.map (viewOther config)
    in
    [ case playingModel.code of
        Nothing ->
            el [ centerX ] <| text "Spectating"

        Just code ->
            Theme.row [ padding 0, centerX ] [ text "Your code: ", viewCode [ Theme.borderWidth ] code ]
    , Theme.wrappedRow [ padding 0, centerX ] (meViews ++ othersViews)
    , if
        playingModel.shared.players
            |> Dict.values
            |> List.all
                (\{ model } ->
                    case model of
                        Won _ ->
                            True

                        Guessing _ ->
                            False
                )
      then
        Theme.button [ centerX ] { onPress = NewGame, label = text "New game" }

      else
        Element.none
    ]


viewMePlaying :
    { codeLength : Int, colors : Int }
    -> { username : String, history : PlayerMoves, model : PlayerModel }
    -> List (Element FrontendMsg)
viewMePlaying ({ codeLength } as config) data =
    let
        shared =
            viewPlayingPlayer config
                { data | username = "You" }
                (case data.model of
                    Won _ ->
                        [ text "You guessed the code!" ]

                    Guessing { current } ->
                        [ el [ centerX ] <| text "Still guessing..."
                        , viewCode [ centerX, padding 0 ] (padCode codeLength current)
                        ]
                )
    in
    case data.model of
        Won _ ->
            [ shared ]

        Guessing { current } ->
            [ Theme.row [ padding 0, alignTop ]
                [ shared
                , Theme.column
                    [ Theme.borderRounded
                    , Theme.borderWidth
                    , alignBottom
                    ]
                    [ el [ centerX, Font.bold ] <| text "Your next guess:"
                    , Element.map SetCode <|
                        codeInput config current
                    , Theme.button
                        [ Element.transparent <|
                            List.any ((==) -1) current
                                || (List.length current /= codeLength)
                        , centerX
                        ]
                        { onPress = Submit, label = text "Submit" }
                    ]
                ]
            ]


viewOther :
    { a | codeLength : Int }
    -> { username : String, history : PlayerMoves, model : PlayerModel }
    -> Element msg
viewOther ({ codeLength } as config) data =
    viewPlayingPlayer config
        data
        (case data.model of
            Won _ ->
                [ text "They guessed the code!" ]

            Guessing { current } ->
                [ el [ centerX ] <| text "Is guessing"
                , viewCode [ centerX, padding 0 ] (padCode codeLength current)
                ]
        )


viewPlayingPlayer : { a | codeLength : Int } -> { username : String, history : PlayerMoves, model : PlayerModel } -> List (Element msg) -> Element msg
viewPlayingPlayer config { username, history, model } rest =
    Theme.column
        [ Theme.borderWidth
        , Theme.borderRounded
        , alignTop
        , case model of
            Won _ ->
                Background.color <| Element.rgb 0.7 0.7 1

            Guessing _ ->
                Background.color <| Element.rgb 1 1 1
        ]
        ([ el [ Font.bold, centerX ] <| text username
         , viewHistory config history
         ]
            ++ rest
        )


viewHistory : { a | codeLength : Int } -> PlayerMoves -> Element msg
viewHistory config moves =
    let
        expectedLength =
            max 8 (1 + List.length moves)

        paddedMoves =
            List.repeat (expectedLength - List.length moves) ( [], { white = 0, black = 0 } ) ++ moves
    in
    paddedMoves
        |> List.reverse
        |> List.indexedMap (\index -> viewHistoryLine config (index + 1))
        |> Theme.column [ padding 0 ]


viewHistoryLine : { a | codeLength : Int } -> Int -> ( Code, Answer ) -> Element msg
viewHistoryLine { codeLength } index ( code, answer ) =
    Theme.row [ padding 0, width fill ]
        [ el [ alignRight, Element.moveUp 2 ] <| text <| String.fromInt index
        , viewCode [ alignRight, padding 0 ] (padCode codeLength code)
        , viewAnswer codeLength answer
        ]


viewAnswer : Int -> Answer -> Element msg
viewAnswer codeLength { black, white } =
    List.repeat black ( Element.rgb 0 0 0, Element.rgb 0 0 0 )
        ++ List.repeat white ( Element.rgb 0 0 0, Element.rgb 1 1 1 )
        ++ List.repeat (codeLength - black - white) ( Element.rgb 0.6 0.6 0.6, Element.rgb 0.6 0.6 0.6 )
        |> List.map
            (\( border, color ) ->
                el
                    [ width <| px 8
                    , height <| px 8
                    , Border.rounded 20
                    , Border.width 1
                    , Border.color border
                    , Background.color color
                    ]
                    Element.none
            )
        |> List.Extra.greedyGroupsOf (codeLength // 2)
        |> List.map (Element.row [ spacing 2 ])
        |> Element.column [ spacing 2 ]


viewPreparing : PreparingFrontendModel -> List (Element FrontendMsg)
viewPreparing preparingModel =
    let
        me =
            Tuple.second preparingModel.me

        viewOthers =
            preparingModel.players
                |> Dict.toList
                |> List.filter (\( id, _ ) -> id /= Tuple.first preparingModel.me)
                |> List.map
                    (\( _, { username, ready } ) ->
                        text <|
                            username
                                ++ ": "
                                ++ (if ready then
                                        "Ready"

                                    else
                                        "Preparing"
                                   )
                    )
                |> Theme.column [ padding 0, centerX ]
    in
    (if me.ready then
        [ el [ centerX ] <| Element.text "Wait for other players"
        , viewCode [ Theme.borderWidth, centerX ] me.code
        ]

     else
        [ el [ centerX ] <| text <| "Set your secret code, " ++ me.username
        , Element.map SetCode <| codeInput preparingModel.shared me.code
        ]
    )
        ++ [ if
                List.all ((/=) -1) me.code
                    && (List.length me.code == preparingModel.shared.codeLength)
                    && not me.ready
             then
                Theme.button [ centerX ] { onPress = Submit, label = text "Ready" }

             else
                Element.none
           , el [ Font.bold, centerX ] <| text "Other players"
           , viewOthers
           ]


codeInput : { a | codeLength : Int, colors : Int } -> Code -> Element Code
codeInput { codeLength, colors } code =
    let
        paddedCode =
            padCode codeLength code

        digitInput index =
            Theme.colors
                |> List.take colors
                |> List.indexedMap
                    (\colorIndex color ->
                        Input.button
                            [ Border.rounded 20
                            ]
                            { onPress = Just <| List.Extra.setAt index colorIndex paddedCode
                            , label = viewColor color
                            }
                    )
                |> Theme.column [ padding 0 ]

        inputs =
            List.range 0 (codeLength - 1)
                |> List.map digitInput
                |> Theme.row [ paddingXY Theme.rythm 0, centerX ]

        current =
            viewCode [ Theme.borderWidth, centerX ] paddedCode
    in
    Theme.column [ padding 0, centerX ]
        [ inputs
        , current
        ]


padCode : Int -> Code -> Code
padCode codeLength code =
    code ++ List.repeat (codeLength - List.length code) -1


viewCode : List (Attribute msg) -> List Int -> Element msg
viewCode attrs =
    Theme.row attrs
        << List.map
            (\digit ->
                let
                    color =
                        List.Extra.getAt digit Theme.colors
                            |> Maybe.withDefault
                                { backgroundColor = Element.rgb 0.7 0.7 0.7
                                , symbol = "🕛"
                                }
                in
                el [ Border.rounded 20 ] <| viewColor color
            )


viewColor : { backgroundColor : Element.Color, symbol : String } -> Element msg
viewColor { backgroundColor, symbol } =
    Element.with .colorblindMode <| \colorblindMode ->
    if colorblindMode then
        el [ centerX, centerY, Theme.fontSizes.big ] <|
            text symbol

    else
        el
            [ width <| px 20
            , height <| px 20
            , Background.color backgroundColor
            , Border.rounded 20
            , Border.width 1
            , Border.color <| Element.rgb 0 0 0
            ]
            Element.none


viewHomepage : String -> HomepageModel -> List (Element FrontendMsg)
viewHomepage error homepageModel =
    let
        errorView =
            if String.isEmpty error then
                Element.none

            else
                el [ Theme.padding, Background.color <| Element.rgb 1 0.8 0.8, width fill ]
                    (text error)

        isInt x =
            String.toInt x /= Nothing

        input { validate, label, text, placeholder, onChange } =
            Input.text
                (if String.isEmpty text || validate text then
                    [ width fill ]

                 else
                    [ Background.color <| Element.rgb 1 0.8 0.8
                    , width fill
                    ]
                )
                { label = Input.labelAbove [] <| Element.text label
                , text = text
                , onChange = onChange
                , placeholder = Just <| Input.placeholder [] <| Element.text placeholder
                }

        row =
            Theme.row [ padding 0, width fill ]
    in
    [ text "Welcome to Codebreaker!"
    , row
        [ input
            { validate = always True
            , label = "Game name"
            , text = rawGameName homepageModel.gameName
            , placeholder = "Game name"
            , onChange = \newGameName -> HomepageMsg { homepageModel | gameName = GameName newGameName }
            }
        , input
            { validate = isInt
            , label = "Length"
            , text = homepageModel.codeLength
            , onChange = \newLength -> HomepageMsg { homepageModel | codeLength = newLength }
            , placeholder = "4"
            }
        ]
    , row
        [ input
            { validate = always True
            , label = "User name"
            , text = homepageModel.username
            , placeholder = "User name"
            , onChange = \newUsername -> HomepageMsg { homepageModel | username = newUsername }
            }
        , input
            { validate = isInt
            , label = "Colors"
            , text = homepageModel.colors
            , onChange = \newLength -> HomepageMsg { homepageModel | colors = newLength }
            , placeholder = "8"
            }
        ]
    , errorView
    , row
        [ Theme.button [ width fill ]
            { onPress = UpsertGame
            , label = text "Create or join game"
            }
        ]
    ]


title : FrontendModel -> String
title { inner } =
    case inner of
        FrontendPlaying { gameName } ->
            "Codebreaker! - " ++ rawGameName gameName

        FrontendPreparing { gameName } ->
            "Codebreaker! - " ++ rawGameName gameName

        _ ->
            "Codebreaker!"
