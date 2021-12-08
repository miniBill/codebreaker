module Frontend exposing (app)

import Browser exposing (UrlRequest(..))
import Browser.Dom
import Browser.Navigation as Nav exposing (Key)
import Dict
import Element.WithContext as Element exposing (alignBottom, alignRight, alignTop, centerX, centerY, el, fill, height, inFront, padding, paddingXY, px, spacing, text, width)
import Element.WithContext.Background as Background
import Element.WithContext.Border as Border
import Element.WithContext.Extra as Extra
import Element.WithContext.Font as Font
import Element.WithContext.Input as Input
import Html.Attributes
import Lamdera
import List.Extra
import Task
import Theme exposing (Attribute, Element)
import Types exposing (..)
import Url exposing (Url)
import Url.Builder
import Url.Parser


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


urlParser : Url.Parser.Parser (String -> c) c
urlParser =
    Url.Parser.oneOf
        [ Url.Parser.map (\s -> Maybe.withDefault s <| Url.percentDecode s) Url.Parser.string
        , Url.Parser.map "" <| Url.Parser.top
        ]


init : Url -> Nav.Key -> ( FrontendModel, Cmd FrontendMsg )
init url key =
    ( { key = key
      , inner =
            url
                |> Url.Parser.parse urlParser
                |> Maybe.withDefault ""
                |> String.replace "-" " "
                |> GameName
                |> FrontendConnecting
      , error = ""
      , colorblindMode = False
      , rootUrl = getRootUrl url
      }
    , Cmd.none
    )


getRootUrl : Url -> String
getRootUrl { protocol, host, port_ } =
    let
        protocolString =
            case protocol of
                Url.Http ->
                    "http"

                Url.Https ->
                    "https"

        portString =
            case port_ of
                Nothing ->
                    ""

                Just p ->
                    ":" ++ String.fromInt p
    in
    protocolString ++ "://" ++ host ++ portString


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
                    let
                        inner =
                            FrontendHomepage homepage
                    in
                    ( { model | inner = inner }
                    , Nav.replaceUrl model.key <| innerModelToUrl inner
                    )

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

        SetGameSettings settings ->
            ( model, Lamdera.sendToBackend <| TBGameSettings settings )

        Submit ->
            ( model, Lamdera.sendToBackend TBSubmit )

        ColorblindMode colorblindMode ->
            ( { model | colorblindMode = colorblindMode }, Cmd.none )

        NewGame ->
            ( model, Lamdera.sendToBackend TBNewGame )

        Home ->
            ( model, Lamdera.sendToBackend TBHome )

        FrontendNoop ->
            ( model, Cmd.none )


updateFromBackend : ToFrontend -> FrontendModel -> ( FrontendModel, Cmd FrontendMsg )
updateFromBackend msg model =
    case msg of
        TFReplaceModel inner ->
            let
                ( inner_, cmd ) =
                    case ( model.inner, inner ) of
                        ( FrontendConnecting gameName, FrontendHomepage homepage ) ->
                            ( FrontendHomepage { homepage | gameName = gameName }
                            , if normalizeGameName gameName == "" then
                                focus ids.gamename

                              else
                                focus ids.username
                            )

                        _ ->
                            ( inner, Cmd.none )
            in
            ( { model | inner = inner_ }
            , Cmd.batch
                [ cmd
                , Nav.replaceUrl model.key <| innerModelToUrl inner_
                ]
            )

        TFError error ->
            ( { model | error = error }, Cmd.none )


focus : DomId -> Cmd FrontendMsg
focus (DomId id) =
    Browser.Dom.focus id
        |> Task.attempt (\_ -> FrontendNoop)


innerModelToUrl : InnerFrontendModel -> String
innerModelToUrl model =
    gameNameToUrl <| getGameName model


gameNameToUrl : GameName -> String
gameNameToUrl gameName =
    Url.Builder.absolute
        [ String.replace " " "-" (normalizeGameName gameName) ]
        []


getGameName : InnerFrontendModel -> GameName
getGameName model =
    case model of
        FrontendConnecting gameName ->
            gameName

        FrontendHomepage { gameName } ->
            gameName

        FrontendPreparing { gameName } ->
            gameName

        FrontendPlaying { gameName } ->
            gameName


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
        maybeHomeButton active =
            case model.inner of
                FrontendConnecting _ ->
                    Element.none

                FrontendHomepage _ ->
                    Element.none

                FrontendPreparing _ ->
                    homeButton active

                FrontendPlaying _ ->
                    homeButton active

        header =
            Theme.row
                [ width fill
                , Font.bold
                , Font.center
                , Theme.fontSizes.big
                , padding 0
                ]
                [ maybeHomeButton True
                , el [ centerX, centerY ] <| text <| title model
                , maybeHomeButton False
                ]

        body =
            case model.inner of
                FrontendConnecting _ ->
                    [ text "Connecting to server" ]

                FrontendHomepage homepage ->
                    viewHomepage model.error homepage

                FrontendPreparing preparing ->
                    viewPreparing model preparing

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

        homeButton active =
            if active then
                Theme.button []
                    { onPress = Home
                    , label = text "🏠"
                    }

            else
                el [ Element.transparent True ] <| text "🏠"
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


viewPreparing : { a | rootUrl : String, colorblindMode : Bool } -> PreparingFrontendModel -> List (Element FrontendMsg)
viewPreparing { rootUrl, colorblindMode } ({ shared } as preparingModel) =
    let
        me =
            Tuple.second preparingModel.me

        isInt x =
            String.toInt x /= Nothing

        sharedInput =
            Theme.column [ padding 0, centerX ]
                [ Theme.column [ padding 0 ]
                    [ text "The game URL is: "
                    , let
                        url =
                            rootUrl ++ gameNameToUrl preparingModel.gameName
                      in
                      Element.link [ Font.underline ] { url = url, label = text url }
                    ]
                , el [ width fill ] <|
                    Theme.input []
                        { validate = isInt
                        , label = "Length"
                        , text = shared.codeLength
                        , onChange = \newLength -> SetGameSettings { shared | codeLength = newLength }
                        , placeholder = "4"
                        }
                , text "Colors"
                , let
                    rows =
                        3

                    rowSize =
                        (List.length Theme.colors + (rows - 1)) // rows
                  in
                  Theme.colors
                    |> List.Extra.greedyGroupsOf rowSize
                    |> List.indexedMap
                        (\rowIndex ->
                            List.indexedMap
                                (\columnIndex color ->
                                    let
                                        index =
                                            rowIndex * rowSize + columnIndex

                                        active =
                                            index < shared.colors

                                        borderColor direction colored =
                                            Element.htmlAttribute <|
                                                Html.Attributes.style ("border-" ++ direction ++ "-color")
                                                    (if colored then
                                                        "black"

                                                     else
                                                        "white"
                                                    )
                                    in
                                    Input.button
                                        [ Theme.borderRounded
                                        , Element.htmlAttribute <|
                                            Html.Attributes.style "filter" <|
                                                if active || not colorblindMode then
                                                    ""

                                                else
                                                    "grayscale(100%)"
                                        ]
                                        { onPress = Just <| SetGameSettings { shared | colors = index + 1 }
                                        , label =
                                            { color
                                                | backgroundColor =
                                                    if active then
                                                        color.backgroundColor

                                                    else
                                                        Theme.desaturate color.backgroundColor
                                            }
                                                |> viewColor
                                        }
                                        |> el
                                            [ Element.padding 4
                                            , Border.widthEach
                                                { left =
                                                    if columnIndex == 0 then
                                                        1

                                                    else
                                                        0
                                                , top =
                                                    if rowIndex == 0 then
                                                        1

                                                    else
                                                        0
                                                , right = 1
                                                , bottom = 1
                                                }
                                            , borderColor "top" <|
                                                if rowIndex == 0 then
                                                    active

                                                else
                                                    False
                                            , borderColor "left" <|
                                                if columnIndex == 0 then
                                                    active

                                                else
                                                    False
                                            , borderColor "right" <|
                                                if columnIndex == rowSize - 1 then
                                                    active

                                                else
                                                    index == shared.colors - 1
                                            , borderColor "bottom" <|
                                                if rowIndex == rows - 1 then
                                                    active

                                                else
                                                    active && index + rowSize >= shared.colors
                                            ]
                                )
                                >> Element.row []
                        )
                    |> Element.column []
                ]

        viewOthers =
            preparingModel.players
                |> Dict.toList
                |> List.filter (\( id, _ ) -> id /= Tuple.first preparingModel.me)
                |> List.map
                    (\( _, { username, ready } ) ->
                        if ready then
                            text <| username ++ ": Ready"

                        else
                            text <| username ++ ": Preparing"
                    )
                |> Theme.column [ padding 0, centerX ]

        sharedParsed =
            preparingSharedParse shared
    in
    sharedInput
        :: (if me.ready then
                [ el [ centerX ] <| Element.text "Wait for other players"
                , viewCode [ Theme.borderWidth, centerX ] me.code
                ]

            else
                [ el [ centerX ] <| text <| "Set your secret code, " ++ me.username
                , Element.map SetCode <|
                    codeInput (preparingSharedParse shared) me.code
                ]
           )
        ++ [ if
                List.all ((/=) -1) me.code
                    && (List.length me.code == sharedParsed.codeLength)
                    && not me.ready
             then
                Theme.button [ centerX ] { onPress = Submit, label = text "Ready" }

             else
                Element.none
           , Theme.column [ padding 0, centerX ]
                [ el [ Font.bold, centerX ] <| text "Other players"
                , viewOthers
                ]
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
    in
    [ text "Welcome to Codebreaker!"
    , Theme.row [ padding 0, width fill ]
        [ Theme.input [ Extra.onEnter UpsertGame, idAttribute ids.gamename ]
            { validate = always True
            , label = "Game name"
            , text = rawGameName homepageModel.gameName
            , placeholder = "Game name"
            , onChange = \newGameName -> HomepageMsg { homepageModel | gameName = GameName newGameName }
            }
        , Theme.input [ Extra.onEnter UpsertGame, idAttribute ids.username ]
            { validate = always True
            , label = "User name"
            , text = homepageModel.username
            , placeholder = "User name"
            , onChange = \newUsername -> HomepageMsg { homepageModel | username = newUsername }
            }
        ]
    , errorView
    , Theme.row [ padding 0, width fill ]
        [ Theme.button [ width fill ]
            { onPress = UpsertGame
            , label = text "Create or join game"
            }
        ]
    ]


type DomId
    = DomId String


idAttribute : DomId -> Attribute FrontendMsg
idAttribute (DomId id) =
    Element.htmlAttribute <| Html.Attributes.id id


ids : { gamename : DomId, username : DomId }
ids =
    { gamename = DomId "gamename"
    , username = DomId "username"
    }


title : FrontendModel -> String
title { inner } =
    case inner of
        FrontendPlaying { gameName } ->
            "Codebreaker! - " ++ rawGameName gameName

        FrontendPreparing { gameName } ->
            "Codebreaker! - " ++ rawGameName gameName

        _ ->
            "Codebreaker!"
