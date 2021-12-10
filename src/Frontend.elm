module Frontend exposing (app)

import Browser exposing (UrlRequest(..))
import Browser.Dom
import Browser.Navigation as Nav exposing (Key)
import Dict
import Element.WithContext as Element exposing (centerX, centerY, el, fill, height, padding, text, width)
import Element.WithContext.Background as Background
import Element.WithContext.Border as Border
import Element.WithContext.Extra as Extra
import Element.WithContext.Font as Font
import Element.WithContext.Input as Input
import Frontend.Common exposing (codeInput, viewCode, viewColor)
import Frontend.Game exposing (viewPlaying)
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


urlParser : Url.Parser.Parser (InnerFrontendModel -> c) c
urlParser =
    Url.Parser.oneOf
        [ Url.Parser.map
            (\s ->
                Url.percentDecode s
                    |> Maybe.withDefault s
                    |> String.replace "-" " "
                    |> GameName
                    |> FrontendConnecting
            )
            Url.Parser.string
        ]


init : Url -> Nav.Key -> ( FrontendModel, Cmd FrontendMsg )
init url key =
    ( { key = key
      , inner =
            url
                |> Url.Parser.parse urlParser
                |> Maybe.withDefault (FrontendConnecting <| GameName "")
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
                            ( if gameName == GameName "admin" then
                                FrontendAdminAuthenticating ""

                              else
                                FrontendHomepage { homepage | gameName = gameName }
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

        FrontendAdminAuthenticating _ ->
            GameName "admin"

        FrontendAdminAuthenticated _ ->
            GameName "admin"


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
                FrontendPreparing _ ->
                    homeButton active

                FrontendPlaying _ ->
                    homeButton active

                FrontendConnecting _ ->
                    Element.none

                FrontendHomepage _ ->
                    Element.none

                FrontendAdminAuthenticated _ ->
                    Element.none

                FrontendAdminAuthenticating _ ->
                    Element.none

        header =
            case title model of
                "" ->
                    Element.none

                t ->
                    Theme.row
                        [ width fill
                        , Font.bold
                        , Font.center
                        , Theme.fontSizes.big
                        , padding 0
                        ]
                        [ maybeHomeButton True
                        , el [ centerX, centerY ] <| text t
                        , maybeHomeButton False
                        ]

        body =
            case model.inner of
                FrontendConnecting _ ->
                    [ el [ centerX, centerY ] <| text "Connecting to server" ]

                FrontendAdminAuthenticating _ ->
                    [ text "TODO" ]

                FrontendAdminAuthenticated _ ->
                    [ text "TODO" ]

                FrontendHomepage homepage ->
                    viewHomepage model.error homepage ++ accessibility

                FrontendPreparing preparing ->
                    viewPreparing model preparing ++ accessibility

                FrontendPlaying playing ->
                    viewPlaying playing ++ accessibility

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
        (header :: body)


viewPreparing : { a | rootUrl : String, colorblindMode : Bool } -> PreparingFrontendModel -> List (Element FrontendMsg)
viewPreparing { rootUrl, colorblindMode } ({ shared } as preparingModel) =
    let
        me =
            Tuple.second preparingModel.me

        isInt x =
            String.toInt x /= Nothing

        sharedInput =
            [ text "The game URL is: "
            , let
                url =
                    rootUrl ++ gameNameToUrl preparingModel.gameName
              in
              Element.link [ Font.underline ] { url = url, label = text url }
            , Theme.input []
                { validate = isInt
                , label = "Length"
                , text = shared.codeLength
                , onChange = \newLength -> SetGameSettings { shared | codeLength = newLength }
                , placeholder = "4"
                }
            , text <| "Colors: " ++ String.fromInt shared.colors
            , let
                rowCount =
                    4

                rowSize =
                    (List.length Theme.colors + (rowCount - 1)) // rowCount
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
                                                    "transparent"
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
                                        [ padding <| Theme.rythm // 2
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
                                            if rowIndex == rowCount - 1 then
                                                active

                                            else
                                                active && index + rowSize >= shared.colors
                                        ]
                            )
                            >> Element.row []
                    )
                |> Element.column [ centerX ]
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

        sharedParsed =
            preparingSharedParse shared

        children =
            sharedInput
                ++ (if me.ready then
                        [ Element.text "Wait for other players"
                        , viewCode [ Theme.borderWidth, centerX ] me.code
                        ]

                    else
                        [ text <| "Set your secret code, " ++ me.username
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
                   , el [ Font.bold ] <| text "Other players"
                   ]
                ++ viewOthers
    in
    [ Theme.column [ padding 0, centerX ] children ]


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
    let
        fromGame gameName =
            case rawGameName gameName of
                "" ->
                    "Codebreaker!"

                rawName ->
                    "Codebreaker! - " ++ rawName
    in
    case inner of
        FrontendPlaying { gameName } ->
            fromGame gameName

        FrontendPreparing { gameName } ->
            fromGame gameName

        FrontendHomepage { gameName } ->
            fromGame gameName

        FrontendAdminAuthenticating _ ->
            ""

        FrontendAdminAuthenticated _ ->
            ""

        FrontendConnecting _ ->
            ""
