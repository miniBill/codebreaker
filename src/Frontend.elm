module Frontend exposing (app)

import Browser exposing (UrlRequest(..))
import Browser.Dom
import Browser.Navigation as Nav exposing (Key)
import Element.WithContext as Element exposing (centerX, centerY, el, fill, height, padding, text, width)
import Element.WithContext.Background as Background
import Element.WithContext.Extra as Extra
import Element.WithContext.Font as Font
import Element.WithContext.Input as Input
import Frontend.Admin as Admin
import Frontend.Common exposing (gameNameToUrl)
import Frontend.Playing as Playing
import Frontend.Preparing as Preparing
import Html.Attributes
import Lamdera
import Task
import Theme exposing (Attribute, Element)
import Types exposing (..)
import Url exposing (Url)
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

        AdminPassword password ->
            case model.inner of
                FrontendAdminAuthenticating _ ->
                    ( { model | inner = FrontendAdminAuthenticating password }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        AdminLogin ->
            case model.inner of
                FrontendAdminAuthenticating password ->
                    ( model, Lamdera.sendToBackend <| TBAdminAuthenticate password )

                _ ->
                    ( model, Cmd.none )

        AdminMsg adminMsg ->
            case model.inner of
                FrontendAdminAuthenticated inner ->
                    let
                        ( inner_, cmd ) =
                            Admin.update adminMsg inner
                    in
                    ( { model | inner = FrontendAdminAuthenticated inner_ }, cmd )

                _ ->
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

                FrontendAdminAuthenticating authenticating ->
                    Admin.authenticating authenticating

                FrontendAdminAuthenticated authenticated ->
                    Admin.authenticated authenticated

                FrontendHomepage homepage ->
                    viewHomepage model.error homepage ++ accessibility

                FrontendPreparing preparing ->
                    Preparing.view model preparing ++ accessibility

                FrontendPlaying playing ->
                    Playing.view playing ++ accessibility

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
