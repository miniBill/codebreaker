module Frontend exposing (..)

import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav exposing (Key)
import Element.WithContext as Element exposing (centerX, el, fill, height, padding, paddingXY, px, text, width)
import Element.WithContext.Background as Background
import Element.WithContext.Border as Border
import Element.WithContext.Font as Font
import Element.WithContext.Input as Input
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
      , context = {}
      , inner = FrontendConnecting
      , error = ""
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

        Ready ->
            ( model, Lamdera.sendToBackend TBSubmit )


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
            model.context
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
            el
                [ width fill
                , Font.bold
                , Font.center
                , Theme.fontSizes.big
                ]
                (text <| title model)

        errors =
            if String.isEmpty model.error then
                Element.none

            else
                el [ Theme.padding, Background.color <| Element.rgb 1 0.8 0.8, width fill ]
                    (text model.error)

        body =
            case model.inner of
                FrontendConnecting ->
                    [ text "Connecting to server" ]

                FrontendHomepage homepageModel ->
                    viewHomepage homepageModel

                FrontendPreparing preparingModel ->
                    viewPreparing preparingModel

                FrontendPlaying _ ->
                    [ text "TODO: branch 'PlayingGame _' not implemented" ]
    in
    Theme.column [ width fill, height fill ]
        (header :: errors :: body)


viewPreparing : PreparingFrontendModel -> List (Element FrontendMsg)
viewPreparing preparingModel =
    (if preparingModel.ready then
        [ el [ centerX ] <| Element.text "Wait for other players"
        , viewCode [ Theme.borderWidth, centerX ] preparingModel.code
        ]

     else
        [ el [ centerX ] <| text "Set your secret code"
        , Element.map SetCode <| codeInput preparingModel.shared preparingModel.code
        ]
    )
        ++ [ if
                List.all ((/=) -1) preparingModel.code
                    && (List.length preparingModel.code == preparingModel.shared.codeLength)
                    && not preparingModel.ready
             then
                Theme.button [ centerX ] { onPress = Ready, label = text "Ready" }

             else
                Element.none
           ]


codeInput : { a | codeLength : Int, colors : Int } -> Code -> Element Code
codeInput { codeLength, colors } code =
    let
        paddedCode =
            code ++ List.repeat (codeLength - List.length code) -1

        digitInput index =
            Theme.colors
                |> List.take colors
                |> List.indexedMap
                    (\colorIndex c ->
                        Input.button
                            [ width <| px 20
                            , height <| px 20
                            , Background.color c
                            , Border.rounded 20
                            , Border.width 1
                            , Border.color <| Element.rgb 0 0 0
                            ]
                            { onPress = Just <| List.Extra.setAt index colorIndex paddedCode
                            , label = Element.none
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
        [ current
        , inputs
        ]


viewCode : List (Attribute msg) -> List Int -> Element msg
viewCode attrs =
    Theme.row attrs
        << List.map
            (\digit ->
                let
                    color =
                        List.Extra.getAt digit Theme.colors
                            |> Maybe.withDefault (Element.rgb 0.7 0.7 0.7)
                in
                el
                    [ width <| px 20
                    , height <| px 20
                    , Background.color color
                    , Border.rounded 20
                    , Theme.borderWidth
                    , Border.color <| Element.rgb 0 0 0
                    ]
                    Element.none
            )


viewHomepage : HomepageModel -> List (Element FrontendMsg)
viewHomepage homepageModel =
    let
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
            , text = homepageModel.gameName
            , placeholder = "Game name"
            , onChange = \newGameName -> HomepageMsg { homepageModel | gameName = newGameName }
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
            "Codebreaker! - Playing game " ++ gameName

        _ ->
            "Codebreaker!"
