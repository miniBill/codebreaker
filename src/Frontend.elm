module Frontend exposing (..)

import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav exposing (Key)
import Dict
import Element.WithContext as Element exposing (alignTop, centerX, el, fill, height, padding, paddingXY, px, spacing, text, width)
import Element.WithContext.Background as Background
import Element.WithContext.Border as Border
import Element.WithContext.Font as Font
import Element.WithContext.Input as Input exposing (username)
import Lamdera exposing (ClientId)
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

                FrontendHomepage homepage ->
                    viewHomepage homepage

                FrontendPreparing preparing ->
                    viewPreparing preparing

                FrontendPlaying playing ->
                    viewPlaying playing
    in
    Theme.column [ width fill, height fill ]
        (header :: errors :: body)


viewPlaying : PlayingFrontendModel -> List (Element FrontendMsg)
viewPlaying playingModel =
    let
        meView =
            case Dict.get playingModel.me playingModel.shared.players of
                Nothing ->
                    el [ alignTop ] <| text "Spectating"

                Just me ->
                    viewMePlaying playingModel.shared.codeLength maxHeight me

        othersViews =
            playingModel.shared.players
                |> Dict.toList
                |> List.Extra.filterNot (\( i, _ ) -> i /= playingModel.me)
                |> List.map Tuple.second
                |> List.map (viewOther playingModel.shared.codeLength maxHeight)

        maxHeight =
            playingModel.shared.players
                |> Dict.values
                |> List.map (.history >> List.length >> (+) 1)
                |> List.maximum
                |> Maybe.withDefault 1
                |> max 8
    in
    [ case playingModel.code of
        Nothing ->
            el [ centerX ] <| text "Spectating"

        Just code ->
            Theme.row [ padding 0, centerX ] [ text "Your code: ", viewCode [ Theme.borderWidth ] code ]
    , Theme.wrappedRow [ padding 0, centerX ] (meView :: othersViews)
    ]


viewMePlaying : Int -> Int -> { username : String, history : PlayerMoves, model : PlayerModel } -> Element msg
viewMePlaying codeLength maxHeight { username, history, model } =
    Theme.column [ Theme.borderWidth, Theme.borderRounded, height fill ]
        [ el [ Font.bold, centerX ] <| text "You"
        , viewHistory codeLength maxHeight history
        ]


viewOther : Int -> Int -> { username : String, history : PlayerMoves, model : PlayerModel } -> Element msg
viewOther codeLength maxHeight { username, history, model } =
    Theme.column [ Theme.borderWidth, Theme.borderRounded, height fill ]
        [ el [ Font.bold, centerX ] <| text username
        , viewHistory codeLength maxHeight history
        ]


viewHistory : Int -> Int -> PlayerMoves -> Element msg
viewHistory codeLength maxHeight moves =
    let
        paddedMoves =
            moves ++ List.repeat (maxHeight - List.length moves) ( [], { white = 0, black = 0 } )
    in
    paddedMoves
        |> List.reverse
        |> List.map (viewHistoryLine codeLength)
        |> Theme.column [ padding 0 ]


viewHistoryLine : Int -> ( Code, Answer ) -> Element msg
viewHistoryLine codeLength ( code, answer ) =
    Theme.row [ padding 0 ] [ viewCode [] (padCode codeLength code), viewAnswer codeLength answer ]


viewAnswer : Int -> Answer -> Element msg
viewAnswer codeLength { black, white } =
    List.repeat black (Element.rgb 0 0 0)
        ++ List.repeat white (Element.rgb 1 1 1)
        ++ List.repeat (codeLength - black - white) (Element.rgb 0.7 0.7 0.7)
        |> List.map
            (\color ->
                el
                    [ width <| px 5
                    , height <| px 5
                    , Border.rounded 10
                    , Border.width 1
                    , Border.color <| Element.rgb 0 0 0
                    , Background.color color
                    ]
                    Element.none
            )
        |> List.Extra.greedyGroupsOf (codeLength // 2)
        |> List.map (Element.row [ spacing 1 ])
        |> Element.column [ spacing 1 ]


viewPreparing : PreparingFrontendModel -> List (Element FrontendMsg)
viewPreparing ({ me } as preparingModel) =
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
                Theme.button [ centerX ] { onPress = Ready, label = text "Ready" }

             else
                Element.none
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