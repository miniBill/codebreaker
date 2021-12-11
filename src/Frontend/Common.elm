module Frontend.Common exposing (codeInput, gameNameToUrl, padCode, viewCode, viewColor)

import Element.WithContext as Element exposing (centerX, centerY, el, height, padding, paddingXY, px, text, width)
import Element.WithContext.Background as Background
import Element.WithContext.Border as Border
import Element.WithContext.Input as Input
import List.Extra
import Theme exposing (Attribute, Element)
import Types exposing (..)
import Url.Builder


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


gameNameToUrl : GameName -> String
gameNameToUrl gameName =
    Url.Builder.absolute
        [ String.replace " " "-" (normalizeGameName gameName) ]
        []
