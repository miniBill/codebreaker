module Frontend.Preparing exposing (view)

import Dict
import Element.WithContext as Element exposing (Color, centerX, el, padding, text)
import Element.WithContext.Border as Border
import Element.WithContext.Font as Font
import Element.WithContext.Input as Input
import Frontend.Common exposing (codeInput, gameNameToUrl, viewCode, viewColor)
import Html.Attributes
import List.Extra
import Theme exposing (Element)
import Types exposing (FrontendMsg(..), FrontendPreparingModel, SharedPreparingModel, sharedPreparingParse)


view : { a | rootUrl : String, colorblindMode : Bool } -> FrontendPreparingModel -> List (Element FrontendMsg)
view ({ rootUrl } as config) ({ shared } as preparingModel) =
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
            , viewColorCount config shared
            ]

        sharedParsed =
            sharedPreparingParse shared

        children =
            sharedInput
                ++ (if me.ready then
                        [ Element.text "Wait for other players"
                        , viewCode [ Theme.borderWidth, centerX ] me.code
                        ]

                    else
                        [ text <| "Set your secret code, " ++ me.username
                        , Element.map SetCode <|
                            codeInput (sharedPreparingParse shared) me.code
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
                ++ viewOthers preparingModel
    in
    [ Theme.column [ padding 0, centerX ] children ]


viewColorCount : { a | colorblindMode : Bool } -> SharedPreparingModel -> Element FrontendMsg
viewColorCount config shared =
    let
        rowCount =
            4

        rowSize =
            (List.length Theme.colors + (rowCount - 1)) // rowCount

        viewRow rowIndex colors =
            colors
                |> List.indexedMap (viewCell config shared rowSize rowIndex)
                |> Element.row []
    in
    Theme.colors
        |> List.Extra.greedyGroupsOf rowSize
        |> List.indexedMap viewRow
        |> Element.column [ centerX ]


viewCell :
    { a | colorblindMode : Bool }
    -> SharedPreparingModel
    -> Int
    -> Int
    -> Int
    -> { backgroundColor : Color, symbol : String }
    -> Element FrontendMsg
viewCell { colorblindMode } shared rowSize rowIndex columnIndex color =
    let
        index =
            rowIndex * rowSize + columnIndex

        active =
            index < shared.colors

        borders =
            { top = rowIndex == 0
            , left = columnIndex == 0
            , right = columnIndex == rowSize - 1 || index == shared.colors - 1
            , bottom = index + rowSize >= shared.colors
            }

        borderIfActiveAnd colored direction =
            Element.htmlAttribute <|
                Html.Attributes.style ("border-" ++ direction ++ "-color")
                    (if colored && active then
                        "black"

                     else
                        "transparent"
                    )

        borderWidths =
            Border.widthEach
                { left = boolToInt borders.left
                , top = boolToInt borders.top
                , right = 1
                , bottom = 1
                }

        label =
            { color
                | backgroundColor =
                    if active then
                        color.backgroundColor

                    else
                        Theme.desaturate color.backgroundColor
            }
                |> viewColor
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
        , label = label
        }
        |> el
            [ padding <| Theme.rythm // 2
            , borderWidths
            , borderIfActiveAnd borders.top "top"
            , borderIfActiveAnd borders.left "left"
            , borderIfActiveAnd borders.right "right"
            , borderIfActiveAnd borders.bottom "bottom"
            ]


boolToInt : Bool -> Int
boolToInt b =
    if b then
        1

    else
        0


viewOthers : FrontendPreparingModel -> List (Element msg)
viewOthers preparingModel =
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
