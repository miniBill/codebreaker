module Frontend.Preparing exposing (view)

import Browser exposing (UrlRequest(..))
import Dict
import Element.WithContext as Element exposing (centerX, el, padding, text)
import Element.WithContext.Border as Border
import Element.WithContext.Font as Font
import Element.WithContext.Input as Input
import Frontend.Common exposing (codeInput, gameNameToUrl, viewCode, viewColor)
import Html.Attributes
import List.Extra
import Theme exposing (Element)
import Types exposing (..)


view : { a | rootUrl : String, colorblindMode : Bool } -> PreparingFrontendModel -> List (Element FrontendMsg)
view { rootUrl, colorblindMode } ({ shared } as preparingModel) =
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
