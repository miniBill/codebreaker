module Frontend.Game exposing (viewPlaying)

import Dict
import Element.WithContext as Element exposing (alignBottom, alignRight, alignTop, centerX, el, fill, height, padding, paragraph, px, spacing, text, width)
import Element.WithContext.Background as Background
import Element.WithContext.Border as Border
import Element.WithContext.Font as Font
import Frontend.Common exposing (codeInput, padCode, viewCode)
import List.Extra
import Theme exposing (Element)
import Time
import Types exposing (..)


viewPlaying : PlayingFrontendModel -> List (Element FrontendMsg)
viewPlaying playingModel =
    let
        meViews =
            case Dict.get playingModel.me playingModel.shared.players of
                Just me ->
                    viewMePlaying playingModel.shared me

                Nothing ->
                    []

        othersViews =
            playingModel.shared.players
                |> Dict.toList
                |> List.filter (\( i, _ ) -> i /= playingModel.me)
                |> List.map Tuple.second
                |> List.map (viewOther playingModel.shared)
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


viewMePlaying : PlayingSharedModel -> PlayingPlayerModel -> List (Element FrontendMsg)
viewMePlaying ({ startTime, codeLength } as shared) player =
    let
        sharedView =
            viewPlayingPlayer shared
                { player | username = "You" }
                (case player.model of
                    Won { winTime } ->
                        [ paragraph []
                            [ text <| "You guessed the code in " ++ formatDelta startTime winTime ]
                        ]

                    Guessing { current } ->
                        [ el [ centerX ] <| text "Still guessing..."
                        , viewCode [ centerX, padding 0 ] (padCode codeLength current)
                        ]
                )
    in
    case player.model of
        Won _ ->
            [ sharedView ]

        Guessing { current } ->
            let
                acceptableCode =
                    List.all ((/=) -1) current
                        && (List.length current == codeLength)
            in
            [ Theme.row [ padding 0, alignTop ]
                [ sharedView
                , Theme.column
                    [ Theme.borderRounded
                    , Theme.borderWidth
                    , alignBottom
                    ]
                    [ el [ centerX, Font.bold ] <| text "Your next guess:"
                    , Element.map SetCode <| codeInput shared current
                    , Theme.button
                        [ Element.transparent <| not acceptableCode
                        , centerX
                        ]
                        { onPress = Submit
                        , label = text "Submit"
                        }
                    ]
                ]
            ]


viewOther : PlayingSharedModel -> PlayingPlayerModel -> Element msg
viewOther ({ startTime, codeLength } as shared) player =
    viewPlayingPlayer shared
        player
        (case player.model of
            Won { winTime } ->
                [ paragraph []
                    [ text <| "They guessed the code in " ++ formatDelta startTime winTime ]
                ]

            Guessing { current } ->
                [ el [ centerX ] <| text "Is guessing"
                , viewCode [ centerX, padding 0 ] (padCode codeLength current)
                ]
        )


viewPlayingPlayer : PlayingSharedModel -> PlayingPlayerModel -> List (Element msg) -> Element msg
viewPlayingPlayer shared { username, history, model, opponentId } rest =
    let
        opponent =
            Dict.get opponentId shared.players
                |> Maybe.map .username
                |> Maybe.withDefault ""
    in
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
         , if Dict.size shared.players == 2 then
            Element.none

           else
            el [ centerX ] <| text <| "Guessing " ++ opponent
         , viewHistory shared history
         ]
            ++ rest
        )


formatDelta : Time.Posix -> Time.Posix -> String
formatDelta from to =
    let
        diff =
            (Time.posixToMillis to - Time.posixToMillis from) // 1000

        pad n i =
            String.fromInt i |> String.padLeft n '0'
    in
    pad 0 (diff // 60) ++ "m " ++ pad 2 (modBy 60 diff) ++ "s"


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
