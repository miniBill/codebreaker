module Frontend.Admin exposing (authenticated, authenticating, update)

import DateFormat
import Element.WithContext as Element exposing (Length, centerY, el, fill, padding, shrink, table, text, width)
import Element.WithContext.Border as Border
import Element.WithContext.Extra exposing (onEnter)
import Element.WithContext.Font as Font
import Element.WithContext.Input as Input
import Lamdera
import Theme exposing (Attribute, Element)
import Time
import Types exposing (AdminMsg(..), FrontendAdminModel, FrontendMsg(..), FrontendPlayingModel, FrontendPreparingModel, PlayerModel(..), ToBackend(..))
import Types.GameName as GameName exposing (GameName)
import Types.Id as Id


authenticated : FrontendAdminModel -> List (Element AdminMsg)
authenticated { preparing, playing } =
    [ section "Preparing"
    , viewPreparing preparing
    , section "Playing"
    , viewPlaying playing
    ]


viewPreparing : List ( Time.Posix, FrontendPreparingModel ) -> Element AdminMsg
viewPreparing preparing =
    table [ width fill ]
        { data = preparing
        , columns =
            [ lastUpdateColumn
            , gameNameColumn
            , colorsColumn
            , lengthColumnString
            , playersColumnPreparing
            , deleteColumn
            ]
        }


viewPlaying : List ( Time.Posix, FrontendPlayingModel ) -> Element AdminMsg
viewPlaying playing =
    table [ width fill ]
        { data = playing
        , columns =
            [ lastUpdateColumn
            , gameNameColumn
            , colorsColumn
            , lengthColumnInt
            , playersColumnPlaying
            , deleteColumn
            ]
        }


type alias Column record msg =
    { header : String
    , view : ( Time.Posix, record ) -> Element msg
    , width : Length
    }


lastUpdateColumn : Column a msg
lastUpdateColumn =
    let
        tokens =
            [ DateFormat.hourMilitaryFixed
            , DateFormat.text ":"
            , DateFormat.minuteFixed
            , DateFormat.text ":"
            , DateFormat.secondFixed
            , DateFormat.text " d"
            , DateFormat.dayOfYearFixed
            ]
    in
    { header = "Last update (UTC)"
    , view =
        \( lastUpdate, _ ) ->
            lastUpdate
                |> DateFormat.format tokens Time.utc
                |> text
    , width = shrink
    }


gameNameColumn : Column { a | gameName : GameName } msg
gameNameColumn =
    simpleColumn
        { header = "Game name"
        , view = \{ gameName } -> GameName.toString gameName
        }


colorsColumn : Column { a | shared : { b | colors : Int } } msg
colorsColumn =
    simpleColumn
        { header = "Colors"
        , view = \{ shared } -> String.fromInt shared.colors
        }


lengthColumnInt : Column { a | shared : { b | codeLength : Int } } msg
lengthColumnInt =
    simpleColumn
        { header = "Length"
        , view = \{ shared } -> String.fromInt shared.codeLength
        }


lengthColumnString : Column { a | shared : { b | codeLength : String } } msg
lengthColumnString =
    simpleColumn
        { header = "Length"
        , view = \{ shared } -> shared.codeLength
        }


playersColumnPreparing : Column FrontendPreparingModel AdminMsg
playersColumnPreparing =
    { header = "Players"
    , view =
        \( _, { players } ) ->
            players
                |> Id.dict.values
                |> List.map
                    (\{ username, ready } ->
                        if ready then
                            username ++ " ✔"

                        else
                            username ++ " ✘"
                    )
                |> String.join ", "
                |> text
    , width = fill
    }


playersColumnPlaying : Column FrontendPlayingModel AdminMsg
playersColumnPlaying =
    { header = "Players"
    , view =
        \( _, { shared } ) ->
            shared.players
                |> Id.dict.values
                |> List.map
                    (\{ username, model } ->
                        case model of
                            Won _ ->
                                username ++ " (W)"

                            Guessing _ ->
                                username ++ " (G)"
                    )
                |> String.join ", "
                |> text
    , width = fill
    }


deleteColumn : Column { a | gameName : GameName } AdminMsg
deleteColumn =
    { header = "Commands"
    , view =
        \( _, { gameName } ) ->
            Theme.button []
                { onPress = AdminDelete gameName
                , label = text "Delete"
                }
    , width = shrink
    }


simpleColumn : { header : String, view : record -> String } -> Column record msg
simpleColumn col =
    { header = col.header
    , view = \( _, model ) -> text <| col.view model
    , width = shrink
    }


table :
    List (Attribute msg)
    ->
        { data : List ( Time.Posix, record )
        , columns :
            List
                { header : String
                , view : ( Time.Posix, record ) -> Element msg
                , width : Length
                }
        }
    -> Element msg
table attrs { data, columns } =
    let
        mapColumn { header, view, width } =
            { header =
                el
                    [ Border.widthEach
                        { bottom = 1
                        , left = 0
                        , right = 0
                        , top = 0
                        }
                    , padding <| Theme.rythm // 2
                    ]
                    (text header)
            , view = el [ padding <| Theme.rythm // 2, centerY ] << view
            , width = width
            }
    in
    Element.table attrs
        { data = List.sortBy (Tuple.first >> Time.posixToMillis) data
        , columns = List.map mapColumn columns
        }


section : String -> Element msg
section label =
    el
        [ Theme.fontSizes.big
        , Font.bold
        ]
        (text label)


authenticating : String -> List (Element FrontendMsg)
authenticating password =
    [ Input.text
        [ width fill
        , onEnter AdminLogin
        ]
        { onChange = AdminPassword
        , text = password
        , placeholder = Nothing
        , label = Input.labelAbove [] <| text "Password"
        }
    , Theme.button []
        { onPress = AdminLogin
        , label = text "Login"
        }
    ]


update : AdminMsg -> FrontendAdminModel -> ( FrontendAdminModel, Cmd msg )
update msg model =
    ( model, Lamdera.sendToBackend <| TBAdmin msg )
