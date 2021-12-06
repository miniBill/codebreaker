module Theme exposing (Attr, Attribute, Element, borderRounded, borderWidth, button, colors, column, fontSizes, padding, row, rythm, spacing, wrappedRow)

import Color
import Element.WithContext as Element exposing (Color)
import Element.WithContext.Background as Background
import Element.WithContext.Border as Border
import Element.WithContext.Font as Font
import Element.WithContext.Input as Input
import Types exposing (Context)


type alias Element msg =
    Element.Element Context msg


type alias Attribute msg =
    Element.Attribute Context msg


type alias Attr decorative msg =
    Element.Attr Context decorative msg


rythm : number
rythm =
    10


padding : Attribute msg
padding =
    Element.padding rythm


spacing : Attribute msg
spacing =
    Element.spacing rythm


row : List (Attribute msg) -> List (Element msg) -> Element msg
row attrs =
    Element.row ([ padding, spacing ] ++ attrs)


column : List (Attribute msg) -> List (Element msg) -> Element msg
column attrs =
    Element.column ([ padding, spacing ] ++ attrs)


wrappedRow : List (Attribute msg) -> List (Element msg) -> Element msg
wrappedRow attrs =
    Element.wrappedRow ([ padding, spacing ] ++ attrs)


borderWidth : Attribute msg
borderWidth =
    Border.width 1


borderRounded : Attribute msg
borderRounded =
    Border.rounded rythm


button : List (Attribute msg) -> { onPress : msg, label : Element msg } -> Element msg
button attrs { onPress, label } =
    Input.button
        ([ Font.center
         , borderRounded
         , borderWidth
         , padding
         , Background.color <| Element.rgb 0.95 0.95 1
         ]
            ++ attrs
        )
        { onPress = Just onPress
        , label = label
        }


fontSizes : { big : Attribute msg, normal : Attribute msg }
fontSizes =
    let
        scale k =
            Font.size <| round <| Element.modular 16 1.25 k
    in
    { big = scale 2
    , normal = scale 1
    }


colors : List { backgroundColor : Color, symbol : String }
colors =
    [ ( Color.rgb255 0xE6 0x19 0x4B, "🔥" )
    , ( Color.rgb255 0x3C 0xB4 0x4B, "🌲" )
    , ( Color.rgb255 0xFF 0xE1 0x19, "🌞" )
    , ( Color.rgb255 0x43 0x63 0xD8, "🌊" )
    , ( Color.rgb255 0xF5 0x82 0x31, "🍊" )
    , ( Color.rgb255 0x91 0x1E 0xB4, "🍇" )
    , ( Color.rgb255 0x42 0xD4 0xF4, "🌀" )
    , ( Color.rgb255 0xF0 0x32 0xE6, "💖" )
    , ( Color.rgb255 0xBF 0xEF 0x45, "🍐" )
    , ( Color.rgb255 0xFA 0xBE 0xD4, "🌸" )
    , ( Color.rgb255 0x46 0x99 0x90, "🐢" )
    , ( Color.rgb255 0xDC 0xBE 0xFF, "👚" )
    , ( Color.rgb255 0x9A 0x63 0x24, "🐒" )
    , ( Color.rgb255 0xFF 0xFA 0xC8, "🏐" )
    , ( Color.rgb255 0x80 0x00 0x00, "🐻" )
    , ( Color.rgb255 0xAA 0xFF 0xC3, "🌱" )
    , ( Color.rgb255 0x80 0x80 0x00, "📦" )
    , ( Color.rgb255 0xFF 0xD8 0xB1, "🐖" )
    , ( Color.rgb255 0x00 0x00 0x75, "👤" )
    , ( Color.rgb255 0xFF 0xFF 0xFF, "💭" )
    , ( Color.rgb255 0x00 0x00 0x00, "🎱" )
    ]
        |> List.map
            (\( color, symbol ) ->
                { backgroundColor = toElmUi color
                , symbol = symbol
                }
            )


toElmUi : Color.Color -> Color
toElmUi =
    Color.toRgba >> Element.fromRgb
