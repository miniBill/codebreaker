module Theme exposing (Attr, Attribute, Element, borderRounded, borderWidth, button, colors, column, fontSizes, padding, row, rythm, spacing, wrappedRow)

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


colors : List Color
colors =
    [ Element.rgb 1 0 0
    , Element.rgb 0 1 0
    , Element.rgb 0 0 1
    , Element.rgb 1 1 0
    , Element.rgb 1 0 1
    , Element.rgb 0 0 0
    , Element.rgb 1 1 1
    , Element.rgb255 255 69 0
    ]
