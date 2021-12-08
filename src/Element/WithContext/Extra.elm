module Element.WithContext.Extra exposing (onEnter)

import Element.WithContext as Element exposing (Attribute)
import Html.Events
import Json.Decode


onEnter : msg -> Attribute context msg
onEnter msg =
    Element.htmlAttribute
        (Html.Events.on "keyup"
            (Json.Decode.field "key" Json.Decode.string
                |> Json.Decode.andThen
                    (\key ->
                        if key == "Enter" then
                            Json.Decode.succeed msg

                        else
                            Json.Decode.fail "Not the enter key"
                    )
            )
        )
