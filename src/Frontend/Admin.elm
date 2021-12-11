module Frontend.Admin exposing (authenticated, authenticating)

import Element.WithContext exposing (text)
import Theme exposing (Element)


authenticated : a -> List (Element msg)
authenticated _ =
    [ text "authenticated" ]


authenticating : a -> List (Element msg)
authenticating _ =
    [ text "authenticating" ]
