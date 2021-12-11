module Frontend.Admin exposing (authenticated, authenticating, update)

import Element.WithContext exposing (fill, text, width)
import Element.WithContext.Input as Input
import Theme exposing (Element)
import Types exposing (AdminModel, AdminMsg(..), FrontendMsg(..), InnerFrontendModel(..))


authenticated : a -> List (Element msg)
authenticated _ =
    [ text "authenticated" ]


authenticating : String -> List (Element FrontendMsg)
authenticating password =
    [ Input.text [ width fill ]
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


update : AdminMsg -> AdminModel -> ( AdminModel, Cmd msg )
update msg model =
    case msg of
        AdminMsgNop ->
            ( model, Cmd.none )
