module Client exposing (main)

import Browser
import Html

main : Program () () msg
main = Browser.element
    { init = \() -> ((), Cmd.none)
    , update = \_ model -> (model, Cmd.none)
    , subscriptions = \_ -> Sub.none
    , view = \model -> Html.h1 [] [Html.text "Hi"]
    }

