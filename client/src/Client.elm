module Client exposing (main)

import Browser
import Html

type Message
    = Ignore

main : Program () () Message
main = Browser.application
    { init = \() url key -> ((), Cmd.none)
    , update = \_ model -> (model, Cmd.none)
    , subscriptions = \_ -> Sub.none
    , view = \model ->
        { title = "Client"
        , body = [Html.h1 [] [Html.text "Hi"]]
        }
    , onUrlChange = \url -> Ignore
    , onUrlRequest = \urlRequest -> Ignore
    }

