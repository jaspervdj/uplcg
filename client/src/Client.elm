module Client exposing (main)

import Browser
import Html exposing (Html)
import Url exposing (Url)

type Message
    = Ignore

type Model
    = Error String
    | JoinRoom
        { id : String
        }

parseRoomId : Url -> Result String String
parseRoomId url = case String.split "/" url.path of
    _ :: "rooms" :: roomId :: _ -> Ok roomId
    _ -> Err <| "Invalid path: " ++ url.path

view : Model -> List (Html Message)
view model = case model of
    Error str ->
        [ Html.h1 [] [Html.text "Error"]
        , Html.p [] [Html.text str]
        ]
    JoinRoom room ->
        [ Html.h1 [] [Html.text <| "Room " ++ room.id]
        ]

main : Program () Model Message
main = Browser.application
    { init = \() url key -> case parseRoomId url of
        Err str -> (Error <| "Could not parse room ID: " ++ str, Cmd.none)
        Ok roomId -> (JoinRoom {id = roomId}, Cmd.none)
    , update = \_ model -> (model, Cmd.none)
    , subscriptions = \_ -> Sub.none
    , view = \model -> {title = "Client", body = view model}
    , onUrlChange = \url -> Ignore
    , onUrlRequest = \urlRequest -> Ignore
    }

