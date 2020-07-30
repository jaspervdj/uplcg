port module Client exposing (main)

import Browser
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Json.Decode
import Json.Encode
import Messages exposing (GameView)
import Url exposing (Url)

port webSocketIn : (String -> msg) -> Sub msg
port webSocketOut : String -> Cmd msg

type Msg
    = Ignore
    | Send
    | WebSocketIn String
    -- Name changes
    | StartChangingName
    | ChangeName String
    | SubmitNewName

type Model
    = Error String
    | Connecting
        { roomId : String
        }
    | Game
        { view : GameView
        , changingName : Maybe String
        }

parseRoomId : Url -> Result String String
parseRoomId url = case String.split "/" url.path of
    _ :: "rooms" :: roomId :: _ -> Ok roomId
    _ -> Err <| "Invalid path: " ++ url.path

view : Model -> List (Html Msg)
view model = case model of
    Error str ->
        [ Html.h1 [] [Html.text "Error"]
        , Html.p [] [Html.text str]
        ]
    Connecting state ->
        [ Html.h1 []
            [Html.text <| "Connecting to room " ++ state.roomId ++ "..."]
        ]
    Game game ->
        [ Html.h1 [] [Html.text "Opponents"]
        , Html.ul [] <| List.map
            (\p -> Html.li [] [Html.text p])
            game.view.opponents
        , Html.h1 [] [Html.text "You"]
        ] ++
        (case game.changingName of
            Nothing ->
                [ Html.p []
                    [Html.text game.view.playerName]
                , Html.button
                    [Html.Events.onClick StartChangingName]
                    [Html.text "change"]
                ]
            Just name ->
                [ Html.input
                    [ Html.Attributes.value name
                    , Html.Events.onInput ChangeName
                    ]
                    []
                , Html.button
                    [Html.Events.onClick SubmitNewName]
                    [Html.text "change"]
                ])


subscriptions : Model -> Sub Msg
subscriptions model = webSocketIn WebSocketIn

send : Messages.ClientMessage -> Cmd Msg
send = webSocketOut << Json.Encode.encode 0 << Messages.jsonEncClientMessage

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = case msg of
    Ignore -> (model, Cmd.none)
    Send -> (model, webSocketOut "Hi")
    WebSocketIn json ->
        case Json.Decode.decodeString Messages.jsonDecServerMessage json of
            Err str -> (Error <| Json.Decode.errorToString str, Cmd.none)
            Ok (Messages.Welcome playerId) ->
                Debug.log ("Welcome " ++ String.fromInt playerId) (model, Cmd.none)
            Ok Messages.Bye -> Debug.log "Bye" (model, Cmd.none)
            Ok (Messages.SyncGameView gameView) ->
                case model of
                    Game game -> (Game {game | view = gameView}, Cmd.none)
                    _ ->
                        ( Game
                            { view = gameView
                            , changingName = Nothing
                            }
                        , Cmd.none
                        )

    StartChangingName -> case model of
        Game game ->
            (Game {game | changingName = Just game.view.playerName}, Cmd.none)
        _ -> (model, Cmd.none)
    ChangeName name -> case model of
        Game game -> (Game {game | changingName = Just name}, Cmd.none)
        _ -> (model, Cmd.none)
    SubmitNewName -> case model of
        Game game ->
            ( Game {game | changingName = Nothing}
            , case game.changingName of
                Nothing   -> Cmd.none
                Just name -> send <| Messages.ChangeName name
            )
        _ -> (model, Cmd.none)

main : Program () Model Msg
main = Browser.application
    { init = \() url key -> case parseRoomId url of
        Err str -> (Error <| "Could not parse room ID: " ++ str, Cmd.none)
        Ok roomId -> (Connecting {roomId = roomId}, Cmd.none)
    , update = update
    , subscriptions = subscriptions
    , view = \model -> {title = "Client", body = view model}
    , onUrlChange = \url -> Ignore
    , onUrlRequest = \urlRequest -> Ignore
    }

