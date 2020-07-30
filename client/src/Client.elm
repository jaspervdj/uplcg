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
    | ChangeMyName String
    | SubmitMyName

type Model
    = Error String
    | Connecting
        { roomId : String
        }
    | Game
        { view : GameView
        , changeMyName : String
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
        , Html.form
            [ Html.Attributes.action ""
            , Html.Events.onSubmit SubmitMyName
            ]
            [ Html.input
                [ Html.Attributes.value game.changeMyName
                , Html.Events.onInput ChangeMyName
                ]
                []
            , Html.button
                [ Html.Attributes.type_ "submit"
                , Html.Attributes.disabled <|
                    game.view.myName == game.changeMyName
                ]
                [Html.text "change"]
            ]
        ] ++
        (case game.view.blackCard of
            Nothing -> []
            Just c -> [blackCard c]) ++
        (List.map whiteCard game.view.hand)

blackCard : Messages.BlackCard -> Html a
blackCard (Messages.BlackCard string) =
    let blank = Html.span [Html.Attributes.class "blank"] [] in
    Html.div [Html.Attributes.class "card", Html.Attributes.class "black"] <|
    List.intersperse blank <|
    List.map Html.text <|
    String.split "\\BLANK" string

whiteCard : Messages.WhiteCard -> Html a
whiteCard (Messages.WhiteCard string) = Html.div
    [Html.Attributes.class "card", Html.Attributes.class "white"]
    [Html.text string]

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
                            , changeMyName = gameView.myName
                            }
                        , Cmd.none
                        )

    ChangeMyName name -> case model of
        Game game -> (Game {game | changeMyName = name}, Cmd.none)
        _ -> (model, Cmd.none)
    SubmitMyName -> case model of
        Game game -> (model , send <| Messages.ChangeMyName game.changeMyName)
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

