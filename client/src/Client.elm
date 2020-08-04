port module Client exposing (main)

import Array exposing (Array)
import Browser
import Html.Attributes
import Html.Events
import Html exposing (Html)
import Json.Decode
import Json.Encode
import Messages exposing (BlackCard, WhiteCard, GameView)
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
    -- Card selection
    | SelectWhiteCard WhiteCard
    | ProposeWhiteCards
    -- Voting
    | SelectVote Int
    | SubmitVote
    -- Tally
    | ConfirmTally

type alias Cards = {black : Array String, white : Array String}

type alias GameState =
    { cards : Cards
    , view : GameView
    , changeMyName : String
    , selectedWhiteCards : List WhiteCard
    , selectedVote : Maybe Int
    }

type Model
    = Error String
    | Connecting
    | Game GameState

viewPlayer : Messages.PlayerView -> Html msg
viewPlayer player = Html.div [] <|
    [ Html.text player.name
    ] ++
    (if player.admin then [Html.text " 👑"] else []) ++
    (if player.ready then [Html.text " ✅"] else []) ++
    [Html.text <| " (" ++ String.fromInt player.points ++ " points)"]

view : Model -> List (Html Msg)
view model = case model of
    Error str ->
        [ Html.h1 [] [Html.text "Error"]
        , Html.p [] [Html.text str]
        ]
    Connecting -> [Html.h1 [] [Html.text "Connecting to room..."]]
    Game game ->
        [ Html.h1 [] [Html.text "Players"]
        , Html.ul [] <| List.map
            (\o -> Html.li [] [viewPlayer o])
            (game.view.me :: game.view.players)
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
                    game.view.me.name == game.changeMyName ||
                    String.length game.changeMyName > 32
                ]
                [Html.text "Update name"]
            ]
        , Html.h1 [] [Html.text "Table"]
        , viewTable game
        , Html.h1 [] [Html.text "Your cards"]
        ] ++
        (List.map
            (\c -> whiteCard game.cards c (cardIsSelected game c))
            game.view.hand)

tableBlackCard : GameState -> Maybe BlackCard
tableBlackCard game = case game.view.table of
    Messages.Proposing b _ -> Just b
    Messages.Voting b _ _ _ -> Just b
    Messages.Tally b _ -> Just b

selectedWhiteCards : GameState -> List WhiteCard
selectedWhiteCards game = case game.view.table of
    Messages.Proposing _ (x :: xs) -> x :: xs
    _ -> game.selectedWhiteCards

cardIsSelected : GameState -> WhiteCard -> Bool
cardIsSelected game card = List.member card <| selectedWhiteCards game

viewTable : GameState -> Html Msg
viewTable game = case game.view.table of
    Messages.Proposing c my -> Html.div [] <|
        [ blackCard [] game.cards c <| selectedWhiteCards game
        , Html.button
            [ Html.Attributes.disabled <|
                List.length my > 0 ||
                List.length (selectedWhiteCards game) /=
                    blackCardBlanks game.cards c
            , Html.Events.onClick ProposeWhiteCards
            ]
            [Html.text "Propose"]
        ]
    Messages.Voting black proposals myProposal myVote -> Html.div [] <|
        [ Html.h2 [] [Html.text "Opponent proposals"]
        ] ++
        List.indexedMap (\i proposal ->
            let attrs =
                    if i == myProposal then
                        [Html.Attributes.class "mine"]
                    else if Just i == myVote || Just i == game.selectedVote then
                        [Html.Attributes.class "voted"]
                    else
                        [ Html.Events.onClick <| SelectVote i
                        , Html.Attributes.class "votable"
                        ] in
            blackCard attrs game.cards black proposal) proposals ++
        [ Html.button
            [ Html.Attributes.disabled <|
                (case myVote of
                     Just _ -> True
                     Nothing -> case game.selectedVote of
                         Just _ -> False
                         Nothing -> True)
            , Html.Events.onClick SubmitVote
            ]
            [Html.text "Vote"]
        ]

    Messages.Tally black results -> Html.div [] <|
        [Html.h2 [] [Html.text "Vote results"]] ++
        List.map (\voted ->
            let attrs =
                    if List.length voted.winners > 0 then
                        [Html.Attributes.class "winner"]
                    else
                        [] in
            blackCard attrs game.cards black voted.proposal)
            results ++
        if not game.view.me.admin then
            []
        else
            [ Html.button
                [Html.Events.onClick ConfirmTally] [Html.text "Next round"]
            ]

intersperseWith : List a -> a -> List a -> List a
intersperseWith values def list = case list of
    [] -> []
    x :: [] -> x :: []
    x :: y :: t -> case values of
        [] -> x :: def :: intersperseWith values def (y :: t)
        v :: vs -> x :: v :: intersperseWith vs def (y :: t)

blackCardContent : Cards -> BlackCard -> List String
blackCardContent cards (Messages.BlackCard idx) =
    String.split "\\BLANK" <| Maybe.withDefault "" <| Array.get idx cards.black

blackCardBlanks : Cards -> BlackCard -> Int
blackCardBlanks cards c = List.length (blackCardContent cards c) - 1

capitalizeFirst : List String -> List String
capitalizeFirst = List.indexedMap <| \i x -> if i == 0
    then String.toUpper (String.left 1 x) ++ String.dropLeft 1 x
    else x

blackCard
    : List (Html.Attribute a) -> Cards -> BlackCard -> List WhiteCard
    -> Html a
blackCard attrs cards black whites =
    let blackParts = blackCardContent cards black
        whiteParts = List.map (whiteCardContent cards) whites |>
            case blackParts of
                "" :: _ -> capitalizeFirst
                _ -> identity
        blank txt = Html.span
            [Html.Attributes.class "blank"]
            [Html.text txt] in
    Html.div
        (List.map Html.Attributes.class ["card", "black"] ++ attrs) <|
    intersperseWith (List.map blank whiteParts) (blank "") <|
    List.map Html.text blackParts

whiteCardContent : Cards -> WhiteCard -> String
whiteCardContent cards (Messages.WhiteCard idx) =
    Maybe.withDefault "" <| Array.get idx cards.white

whiteCard : Cards -> WhiteCard -> Bool -> Html Msg
whiteCard cards c selected = Html.div
    [ Html.Attributes.class "card"
    , Html.Attributes.class "white"
    , Html.Attributes.class <| if selected then "selected" else ""
    , Html.Events.onClick <| SelectWhiteCard c
    ]
    [ Html.text <| whiteCardContent cards c
    ]

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
                            { cards = {black = Array.empty, white = Array.empty}
                            , view = gameView
                            , changeMyName = gameView.me.name
                            , selectedWhiteCards = []
                            , selectedVote = Nothing
                            }
                        , Cmd.none
                        )
            Ok (Messages.SyncCards cards) ->
                let arr =
                        { black = Array.fromList cards.black
                        , white = Array.fromList cards.white
                        } in
                case model of
                    Game game -> (Game {game | cards = arr}, Cmd.none)
                    _ -> (model, Cmd.none)

    ChangeMyName name -> case model of
        Game game -> (Game {game | changeMyName = name}, Cmd.none)
        _ -> (model, Cmd.none)
    SubmitMyName -> case model of
        Game game -> (model, send <| Messages.ChangeMyName game.changeMyName)
        _ -> (model, Cmd.none)

    SelectWhiteCard card -> case model of
        Game game ->
            let cards = case List.member card game.selectedWhiteCards of
                    True  -> List.filter (\c -> c /= card) game.selectedWhiteCards
                    False -> List.take
                        (case tableBlackCard game of
                            Nothing -> 0
                            Just c  -> blackCardBlanks game.cards c - 1)
                        game.selectedWhiteCards ++
                        [card] in
            (Game {game | selectedWhiteCards = cards}, Cmd.none)
        _ -> (model, Cmd.none)

    ProposeWhiteCards -> case model of
        Game game ->
            ( Game {game | selectedWhiteCards = []}
            , send <| Messages.ProposeWhiteCards game.selectedWhiteCards
            )
        _ -> (model, Cmd.none)

    SelectVote i -> case model of
        Game game -> case game.view.table of
            Messages.Voting _ _ _ Nothing ->
                (Game {game | selectedVote = Just i}, Cmd.none)
            _ -> (model, Cmd.none)
        _ -> (model, Cmd.none)

    SubmitVote -> case model of
        Game game -> case game.selectedVote of
            Just vote ->
                ( Game {game | selectedVote = Nothing}
                , send <| Messages.SubmitVote vote
                )
            _ -> (model, Cmd.none)
        _ -> (model, Cmd.none)

    ConfirmTally -> (model, send <| Messages.ConfirmTally)

main : Program () Model Msg
main = Browser.application
    { init = \() url key -> (Connecting, Cmd.none)
    , update = update
    , subscriptions = subscriptions
    , view = \model -> {title = "Client", body = view model}
    , onUrlChange = \url -> Ignore
    , onUrlRequest = \urlRequest -> Ignore
    }

