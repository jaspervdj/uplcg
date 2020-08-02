module Messages exposing(..)

import Json.Decode
import Json.Encode exposing (Value)
-- The following module comes from bartavelle/json-helpers
import Json.Helpers exposing (..)
import Dict exposing (Dict)
import Set exposing (Set)


type BlackCard  =
    BlackCard Int

jsonDecBlackCard : Json.Decode.Decoder ( BlackCard )
jsonDecBlackCard =
    Json.Decode.lazy (\_ -> Json.Decode.map BlackCard (Json.Decode.int))


jsonEncBlackCard : BlackCard -> Value
jsonEncBlackCard (BlackCard v1) =
    Json.Encode.int v1



type WhiteCard  =
    WhiteCard Int

jsonDecWhiteCard : Json.Decode.Decoder ( WhiteCard )
jsonDecWhiteCard =
    Json.Decode.lazy (\_ -> Json.Decode.map WhiteCard (Json.Decode.int))


jsonEncWhiteCard : WhiteCard -> Value
jsonEncWhiteCard (WhiteCard v1) =
    Json.Encode.int v1



type alias Cards  =
   { black: (List String)
   , white: (List String)
   }

jsonDecCards : Json.Decode.Decoder ( Cards )
jsonDecCards =
   Json.Decode.succeed (\pblack pwhite -> {black = pblack, white = pwhite})
   |> required "black" (Json.Decode.list (Json.Decode.string))
   |> required "white" (Json.Decode.list (Json.Decode.string))

jsonEncCards : Cards -> Value
jsonEncCards  val =
   Json.Encode.object
   [ ("black", (Json.Encode.list Json.Encode.string) val.black)
   , ("white", (Json.Encode.list Json.Encode.string) val.white)
   ]



type alias Opponent  =
   { name: String
   , admin: Bool
   , ready: Bool
   }

jsonDecOpponent : Json.Decode.Decoder ( Opponent )
jsonDecOpponent =
   Json.Decode.succeed (\pname padmin pready -> {name = pname, admin = padmin, ready = pready})
   |> required "name" (Json.Decode.string)
   |> required "admin" (Json.Decode.bool)
   |> required "ready" (Json.Decode.bool)

jsonEncOpponent : Opponent -> Value
jsonEncOpponent  val =
   Json.Encode.object
   [ ("name", Json.Encode.string val.name)
   , ("admin", Json.Encode.bool val.admin)
   , ("ready", Json.Encode.bool val.ready)
   ]



type TableView  =
    Proposing BlackCard (List WhiteCard)
    | Voting BlackCard (List (List WhiteCard)) Int (Maybe Int)

jsonDecTableView : Json.Decode.Decoder ( TableView )
jsonDecTableView =
    let jsonDecDictTableView = Dict.fromList
            [ ("Proposing", Json.Decode.lazy (\_ -> Json.Decode.map2 Proposing (Json.Decode.index 0 (jsonDecBlackCard)) (Json.Decode.index 1 (Json.Decode.list (jsonDecWhiteCard)))))
            , ("Voting", Json.Decode.lazy (\_ -> Json.Decode.map4 Voting (Json.Decode.index 0 (jsonDecBlackCard)) (Json.Decode.index 1 (Json.Decode.list (Json.Decode.list (jsonDecWhiteCard)))) (Json.Decode.index 2 (Json.Decode.int)) (Json.Decode.index 3 (Json.Decode.maybe (Json.Decode.int)))))
            ]
    in  decodeSumObjectWithSingleField  "TableView" jsonDecDictTableView

jsonEncTableView : TableView -> Value
jsonEncTableView  val =
    let keyval v = case v of
                    Proposing v1 v2 -> ("Proposing", encodeValue (Json.Encode.list identity [jsonEncBlackCard v1, (Json.Encode.list jsonEncWhiteCard) v2]))
                    Voting v1 v2 v3 v4 -> ("Voting", encodeValue (Json.Encode.list identity [jsonEncBlackCard v1, (Json.Encode.list (Json.Encode.list jsonEncWhiteCard)) v2, Json.Encode.int v3, (maybeEncode (Json.Encode.int)) v4]))
    in encodeSumObjectWithSingleField keyval val



type alias GameView  =
   { opponents: (List Opponent)
   , myName: String
   , table: TableView
   , hand: (List WhiteCard)
   }

jsonDecGameView : Json.Decode.Decoder ( GameView )
jsonDecGameView =
   Json.Decode.succeed (\popponents pmyName ptable phand -> {opponents = popponents, myName = pmyName, table = ptable, hand = phand})
   |> required "opponents" (Json.Decode.list (jsonDecOpponent))
   |> required "myName" (Json.Decode.string)
   |> required "table" (jsonDecTableView)
   |> required "hand" (Json.Decode.list (jsonDecWhiteCard))

jsonEncGameView : GameView -> Value
jsonEncGameView  val =
   Json.Encode.object
   [ ("opponents", (Json.Encode.list jsonEncOpponent) val.opponents)
   , ("myName", Json.Encode.string val.myName)
   , ("table", jsonEncTableView val.table)
   , ("hand", (Json.Encode.list jsonEncWhiteCard) val.hand)
   ]



type ServerMessage  =
    Welcome Int
    | SyncCards Cards
    | SyncGameView GameView
    | Bye 

jsonDecServerMessage : Json.Decode.Decoder ( ServerMessage )
jsonDecServerMessage =
    let jsonDecDictServerMessage = Dict.fromList
            [ ("Welcome", Json.Decode.lazy (\_ -> Json.Decode.map Welcome (Json.Decode.int)))
            , ("SyncCards", Json.Decode.lazy (\_ -> Json.Decode.map SyncCards (jsonDecCards)))
            , ("SyncGameView", Json.Decode.lazy (\_ -> Json.Decode.map SyncGameView (jsonDecGameView)))
            , ("Bye", Json.Decode.lazy (\_ -> Json.Decode.succeed Bye))
            ]
    in  decodeSumObjectWithSingleField  "ServerMessage" jsonDecDictServerMessage

jsonEncServerMessage : ServerMessage -> Value
jsonEncServerMessage  val =
    let keyval v = case v of
                    Welcome v1 -> ("Welcome", encodeValue (Json.Encode.int v1))
                    SyncCards v1 -> ("SyncCards", encodeValue (jsonEncCards v1))
                    SyncGameView v1 -> ("SyncGameView", encodeValue (jsonEncGameView v1))
                    Bye  -> ("Bye", encodeValue (Json.Encode.list identity []))
    in encodeSumObjectWithSingleField keyval val



type ClientMessage  =
    ChangeMyName String
    | ProposeWhiteCards (List WhiteCard)
    | SubmitVote Int

jsonDecClientMessage : Json.Decode.Decoder ( ClientMessage )
jsonDecClientMessage =
    let jsonDecDictClientMessage = Dict.fromList
            [ ("ChangeMyName", Json.Decode.lazy (\_ -> Json.Decode.map ChangeMyName (Json.Decode.string)))
            , ("ProposeWhiteCards", Json.Decode.lazy (\_ -> Json.Decode.map ProposeWhiteCards (Json.Decode.list (jsonDecWhiteCard))))
            , ("SubmitVote", Json.Decode.lazy (\_ -> Json.Decode.map SubmitVote (Json.Decode.int)))
            ]
    in  decodeSumObjectWithSingleField  "ClientMessage" jsonDecDictClientMessage

jsonEncClientMessage : ClientMessage -> Value
jsonEncClientMessage  val =
    let keyval v = case v of
                    ChangeMyName v1 -> ("ChangeMyName", encodeValue (Json.Encode.string v1))
                    ProposeWhiteCards v1 -> ("ProposeWhiteCards", encodeValue ((Json.Encode.list jsonEncWhiteCard) v1))
                    SubmitVote v1 -> ("SubmitVote", encodeValue (Json.Encode.int v1))
    in encodeSumObjectWithSingleField keyval val


