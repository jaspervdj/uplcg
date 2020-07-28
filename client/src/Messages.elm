module Messages exposing(..)

import Json.Decode
import Json.Encode exposing (Value)
-- The following module comes from bartavelle/json-helpers
import Json.Helpers exposing (..)
import Dict exposing (Dict)
import Set exposing (Set)


type ServerMessage  =
    Welcome 

jsonDecServerMessage : Json.Decode.Decoder ( ServerMessage )
jsonDecServerMessage = 
    let jsonDecDictServerMessage = Dict.fromList [("Welcome", Welcome)]
    in  decodeSumUnaries "ServerMessage" jsonDecDictServerMessage

jsonEncServerMessage : ServerMessage -> Value
jsonEncServerMessage  val =
    case val of
        Welcome -> Json.Encode.string "Welcome"


