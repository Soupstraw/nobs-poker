module NoBSAPI exposing(..)

import Json.Decode
import Json.Encode exposing (Value)
-- The following module comes from bartavelle/json-helpers
import Json.Helpers exposing (..)
import Dict exposing (Dict)
import Set exposing (Set)


type ClientMsg  =
    CJoin String
    | CCreateRoom 
    | CLeave 
    | CSit Int
    | CSay String
    | CRaise Int
    | CCall 
    | CFold 

jsonDecClientMsg : Json.Decode.Decoder ( ClientMsg )
jsonDecClientMsg =
    let jsonDecDictClientMsg = Dict.fromList
            [ ("CJoin", Json.Decode.lazy (\_ -> Json.Decode.map CJoin (Json.Decode.string)))
            , ("CCreateRoom", Json.Decode.lazy (\_ -> Json.Decode.succeed CCreateRoom))
            , ("CLeave", Json.Decode.lazy (\_ -> Json.Decode.succeed CLeave))
            , ("CSit", Json.Decode.lazy (\_ -> Json.Decode.map CSit (Json.Decode.int)))
            , ("CSay", Json.Decode.lazy (\_ -> Json.Decode.map CSay (Json.Decode.string)))
            , ("CRaise", Json.Decode.lazy (\_ -> Json.Decode.map CRaise (Json.Decode.int)))
            , ("CCall", Json.Decode.lazy (\_ -> Json.Decode.succeed CCall))
            , ("CFold", Json.Decode.lazy (\_ -> Json.Decode.succeed CFold))
            ]
    in  decodeSumObjectWithSingleField  "ClientMsg" jsonDecDictClientMsg

jsonEncClientMsg : ClientMsg -> Value
jsonEncClientMsg  val =
    let keyval v = case v of
                    CJoin v1 -> ("CJoin", encodeValue (Json.Encode.string v1))
                    CCreateRoom  -> ("CCreateRoom", encodeValue (Json.Encode.list identity []))
                    CLeave  -> ("CLeave", encodeValue (Json.Encode.list identity []))
                    CSit v1 -> ("CSit", encodeValue (Json.Encode.int v1))
                    CSay v1 -> ("CSay", encodeValue (Json.Encode.string v1))
                    CRaise v1 -> ("CRaise", encodeValue (Json.Encode.int v1))
                    CCall  -> ("CCall", encodeValue (Json.Encode.list identity []))
                    CFold  -> ("CFold", encodeValue (Json.Encode.list identity []))
    in encodeSumObjectWithSingleField keyval val



type ServerMsg  =
    SRoomData RoomData
    | SJoin Player
    | SSay Player String
    | SLeave String
    | SSit String Int
    | SRaise String Int
    | SCall String
    | SFold String
    | SDrawCards 
    | SRoomCreated String

jsonDecServerMsg : Json.Decode.Decoder ( ServerMsg )
jsonDecServerMsg =
    let jsonDecDictServerMsg = Dict.fromList
            [ ("SRoomData", Json.Decode.lazy (\_ -> Json.Decode.map SRoomData (jsonDecRoomData)))
            , ("SJoin", Json.Decode.lazy (\_ -> Json.Decode.map SJoin (jsonDecPlayer)))
            , ("SSay", Json.Decode.lazy (\_ -> Json.Decode.map2 SSay (Json.Decode.index 0 (jsonDecPlayer)) (Json.Decode.index 1 (Json.Decode.string))))
            , ("SLeave", Json.Decode.lazy (\_ -> Json.Decode.map SLeave (Json.Decode.string)))
            , ("SSit", Json.Decode.lazy (\_ -> Json.Decode.map2 SSit (Json.Decode.index 0 (Json.Decode.string)) (Json.Decode.index 1 (Json.Decode.int))))
            , ("SRaise", Json.Decode.lazy (\_ -> Json.Decode.map2 SRaise (Json.Decode.index 0 (Json.Decode.string)) (Json.Decode.index 1 (Json.Decode.int))))
            , ("SCall", Json.Decode.lazy (\_ -> Json.Decode.map SCall (Json.Decode.string)))
            , ("SFold", Json.Decode.lazy (\_ -> Json.Decode.map SFold (Json.Decode.string)))
            , ("SDrawCards", Json.Decode.lazy (\_ -> Json.Decode.succeed SDrawCards))
            , ("SRoomCreated", Json.Decode.lazy (\_ -> Json.Decode.map SRoomCreated (Json.Decode.string)))
            ]
    in  decodeSumObjectWithSingleField  "ServerMsg" jsonDecDictServerMsg

jsonEncServerMsg : ServerMsg -> Value
jsonEncServerMsg  val =
    let keyval v = case v of
                    SRoomData v1 -> ("SRoomData", encodeValue (jsonEncRoomData v1))
                    SJoin v1 -> ("SJoin", encodeValue (jsonEncPlayer v1))
                    SSay v1 v2 -> ("SSay", encodeValue (Json.Encode.list identity [jsonEncPlayer v1, Json.Encode.string v2]))
                    SLeave v1 -> ("SLeave", encodeValue (Json.Encode.string v1))
                    SSit v1 v2 -> ("SSit", encodeValue (Json.Encode.list identity [Json.Encode.string v1, Json.Encode.int v2]))
                    SRaise v1 v2 -> ("SRaise", encodeValue (Json.Encode.list identity [Json.Encode.string v1, Json.Encode.int v2]))
                    SCall v1 -> ("SCall", encodeValue (Json.Encode.string v1))
                    SFold v1 -> ("SFold", encodeValue (Json.Encode.string v1))
                    SDrawCards  -> ("SDrawCards", encodeValue (Json.Encode.list identity []))
                    SRoomCreated v1 -> ("SRoomCreated", encodeValue (Json.Encode.string v1))
    in encodeSumObjectWithSingleField keyval val



type alias Player  =
   { pUserID: String
   , pUserName: String
   }

jsonDecPlayer : Json.Decode.Decoder ( Player )
jsonDecPlayer =
   Json.Decode.succeed (\ppUserID ppUserName -> {pUserID = ppUserID, pUserName = ppUserName})
   |> required "pUserID" (Json.Decode.string)
   |> required "pUserName" (Json.Decode.string)

jsonEncPlayer : Player -> Value
jsonEncPlayer  val =
   Json.Encode.object
   [ ("pUserID", Json.Encode.string val.pUserID)
   , ("pUserName", Json.Encode.string val.pUserName)
   ]



type alias RoomData  =
   { rdPlayers: (List Player)
   }

jsonDecRoomData : Json.Decode.Decoder ( RoomData )
jsonDecRoomData =
   Json.Decode.succeed (\prdPlayers -> {rdPlayers = prdPlayers}) |> custom (Json.Decode.list (jsonDecPlayer))

jsonEncRoomData : RoomData -> Value
jsonEncRoomData  val =
   (Json.Encode.list jsonEncPlayer) val.rdPlayers
