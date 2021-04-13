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
    | SSay Unique String
    | SLeave Unique
    | SSit Unique Int
    | SRaise Unique Int
    | SCall Unique
    | SFold Unique
    | SDrawCards 
    | SRoomCreated Unique

jsonDecServerMsg : Json.Decode.Decoder ( ServerMsg )
jsonDecServerMsg =
    let jsonDecDictServerMsg = Dict.fromList
            [ ("SRoomData", Json.Decode.lazy (\_ -> Json.Decode.map SRoomData (jsonDecRoomData)))
            , ("SJoin", Json.Decode.lazy (\_ -> Json.Decode.map SJoin (jsonDecPlayer)))
            , ("SSay", Json.Decode.lazy (\_ -> Json.Decode.map2 SSay (Json.Decode.index 0 (jsonDecUnique)) (Json.Decode.index 1 (Json.Decode.string))))
            , ("SLeave", Json.Decode.lazy (\_ -> Json.Decode.map SLeave (jsonDecUnique)))
            , ("SSit", Json.Decode.lazy (\_ -> Json.Decode.map2 SSit (Json.Decode.index 0 (jsonDecUnique)) (Json.Decode.index 1 (Json.Decode.int))))
            , ("SRaise", Json.Decode.lazy (\_ -> Json.Decode.map2 SRaise (Json.Decode.index 0 (jsonDecUnique)) (Json.Decode.index 1 (Json.Decode.int))))
            , ("SCall", Json.Decode.lazy (\_ -> Json.Decode.map SCall (jsonDecUnique)))
            , ("SFold", Json.Decode.lazy (\_ -> Json.Decode.map SFold (jsonDecUnique)))
            , ("SDrawCards", Json.Decode.lazy (\_ -> Json.Decode.succeed SDrawCards))
            , ("SRoomCreated", Json.Decode.lazy (\_ -> Json.Decode.map SRoomCreated (jsonDecUnique)))
            ]
    in  decodeSumObjectWithSingleField  "ServerMsg" jsonDecDictServerMsg

jsonEncServerMsg : ServerMsg -> Value
jsonEncServerMsg  val =
    let keyval v = case v of
                    SRoomData v1 -> ("SRoomData", encodeValue (jsonEncRoomData v1))
                    SJoin v1 -> ("SJoin", encodeValue (jsonEncPlayer v1))
                    SSay v1 v2 -> ("SSay", encodeValue (Json.Encode.list identity [jsonEncUnique v1, Json.Encode.string v2]))
                    SLeave v1 -> ("SLeave", encodeValue (jsonEncUnique v1))
                    SSit v1 v2 -> ("SSit", encodeValue (Json.Encode.list identity [jsonEncUnique v1, Json.Encode.int v2]))
                    SRaise v1 v2 -> ("SRaise", encodeValue (Json.Encode.list identity [jsonEncUnique v1, Json.Encode.int v2]))
                    SCall v1 -> ("SCall", encodeValue (jsonEncUnique v1))
                    SFold v1 -> ("SFold", encodeValue (jsonEncUnique v1))
                    SDrawCards  -> ("SDrawCards", encodeValue (Json.Encode.list identity []))
                    SRoomCreated v1 -> ("SRoomCreated", encodeValue (jsonEncUnique v1))
    in encodeSumObjectWithSingleField keyval val



type alias Player  =
   { pUserID: Unique
   , pUserName: String
   , pSeat: (Maybe Int)
   , pCards: Int
   , pHand: (List Card)
   , pPlaying: Bool
   }

jsonDecPlayer : Json.Decode.Decoder ( Player )
jsonDecPlayer =
   Json.Decode.succeed (\ppUserID ppUserName ppSeat ppCards ppHand ppPlaying -> {pUserID = ppUserID, pUserName = ppUserName, pSeat = ppSeat, pCards = ppCards, pHand = ppHand, pPlaying = ppPlaying})
   |> required "pUserID" (jsonDecUnique)
   |> required "pUserName" (Json.Decode.string)
   |> fnullable "pSeat" (Json.Decode.int)
   |> required "pCards" (Json.Decode.int)
   |> required "pHand" (Json.Decode.list (jsonDecCard))
   |> required "pPlaying" (Json.Decode.bool)

jsonEncPlayer : Player -> Value
jsonEncPlayer  val =
   Json.Encode.object
   [ ("pUserID", jsonEncUnique val.pUserID)
   , ("pUserName", Json.Encode.string val.pUserName)
   , ("pSeat", (maybeEncode (Json.Encode.int)) val.pSeat)
   , ("pCards", Json.Encode.int val.pCards)
   , ("pHand", (Json.Encode.list jsonEncCard) val.pHand)
   , ("pPlaying", Json.Encode.bool val.pPlaying)
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


type alias Unique  = String

jsonDecUnique : Json.Decode.Decoder ( Unique )
jsonDecUnique =
    Json.Decode.string

jsonEncUnique : Unique -> Value
jsonEncUnique  val = Json.Encode.string val



type Rank  =
    R2 
    | R3 
    | R4 
    | R5 
    | R6 
    | R7 
    | R8 
    | R9 
    | RT 
    | RJ 
    | RQ 
    | RK 
    | RA 

jsonDecRank : Json.Decode.Decoder ( Rank )
jsonDecRank = 
    let jsonDecDictRank = Dict.fromList [("R2", R2), ("R3", R3), ("R4", R4), ("R5", R5), ("R6", R6), ("R7", R7), ("R8", R8), ("R9", R9), ("RT", RT), ("RJ", RJ), ("RQ", RQ), ("RK", RK), ("RA", RA)]
    in  decodeSumUnaries "Rank" jsonDecDictRank

jsonEncRank : Rank -> Value
jsonEncRank  val =
    case val of
        R2 -> Json.Encode.string "R2"
        R3 -> Json.Encode.string "R3"
        R4 -> Json.Encode.string "R4"
        R5 -> Json.Encode.string "R5"
        R6 -> Json.Encode.string "R6"
        R7 -> Json.Encode.string "R7"
        R8 -> Json.Encode.string "R8"
        R9 -> Json.Encode.string "R9"
        RT -> Json.Encode.string "RT"
        RJ -> Json.Encode.string "RJ"
        RQ -> Json.Encode.string "RQ"
        RK -> Json.Encode.string "RK"
        RA -> Json.Encode.string "RA"



type Suit  =
    Clubs 
    | Diamonds 
    | Hearts 
    | Spades 

jsonDecSuit : Json.Decode.Decoder ( Suit )
jsonDecSuit = 
    let jsonDecDictSuit = Dict.fromList [("Clubs", Clubs), ("Diamonds", Diamonds), ("Hearts", Hearts), ("Spades", Spades)]
    in  decodeSumUnaries "Suit" jsonDecDictSuit

jsonEncSuit : Suit -> Value
jsonEncSuit  val =
    case val of
        Clubs -> Json.Encode.string "Clubs"
        Diamonds -> Json.Encode.string "Diamonds"
        Hearts -> Json.Encode.string "Hearts"
        Spades -> Json.Encode.string "Spades"



type alias Card  =
   { cRank: Rank
   , cSuit: Suit
   }

jsonDecCard : Json.Decode.Decoder ( Card )
jsonDecCard =
   Json.Decode.succeed (\pcRank pcSuit -> {cRank = pcRank, cSuit = pcSuit})
   |> required "cRank" (jsonDecRank)
   |> required "cSuit" (jsonDecSuit)

jsonEncCard : Card -> Value
jsonEncCard  val =
   Json.Encode.object
   [ ("cRank", jsonEncRank val.cRank)
   , ("cSuit", jsonEncSuit val.cSuit)
   ]

