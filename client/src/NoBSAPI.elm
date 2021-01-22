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



type alias RoomData  = (List Player)

jsonDecRoomData : Json.Decode.Decoder ( RoomData )
jsonDecRoomData =
    Json.Decode.list (jsonDecPlayer)

jsonEncRoomData : RoomData -> Value
jsonEncRoomData  val = (Json.Encode.list jsonEncPlayer) val



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



type PokerHand  =
    HighCard Card
    | OnePair (Card, Card)
    | TwoPair (Card, Card) (Card, Card)
    | ThreeOfAKind (Card, Card, Card)
    | Straight (Card, Card, Card, Card, Card)
    | FullHouse (Card, Card, Card) (Card, Card)
    | Flush (Card, Card, Card, Card, Card)
    | FourOfAKind (Card, Card, Card, Card)
    | StraightFlush (Card, Card, Card, Card, Card)
    | DoubleStraightFlush (Card, Card, Card, Card, Card) (Card, Card, Card, Card, Card)
    | TripleStraightFlush (Card, Card, Card, Card, Card) (Card, Card, Card, Card, Card) (Card, Card, Card, Card, Card)
    | QuadStraightFlush (Card, Card, Card, Card, Card) (Card, Card, Card, Card, Card) (Card, Card, Card, Card, Card) (Card, Card, Card, Card, Card)

jsonDecPokerHand : Json.Decode.Decoder ( PokerHand )
jsonDecPokerHand =
    let jsonDecDictPokerHand = Dict.fromList
            [ ("HighCard", Json.Decode.lazy (\_ -> Json.Decode.map HighCard (jsonDecCard)))
            , ("OnePair", Json.Decode.lazy (\_ -> Json.Decode.map OnePair (Json.Decode.map2 tuple2 (Json.Decode.index 0 (jsonDecCard)) (Json.Decode.index 1 (jsonDecCard)))))
            , ("TwoPair", Json.Decode.lazy (\_ -> Json.Decode.map2 TwoPair (Json.Decode.index 0 (Json.Decode.map2 tuple2 (Json.Decode.index 0 (jsonDecCard)) (Json.Decode.index 1 (jsonDecCard)))) (Json.Decode.index 1 (Json.Decode.map2 tuple2 (Json.Decode.index 0 (jsonDecCard)) (Json.Decode.index 1 (jsonDecCard))))))
            , ("ThreeOfAKind", Json.Decode.lazy (\_ -> Json.Decode.map ThreeOfAKind (Json.Decode.map3 tuple3 (Json.Decode.index 0 (jsonDecCard)) (Json.Decode.index 1 (jsonDecCard)) (Json.Decode.index 2 (jsonDecCard)))))
            , ("Straight", Json.Decode.lazy (\_ -> Json.Decode.map Straight (Json.Decode.map5 tuple5 (Json.Decode.index 0 (jsonDecCard)) (Json.Decode.index 1 (jsonDecCard)) (Json.Decode.index 2 (jsonDecCard)) (Json.Decode.index 3 (jsonDecCard)) (Json.Decode.index 4 (jsonDecCard)))))
            , ("FullHouse", Json.Decode.lazy (\_ -> Json.Decode.map2 FullHouse (Json.Decode.index 0 (Json.Decode.map3 tuple3 (Json.Decode.index 0 (jsonDecCard)) (Json.Decode.index 1 (jsonDecCard)) (Json.Decode.index 2 (jsonDecCard)))) (Json.Decode.index 1 (Json.Decode.map2 tuple2 (Json.Decode.index 0 (jsonDecCard)) (Json.Decode.index 1 (jsonDecCard))))))
            , ("Flush", Json.Decode.lazy (\_ -> Json.Decode.map Flush (Json.Decode.map5 tuple5 (Json.Decode.index 0 (jsonDecCard)) (Json.Decode.index 1 (jsonDecCard)) (Json.Decode.index 2 (jsonDecCard)) (Json.Decode.index 3 (jsonDecCard)) (Json.Decode.index 4 (jsonDecCard)))))
            , ("FourOfAKind", Json.Decode.lazy (\_ -> Json.Decode.map FourOfAKind (Json.Decode.map4 tuple4 (Json.Decode.index 0 (jsonDecCard)) (Json.Decode.index 1 (jsonDecCard)) (Json.Decode.index 2 (jsonDecCard)) (Json.Decode.index 3 (jsonDecCard)))))
            , ("StraightFlush", Json.Decode.lazy (\_ -> Json.Decode.map StraightFlush (Json.Decode.map5 tuple5 (Json.Decode.index 0 (jsonDecCard)) (Json.Decode.index 1 (jsonDecCard)) (Json.Decode.index 2 (jsonDecCard)) (Json.Decode.index 3 (jsonDecCard)) (Json.Decode.index 4 (jsonDecCard)))))
            , ("DoubleStraightFlush", Json.Decode.lazy (\_ -> Json.Decode.map2 DoubleStraightFlush (Json.Decode.index 0 (Json.Decode.map5 tuple5 (Json.Decode.index 0 (jsonDecCard)) (Json.Decode.index 1 (jsonDecCard)) (Json.Decode.index 2 (jsonDecCard)) (Json.Decode.index 3 (jsonDecCard)) (Json.Decode.index 4 (jsonDecCard)))) (Json.Decode.index 1 (Json.Decode.map5 tuple5 (Json.Decode.index 0 (jsonDecCard)) (Json.Decode.index 1 (jsonDecCard)) (Json.Decode.index 2 (jsonDecCard)) (Json.Decode.index 3 (jsonDecCard)) (Json.Decode.index 4 (jsonDecCard))))))
            , ("TripleStraightFlush", Json.Decode.lazy (\_ -> Json.Decode.map3 TripleStraightFlush (Json.Decode.index 0 (Json.Decode.map5 tuple5 (Json.Decode.index 0 (jsonDecCard)) (Json.Decode.index 1 (jsonDecCard)) (Json.Decode.index 2 (jsonDecCard)) (Json.Decode.index 3 (jsonDecCard)) (Json.Decode.index 4 (jsonDecCard)))) (Json.Decode.index 1 (Json.Decode.map5 tuple5 (Json.Decode.index 0 (jsonDecCard)) (Json.Decode.index 1 (jsonDecCard)) (Json.Decode.index 2 (jsonDecCard)) (Json.Decode.index 3 (jsonDecCard)) (Json.Decode.index 4 (jsonDecCard)))) (Json.Decode.index 2 (Json.Decode.map5 tuple5 (Json.Decode.index 0 (jsonDecCard)) (Json.Decode.index 1 (jsonDecCard)) (Json.Decode.index 2 (jsonDecCard)) (Json.Decode.index 3 (jsonDecCard)) (Json.Decode.index 4 (jsonDecCard))))))
            , ("QuadStraightFlush", Json.Decode.lazy (\_ -> Json.Decode.map4 QuadStraightFlush (Json.Decode.index 0 (Json.Decode.map5 tuple5 (Json.Decode.index 0 (jsonDecCard)) (Json.Decode.index 1 (jsonDecCard)) (Json.Decode.index 2 (jsonDecCard)) (Json.Decode.index 3 (jsonDecCard)) (Json.Decode.index 4 (jsonDecCard)))) (Json.Decode.index 1 (Json.Decode.map5 tuple5 (Json.Decode.index 0 (jsonDecCard)) (Json.Decode.index 1 (jsonDecCard)) (Json.Decode.index 2 (jsonDecCard)) (Json.Decode.index 3 (jsonDecCard)) (Json.Decode.index 4 (jsonDecCard)))) (Json.Decode.index 2 (Json.Decode.map5 tuple5 (Json.Decode.index 0 (jsonDecCard)) (Json.Decode.index 1 (jsonDecCard)) (Json.Decode.index 2 (jsonDecCard)) (Json.Decode.index 3 (jsonDecCard)) (Json.Decode.index 4 (jsonDecCard)))) (Json.Decode.index 3 (Json.Decode.map5 tuple5 (Json.Decode.index 0 (jsonDecCard)) (Json.Decode.index 1 (jsonDecCard)) (Json.Decode.index 2 (jsonDecCard)) (Json.Decode.index 3 (jsonDecCard)) (Json.Decode.index 4 (jsonDecCard))))))
            ]
    in  decodeSumObjectWithSingleField  "PokerHand" jsonDecDictPokerHand

jsonEncPokerHand : PokerHand -> Value
jsonEncPokerHand  val =
    let keyval v = case v of
                    HighCard v1 -> ("HighCard", encodeValue (jsonEncCard v1))
                    OnePair v1 -> ("OnePair", encodeValue ((\(t1,t2) -> Json.Encode.list identity [(jsonEncCard) t1,(jsonEncCard) t2]) v1))
                    TwoPair v1 v2 -> ("TwoPair", encodeValue (Json.Encode.list identity [(\(t1,t2) -> Json.Encode.list identity [(jsonEncCard) t1,(jsonEncCard) t2]) v1, (\(t1,t2) -> Json.Encode.list identity [(jsonEncCard) t1,(jsonEncCard) t2]) v2]))
                    ThreeOfAKind v1 -> ("ThreeOfAKind", encodeValue ((\(t1,t2,t3) -> Json.Encode.list identity [(jsonEncCard) t1,(jsonEncCard) t2,(jsonEncCard) t3]) v1))
                    Straight v1 -> ("Straight", encodeValue ((\(t1,t2,t3,t4,t5) -> Json.Encode.list identity [(jsonEncCard) t1,(jsonEncCard) t2,(jsonEncCard) t3,(jsonEncCard) t4,(jsonEncCard) t5]) v1))
                    FullHouse v1 v2 -> ("FullHouse", encodeValue (Json.Encode.list identity [(\(t1,t2,t3) -> Json.Encode.list identity [(jsonEncCard) t1,(jsonEncCard) t2,(jsonEncCard) t3]) v1, (\(t1,t2) -> Json.Encode.list identity [(jsonEncCard) t1,(jsonEncCard) t2]) v2]))
                    Flush v1 -> ("Flush", encodeValue ((\(t1,t2,t3,t4,t5) -> Json.Encode.list identity [(jsonEncCard) t1,(jsonEncCard) t2,(jsonEncCard) t3,(jsonEncCard) t4,(jsonEncCard) t5]) v1))
                    FourOfAKind v1 -> ("FourOfAKind", encodeValue ((\(t1,t2,t3,t4) -> Json.Encode.list identity [(jsonEncCard) t1,(jsonEncCard) t2,(jsonEncCard) t3,(jsonEncCard) t4]) v1))
                    StraightFlush v1 -> ("StraightFlush", encodeValue ((\(t1,t2,t3,t4,t5) -> Json.Encode.list identity [(jsonEncCard) t1,(jsonEncCard) t2,(jsonEncCard) t3,(jsonEncCard) t4,(jsonEncCard) t5]) v1))
                    DoubleStraightFlush v1 v2 -> ("DoubleStraightFlush", encodeValue (Json.Encode.list identity [(\(t1,t2,t3,t4,t5) -> Json.Encode.list identity [(jsonEncCard) t1,(jsonEncCard) t2,(jsonEncCard) t3,(jsonEncCard) t4,(jsonEncCard) t5]) v1, (\(t1,t2,t3,t4,t5) -> Json.Encode.list identity [(jsonEncCard) t1,(jsonEncCard) t2,(jsonEncCard) t3,(jsonEncCard) t4,(jsonEncCard) t5]) v2]))
                    TripleStraightFlush v1 v2 v3 -> ("TripleStraightFlush", encodeValue (Json.Encode.list identity [(\(t1,t2,t3,t4,t5) -> Json.Encode.list identity [(jsonEncCard) t1,(jsonEncCard) t2,(jsonEncCard) t3,(jsonEncCard) t4,(jsonEncCard) t5]) v1, (\(t1,t2,t3,t4,t5) -> Json.Encode.list identity [(jsonEncCard) t1,(jsonEncCard) t2,(jsonEncCard) t3,(jsonEncCard) t4,(jsonEncCard) t5]) v2, (\(t1,t2,t3,t4,t5) -> Json.Encode.list identity [(jsonEncCard) t1,(jsonEncCard) t2,(jsonEncCard) t3,(jsonEncCard) t4,(jsonEncCard) t5]) v3]))
                    QuadStraightFlush v1 v2 v3 v4 -> ("QuadStraightFlush", encodeValue (Json.Encode.list identity [(\(t1,t2,t3,t4,t5) -> Json.Encode.list identity [(jsonEncCard) t1,(jsonEncCard) t2,(jsonEncCard) t3,(jsonEncCard) t4,(jsonEncCard) t5]) v1, (\(t1,t2,t3,t4,t5) -> Json.Encode.list identity [(jsonEncCard) t1,(jsonEncCard) t2,(jsonEncCard) t3,(jsonEncCard) t4,(jsonEncCard) t5]) v2, (\(t1,t2,t3,t4,t5) -> Json.Encode.list identity [(jsonEncCard) t1,(jsonEncCard) t2,(jsonEncCard) t3,(jsonEncCard) t4,(jsonEncCard) t5]) v3, (\(t1,t2,t3,t4,t5) -> Json.Encode.list identity [(jsonEncCard) t1,(jsonEncCard) t2,(jsonEncCard) t3,(jsonEncCard) t4,(jsonEncCard) t5]) v4]))
    in encodeSumObjectWithSingleField keyval val

