port module Main exposing (..)

import Browser exposing (..)
import Browser.Navigation exposing (Key)

import Element exposing (..)
import Element.Border as B
import Element.Font as F
import Element.Input as I

import Json.Decode exposing (decodeString)
import Json.Encode exposing (encode)

import List as L

import Maybe.Extra exposing (..)

import NoBSAPI as API

import String as S

import Url exposing (Url)
import Url.Parser as U
import Url.Parser exposing ((</>))

port port_sendMessage : String -> Cmd msg
port port_onConnect : (String -> msg) -> Sub msg
port port_messageReceiver : (String -> msg) -> Sub msg

seatCount : number
seatCount = 10

main : Program () Model Msg
main = application
  { init = init
  , view = document
  , update = update
  , subscriptions = subscriptions
  , onUrlChange = UrlChange
  , onUrlRequest = UrlReq
  }

type alias GameRec = 
  { raiseAmt : Int
  , minRaise : Int
  , messages : List String
  , draft    : String
  , roomId   : Int
  , players  : List API.Player
  }

type alias MenuRec =
  {
  }

type alias ConnRec =
  {
  }

type Model 
  = Game GameRec
  | Menu MenuRec
  | Conn ConnRec

type Msg 
  = Fold 
  | Call 
  | Raise
  | Send API.ClientMsg
  | Recv String
  | Draft String
  | SetBet String
  | UrlChange Url
  | UrlReq UrlRequest
  | Connected String
  | Sit Int
  | CreateRoom

init : a -> Url -> Key -> (Model, Cmd msg)
init _ url key =
  ( Conn {} 
  , Cmd.none)

document : Model -> Document Msg
document model = 
  { title = "Udupoker"
  , body = [Element.layout [] (view model)]
  }

update : Msg -> Model -> (Model, Cmd Msg)
update msg mdl =
  case mdl of
    Game model -> handleGameMsg model msg
    Menu model -> handleMenuMsg model msg
    Conn model -> handleConnMsg model msg

handleGameMsg : GameRec -> Msg -> (Model, Cmd Msg)
handleGameMsg model msg = 
  case msg of
    Draft x  -> 
      ( Game { model | draft = x }
      , Cmd.none
      )
    Raise    -> 
      ( Game { model | raiseAmt = 0}
      , sendMessage <| API.CRaise <| model.raiseAmt
      )
    Call     ->
      ( Game model
      , sendMessage API.CCall
      )
    Fold     ->
      ( Game model
      , sendMessage API.CFold
      )
    Recv m   -> 
      case decodeString API.jsonDecServerMsg m of
        Ok (API.SJoin userId) ->
          ( Game { model | players = model.players ++ [userId] }
          , Cmd.none
          )
        Ok (API.SSay user sayMsg) ->
          ( Game <| addMessage model <| user.pUserID ++ ": " ++ sayMsg
          , Cmd.none
          )
        x -> Debug.todo <| "Unexpected command: " ++ Debug.toString x
    Send x   -> 
      ( Game { model | draft = ""}
      , sendMessage x
      )
    SetBet x -> case String.toInt x of
      Just amt -> 
        ( Game { model |  raiseAmt = amt }
        , Cmd.none
        )
      Nothing  -> if x == "" 
                    then 
                      ( Game {model | raiseAmt = 0}
                      , Cmd.none
                      )
                    else 
                      ( Game model
                      , Cmd.none
                      )
    Sit x -> 
      ( Game model
      , sendMessage <| API.CSit <| x
      )
    x -> Debug.todo <| "Unexpected message: " ++ Debug.toString x

handleMenuMsg : MenuRec -> Msg -> (Model, Cmd Msg)
handleMenuMsg model msg =
  case msg of
    CreateRoom ->
      ( Conn {}
      , sendMessage API.CCreateRoom
      )
    x -> Debug.todo <| "Unexpected message: " ++ Debug.toString x

handleConnMsg : ConnRec -> Msg -> (Model, Cmd Msg)
handleConnMsg model msg =
  case msg of
    Connected x -> case Url.fromString x of
      Just url -> 
        case U.parse (U.s "room" </> U.string) url of
          Just roomId -> 
            ( Conn model
            , sendMessage <| API.CJoin roomId
            )
          Nothing     -> 
            ( Menu {}
            , Cmd.none
            )
      Nothing  -> (Conn model, Cmd.none)
    Recv m ->
      case decodeString API.jsonDecServerMsg m of
        Ok (API.SRoomCreated roomId) -> Debug.log "Got room created confirmation"
          ( Conn model
          , sendMessage <| API.CJoin roomId
          )
        Ok (API.SRoomData info) -> Debug.log "Got room info"
          ( Game <| fromData info
          , Cmd.none
          )
        x -> Debug.todo <| "Unexpected command: " ++ Debug.toString x
    x -> Debug.todo <| "Unexpected message: " ++ Debug.toString x

fromData : API.RoomData -> GameRec
fromData data = 
  { raiseAmt = 0
  , draft = ""
  , messages = []
  , minRaise = 0
  , players = data.rdPlayers
  , roomId = 0
  }

subscriptions : Model -> Sub Msg
subscriptions model = Sub.batch
  [ port_messageReceiver Recv
  , port_onConnect Connected
  ]

view : Model -> Element Msg
view mdl = 
  case mdl of
    Game model ->
      row 
      [ width fill
      , height fill
      ]
      [ column 
          [ width <| fillPortion 3
          , height fill
          ]
          [ gameView model
          , inputRow model
          ]
      , chatView model
      ]
    Menu model ->
      row
        [ width fill
        , height fill
        ]
        [ I.button 
            [ width fill
            , height fill
            , centerX
            , centerY
            , F.center
            ]
            { label = text "Create room"
            , onPress = Just CreateRoom
            }
        ]
    Conn model ->
      row
        [ width fill
        , height fill
        , F.center
        ]
        [ text "Connecting.."
        ]

gameView : GameRec -> Element Msg
gameView model = el 
  ( [ height <| fillPortion 3
    , width fill
    ] ++ seats model
  ) <|
  column 
    [ centerX
    , centerY
    ] 
    [ text "$0"
    ]

chatView : GameRec -> Element Msg
chatView model = column
  [ width fill 
  , height fill
  , B.width 1
  ]
  [ column
      [ width fill
      , height fill
      , padding 10
      , B.width 1
      ]
      (List.map (text << .pUserID) model.players)
  , column 
      [ width fill
      , height fill
      , padding 10
      , B.width 1
      ] 
      (msgElems model)
  , row [alignBottom]
      [ I.text [] 
          { label = I.labelHidden "Send a message.."
          , onChange = Draft
          , placeholder = Just (I.placeholder [] <| text "Send a message..")
          , text = model.draft
          }
      , I.button [alignRight]
          { label = text "Send"
          , onPress = Just <| Send (API.CSay model.draft)
          }
      ]
  ]

msgElems : GameRec -> List (Element Msg)
msgElems model = L.map text model.messages

seats : GameRec -> List (Attribute Msg)
seats model = 
  let
    f i = inFront <| tablePlayer i model
  in L.map f <| L.range 1 seatCount

inputRow : GameRec -> Element Msg
inputRow model = row 
  [ padding 10
  , B.width 1
  , B.solid
  , width fill
  , height fill
  , spaceEvenly
  , alignBottom
  ]
  [ I.button buttonStyle
      { onPress = Just Fold
      , label = text "Fold"
      }
  , I.button buttonStyle
      { onPress = Just Call
      , label = text "Call"
      }
  , column [width fill, height fill]
      [ I.text [alignTop] 
          { label = I.labelHidden "Raise amount"
          , onChange = SetBet
          , placeholder = Just (I.placeholder [] <| text <| S.fromInt model.minRaise)
          , text = if model.raiseAmt == 0
                     then ""
                     else S.fromInt model.raiseAmt
          }
      , I.button buttonStyle
          { onPress = Just Raise
          , label = text "Raise"
          }
      ]
  ]

buttonStyle : List (Attribute msg)
buttonStyle =
  [ width fill
  , height fill
  , F.center
  ]

tablePlayer : Int -> GameRec -> Element Msg
tablePlayer idx model =
  let ang = 2*pi*(toFloat idx)/seatCount
  in I.button 
       [ moveRight (500 * (sin ang))
       , moveDown (250 * (cos ang))
       , centerX
       , centerY
       ]
       { onPress = Just <| Sit idx
       , label = text "Sit"
       }

playerStyle : List (Attribute msg)
playerStyle =
  [ centerX
  , centerY
  ]

sendMessage : API.ClientMsg -> Cmd Msg
sendMessage msg =
  port_sendMessage 
  <| encode 0
  <| API.jsonEncClientMsg msg

addMessage : GameRec -> String -> GameRec
addMessage model msg = 
  { model | messages = model.messages ++ [msg] }

