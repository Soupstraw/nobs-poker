port module Main exposing (..)

import Browser exposing (..)
import Browser.Events exposing (..)
import Browser.Navigation exposing (..)

import Element exposing (..)
import Element.Border as B
import Element.Font as F
import Element.Input as I

import Html.Events as HE

import Json.Decode as JD
import Json.Encode as JE

import List as L
import List.Extra exposing (..)

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

color : { blue : Color, darkCharcoal : Color, lightBlue : Color, lightGrey : Color, white : Color }
color = 
    { blue = rgb255 0x72 0x9F 0xCF
    , darkCharcoal = rgb255 0x2E 0x34 0x36
    , lightBlue = rgb255 0xC5 0xE8 0xF7
    , lightGrey = rgb255 0xE0 0xE0 0xE0
    , white = rgb255 0xFF 0xFF 0xFF
    }

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
  { messages      : List String
  , draft         : String
  , roomId        : Int
  , players       : List API.Player
  , key           : Key
  , currentBid    : Maybe API.Bid
  , selectedBid   : Maybe API.Bid
  , playerSitting : Bool
  }

type alias MenuRec =
  { key : Key
  }

type alias ConnRec =
  { key : Key
  }

type Model 
  = Game GameRec
  | Menu MenuRec
  | Conn ConnRec

type Msg 
  = CallBluff
  | Bid API.Bid
  | Send API.ClientMsg
  | Recv String
  | Draft String
  | UrlChange Url
  | UrlReq UrlRequest
  | Connected String
  | Sit Int
  | CreateRoom
  | ChatKeyDown

init : a -> Url -> Key -> (Model, Cmd msg)
init _ url key =
  ( Conn { key = key } 
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
    Draft x   -> 
      ( Game { model | draft = x }
      , Cmd.none
      )
    Bid bid   -> 
      ( Game model
      , sendMessage <| API.CBid bid
      )
    CallBluff ->
      ( Game model
      , sendMessage API.CCallBluff
      )
    Recv m   -> 
      case JD.decodeString API.jsonDecServerMsg m of
        Ok (API.SJoin user) ->
          ( Game { model | players = model.players ++ [user] }
          , Cmd.none
          )
        Ok (API.SSay userId sayMsg) ->
          case getUser model userId of
            Just user ->
              ( Game <| addMessage model <| user.pUserName ++ ": " ++ sayMsg
              , Cmd.none
              )
            Nothing ->
              ( Debug.log "Got a message from a non-existent player" <| Game model
              , Cmd.none
              )
        Ok (API.SSit userId seatIdx) ->
          let
              f p = { p | pSeat = Just seatIdx }
          in
            ( Game { model | players = updateIf (\x -> x.pUserID == userId) f model.players}
            , Cmd.none
            )
        x -> Debug.todo <| "Unexpected command: " ++ Debug.toString x
    Send x   -> 
      ( Game { model | draft = ""}
      , sendMessage x
      )
    Sit x -> 
      ( Game {model | playerSitting = True}
      , sendMessage <| API.CSit <| x
      )
    x -> Debug.todo <| "Unexpected message: " ++ Debug.toString x

getUser : GameRec -> API.Unique -> Maybe API.Player
getUser model userId = find ((==) userId << .pUserID) model.players

handleMenuMsg : MenuRec -> Msg -> (Model, Cmd Msg)
handleMenuMsg model msg =
  case msg of
    CreateRoom ->
      ( Conn { key = model.key }
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
            ( Menu { key = model.key }
            , Cmd.none
            )
      Nothing  -> (Conn model, Cmd.none)
    Recv m ->
      case JD.decodeString API.jsonDecServerMsg m of
        Ok (API.SRoomCreated roomId) -> Debug.log "Got room created confirmation"
          ( Conn model
          , Cmd.batch
              [ sendMessage <| API.CJoin roomId
              , pushUrl model.key <| "http://localhost:8080/room/" ++ roomId
              ]
          )
        Ok (API.SRoomData info) -> Debug.log "Got room info"
          ( Game <| fromData info model.key
          , Cmd.none
          )
        x -> Debug.todo <| "Unexpected command: " ++ Debug.toString x
    UrlChange _ -> (Conn model, Cmd.none)
    x -> Debug.todo <| "Unexpected message: " ++ Debug.toString x

fromData : API.RoomData -> Key -> GameRec
fromData data key = 
  { draft         = ""
  , messages      = []
  , players       = data.rdPlayers
  , roomId        = 0
  , key           = key
  , selectedBid   = Nothing
  , currentBid    = Nothing
  , playerSitting = False
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
      (List.map (text << .pUserName) model.players)
  , column 
      [ width fill
      , height fill
      , padding 10
      , B.width 1
      ] 
      (msgElems model)
  , row [alignBottom]
      [ I.text 
          [ onEnter <| Send (API.CSay model.draft)
          ] 
          { label = I.labelHidden "Send a message.."
          , onChange = Draft
          , placeholder = Just (I.placeholder [] <| text "Send a message..")
          , text = model.draft
          }
      , I.button 
          [ alignRight
          ]
          { label = text "Send"
          , onPress = Just <| Send (API.CSay model.draft)
          }
      ]
  ]

onEnter : msg -> Attribute msg
onEnter msg =
    Element.htmlAttribute
        (HE.on "keypress"
            (JD.field "key" JD.string
                |> JD.andThen
                    (\key ->
                        if key == "Enter" then
                            JD.succeed msg
                        else
                            JD.fail "Not the enter key"
                    )
            )
        )

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
      { onPress = Nothing
      , label = text "Call Bluff"
      }
  , bidTable
  ]

buttonStyle : List (Attribute msg)
buttonStyle =
  [ width fill
  , height fill
  , F.center
  ]

tableStyle : List (Attribute msg)
tableStyle =
  [ width fill
  , height fill
  , F.center
  , spaceEvenly
  ]

bidTable : Element Msg
bidTable = row tableStyle
  [ suitColumn "1x"
  , suitColumn "2x"
  , doubleSuitColumn "2x+2x"
  , suitColumn "3x"
  , straightColumn "Straight"
  , doubleSuitColumn "3x+2x"
  , flushColumn "Flush"
  , suitColumn "4x"
  , straightFlushColumn
  ]

suitColumn : String -> Element Msg
suitColumn header = column [] <| L.append [text header] suitColumn1

suitColumn1 : List (Element Msg)
suitColumn1 = 
  let
    f i = 
      I.button buttonStyle
        { onPress = Nothing
        , label   = text <| indexToRank i
        }
  in 
    L.map f <| L.range 14 2

doubleSuitColumn : String -> Element Msg
doubleSuitColumn header = column [] 
  [ text header
  , row [spaceEvenly] 
      [ column [] suitColumn1 
      , column [] suitColumn1
      ]
  ]

straightColumn : String -> Element Msg
straightColumn header = column [] []

flushColumn : String -> Element Msg
flushColumn header = column [] []

straightFlushColumn : Element Msg
straightFlushColumn = column [] []

tablePlayer : Int -> GameRec -> Element Msg
tablePlayer idx model =
  let 
    ang = 2*pi*(toFloat idx)/seatCount
    style = 
       [ moveRight (500 * (sin ang))
       , moveDown (250 * (cos ang))
       , centerX
       , centerY
       ]
    styleGreyed = L.append style [F.color color.lightGrey]
  in 
    case find (\x -> x.pSeat == Just idx) model.players of
      Just player ->
        column
          style
          [ text <| player.pUserName
          ]
      _ -> 
        if model.playerSitting
          then
            column styleGreyed [text "Sit"]
          else
            I.button 
              style
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
  <| JE.encode 0
  <| API.jsonEncClientMsg msg

addMessage : GameRec -> String -> GameRec
addMessage model msg = 
  { model | messages = model.messages ++ [msg] }

indexToRank : Int -> String
indexToRank x = 
  case x of
    10 -> "T"
    11 -> "J"
    12 -> "Q"
    13 -> "K"
    14 -> "A"
    y  -> S.fromInt y


