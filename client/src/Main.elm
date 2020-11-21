port module Main exposing (..)

import Browser exposing (..)
import List as L
import Element exposing (..)
import Element.Input as I
import Element.Border as B
import Element.Font as F
import String as S
import Url exposing (Url)
import Url.Parser as U
import Url.Parser exposing ((</>))
import NoBSAPI as API
import Json.Encode exposing (encode)
import Json.Decode exposing (decodeString)

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
  { raiseAmt    : Int
  , minRaise    : Int
  , messages    : List String
  , draft       : String
  , roomId      : Int
  , playerNames : List String
  }

type alias MenuRec =
  {
  }

type Model 
  = Game GameRec
  | MenuModel MenuRec
  | JoinModel

type Msg 
  = Fold 
  | Call 
  | Raise
  | Send String
  | Recv String
  | Draft String
  | SetBet String
  | UrlChange Url
  | UrlReq UrlRequest
  | Connected String
  | Sit Int

init : a -> b -> c -> (Model, Cmd msg)
init _ _ _ =
  ( Game 
      { raiseAmt = 0
      , minRaise = 10
      , messages = []
      , draft = ""
      , roomId = 0
      , playerNames = []
      }
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
    _ -> (mdl, Cmd.none)

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
    Recv x   -> 
      ( Game <| addMessage model x
      , Cmd.none
      )
    Send x   -> 
      ( Game { model | draft = ""}
      , sendMessage <| API.CSay x
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
    Connected x -> case Url.fromString x of
      Just url -> 
        case U.parse (U.s "room" </> U.string) url of
          Just roomId -> 
            ( Game model
            , sendMessage <| API.CJoin roomId
            )
          Nothing     -> 
            ( Game 
                <| addMessage model 
                <| "Could not get room ID from the url: " ++ x
            , Cmd.none
            )
      Nothing  -> (Game model, Cmd.none)
    Sit x -> 
      ( Game model
      , sendMessage <| API.CSit <| x
      )
    _ -> (Game model, Cmd.none)

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
    _ -> none

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
      (List.map text model.playerNames)
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
          , onPress = Just <| Send model.draft
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

