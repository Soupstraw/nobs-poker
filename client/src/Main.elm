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

type Model 
  = GameModel GameRec

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

init : a -> b -> c -> (Model, Cmd msg)
init _ _ _ =
  ( GameModel 
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
update msg (GameModel model) =
  case msg of
    Draft x  -> 
      ( GameModel { model | draft = x }
      , Cmd.none
      )
    Raise    -> 
      ( GameModel { model | raiseAmt = 0}
      , sendMessage model <| "/raise " ++ String.fromInt model.raiseAmt
      )
    Call     ->
      ( GameModel model
      , sendMessage model "/call"
      )
    Fold     ->
      ( GameModel model
      , sendMessage model "/fold"
      )
    Recv x   -> 
      ( GameModel <| addMessage model x
      , Cmd.none
      )
    Send x   -> 
      ( GameModel { model | draft = ""}
      , sendMessage model x
      )
    SetBet x -> case String.toInt x of
      Just amt -> 
        ( GameModel { model |  raiseAmt = amt }
        , Cmd.none
        )
      Nothing  -> if x == "" 
                    then 
                      ( GameModel {model | raiseAmt = 0}
                      , Cmd.none
                      )
                    else 
                      ( GameModel model
                      , Cmd.none
                      )
    Connected x -> case Url.fromString x of
      Just url -> 
        case U.parse (U.s "room" </> U.string) url of
          Just roomId -> 
            ( GameModel model
            , sendMessage model <| "/join " ++ roomId
            )
          Nothing     -> 
            ( GameModel 
                <| addMessage model 
                <| "Could not get room ID from the url: " ++ x
            , Cmd.none
            )
      Nothing  -> (GameModel model, Cmd.none)
    _ -> (GameModel model, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions model = Sub.batch
  [ port_messageReceiver Recv
  , port_onConnect Connected
  ]

view : Model -> Element Msg
view (GameModel model) = row 
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

gameView : GameRec -> Element msg
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

seats : GameRec -> List (Attribute msg)
seats model = 
  let
    f i = let ang = 2*pi*i/seatCount
      in inFront <| tablePlayer (500 * (sin ang)) (250 * (cos ang)) model
  in L.map (f << toFloat) <| L.range 1 seatCount

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

tablePlayer : Float -> Float -> GameRec -> Element msg
tablePlayer offset_x offset_y model = column
  (playerStyle ++ [moveRight offset_x, moveDown offset_y])
  [ text "Player"
  , text "$1337"
  ]

playerStyle : List (Attribute msg)
playerStyle =
  [ centerX
  , centerY
  ]

sendMessage : GameRec -> String -> Cmd Msg
sendMessage model msg =
  port_sendMessage <| (String.fromInt model.roomId) ++ " " ++ msg

addMessage : GameRec -> String -> GameRec
addMessage model msg = 
  { model | messages = model.messages ++ [msg] }

