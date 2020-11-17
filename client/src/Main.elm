port module Main exposing (..)

import Browser
import List as L
import Element exposing (..)
import Element.Input as I
import Element.Border as B
import Element.Font as F
import String as S

port sendMessage : String -> Cmd msg
port messageReceiver : (String -> msg) -> Sub msg

seatCount = 10

main = Element.layout [] view

type alias Model = 
  { raiseAmt : Int
  , minRaise : Int
  }

type Msg 
  = Fold 
  | Call 
  | Raise Int
  | Recv String
  | Send String
  | SetBet String

init =
  { raiseAmt = 0
  , minRaise = 10
  }

update : Msg -> Model -> Model
update msg model =
  case msg of
    Fold     -> model
    Call     -> model
    Raise _  -> model
    Recv x   -> model
    Send x   -> model
    SetBet x -> case String.toInt x of
      Just amt -> { model |  raiseAmt = amt }
      Nothing  -> model

subscriptions : Model -> Sub Msg
subscriptions model =
  messageReceiver Recv

view = row 
  [ width fill
  , height fill
  ]
  [ column 
      [ width <| fillPortion 3
      , height fill
      ]
      [ gameView
      , inputRow
      ]
  , chatView
  ]

gameView = el 
  ( [ height <| fillPortion 3
    , width fill
    ] ++ seats
  ) <|
  column 
    [ centerX
    , centerY
    ] 
    [ text "$0"
    ]

chatView = column
  [ width fill 
  , height fill
  , B.width 1
  ]
  [ column 
      [ width fill
      , height fill
      , padding 10
      ] 
      [ text "Player: Hello world!"
      , text "World: Hello to you too!"
      ]
  , I.text [alignBottom] 
      { label = I.labelHidden "Send a message.."
      , onChange = Send
      , placeholder = Just (I.placeholder [] <| text "Send a message..")
      , text = ""
      }
  ]

seats = 
  let
    f i = let ang = 2*pi*i/seatCount
      in inFront <| tablePlayer (500 * (sin ang)) (250 * (cos ang))
  in L.map (f << toFloat) <| L.range 1 seatCount

inputRow = row 
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
      , label = text "Fold"
      }
  , I.button buttonStyle
      { onPress = Nothing
      , label = text "Call"
      }
  , column [width fill, height fill]
      [ I.text [alignTop] 
          { label = I.labelHidden "Raise amount"
          , onChange = SetBet
          , placeholder = Nothing
          , text = ""
          }
      , I.button buttonStyle
          { onPress = Nothing
          , label = text "Raise"
          }
      ]
  ]

buttonStyle =
  [ width fill
  , height fill
  , F.center
  ]

tablePlayer offset_x offset_y = column
  (playerStyle ++ [moveRight offset_x, moveDown offset_y])
  [ text "Player"
  , text "$1337"
  ]

playerStyle =
  [ centerX
  , centerY
  ]

