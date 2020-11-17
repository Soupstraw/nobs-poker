port module Main exposing (..)

import Browser exposing (..)
import List as L
import Element exposing (..)
import Element.Input as I
import Element.Border as B
import Element.Font as F
import String as S
import Url exposing (Url)


port sendMessage : String -> Cmd msg
port messageReceiver : (String -> msg) -> Sub msg

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

type alias Model = 
  { raiseAmt : Int
  , minRaise : Int
  , messages : List String
  , draft    : String
  }

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

init _ _ _ =
  ({ raiseAmt = 0
  , minRaise = 10
  , messages = []
  , draft = ""
  }, Cmd.none)

document : Model -> Document Msg
document model = 
  { title = "Udupoker"
  , body = [Element.layout [] (view model)]
  }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Draft x  -> 
      ( {model | draft = x}
      , Cmd.none
      )
    Recv x   -> ({ model | messages = model.messages ++ [x]}, Cmd.none)
    Send x   -> ({ model | draft = ""}, sendMessage x)
    SetBet x -> case String.toInt x of
      Just amt -> ({ model |  raiseAmt = amt }, Cmd.none)
      Nothing  -> if x == "" 
                    then ({model | raiseAmt = 0}, Cmd.none)
                    else (model, Cmd.none)
    _ -> (model, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions model =
  messageReceiver Recv

view : Model -> Element Msg
view model = row 
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

gameView model = el 
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

chatView model = column
  [ width fill 
  , height fill
  , B.width 1
  ]
  [ column 
      [ width fill
      , height fill
      , padding 10
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

msgElems : Model -> List (Element Msg)
msgElems model = L.map text model.messages

seats = 
  let
    f i = let ang = 2*pi*i/seatCount
      in inFront <| tablePlayer (500 * (sin ang)) (250 * (cos ang))
  in L.map (f << toFloat) <| L.range 1 seatCount

inputRow : Model -> Element Msg
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

