module Main where

import Prelude

import Data.Maybe (Maybe(..))
import Data.List (List)
import Data.Lens (Lens', (.~))
import Data.Lens.Getter ((^.))
import Data.Lens.Record (prop)
import Data.Symbol (SProxy(..))

import Effect (Effect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.VDom.Driver (runUI)

import Web.HTML as WH
import WebSocket as WS

import Undefined

main :: Effect Unit
main = 
  do
    win <- WH.window
    ws <- WS.socket "ws://localhost:8080/socket"
    HA.runHalogenAff do
      body <- HA.awaitBody
      runUI rootComponent ws body

data Player

type AppState =
  { socket :: WS.WebSocket
  , view   :: View
  }

data View
  = MainView 
  | JoinView
  | GameView
    { players :: List Player
    }

_view :: forall r. Lens' { view :: View | r } View
_view = prop (SProxy :: SProxy "view")

data Action 
  = CreateRoom

rootComponent :: forall query output m. H.Component HH.HTML query WS.WebSocket output m
rootComponent =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval H.defaultEval { handleAction = handleAction }
    }

initialState :: WS.WebSocket -> AppState
initialState sock =
  { socket: sock
  , view: MainView
  }

render :: forall b. AppState -> HH.HTML b Action
render state =
  case state ^. _view of
    MainView -> HH.div_
      [ HH.button [HE.onClick \_ -> Just CreateRoom] [HH.text "Create Room"]
      ]
    JoinView -> HH.text "Joining"
    GameView _ -> HH.text "Joined!"

handleAction :: forall m output. Action -> H.HalogenM AppState Action () output m Unit
handleAction CreateRoom =
  do
    H.modify_ $ _view .~ JoinView
    st <- H.get
    --WS.send st.socket undefined
    pure unit

