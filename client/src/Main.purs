module Main where

import Prelude

import Effect (Effect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.VDom.Driver (runUI)

import Web.HTML as WH

main :: Effect Unit
main = 
  do
    win <- WH.window
    HA.runHalogenAff do
      body <- HA.awaitBody
      runUI component unit body

data Player

data State 
  = MainView 
  | GameView
    { players :: Player
    }

data Action = Increment | Decrement

component :: forall query input output m. H.Component HH.HTML query input output m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval H.defaultEval { handleAction = handleAction }
    }

initialState :: forall a. a -> State
initialState _ = MainView

render :: forall b. State -> HH.HTML b Action
render state =
  HH.div_
    [ HH.text "Hello world!"
    ]

handleAction :: forall m output. Action -> H.HalogenM State Action () output m Unit
handleAction = const $ pure unit

