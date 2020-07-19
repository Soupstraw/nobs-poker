{-# LANGUAGE OverloadedStrings #-}

module Html.Index (gameHtml) where

import Control.Monad

import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A

gameHtml :: Html
gameHtml = docTypeHtml $ do
  H.head $ do
    H.title "NoBS Poker"
  body $ do
    mempty

