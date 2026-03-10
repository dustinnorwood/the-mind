{-# LANGUAGE OverloadedStrings #-}
module Frontend (frontend) where

import qualified Data.Text as T
import System.Environment (lookupEnv)
import Reflex.Dom
import Frontend.Head (pageHead)
import Frontend.Game (app)

frontend :: IO ()
frontend = do
  mWsUrl <- lookupEnv "GAME_WS_URL"
  let wsUrl = maybe "ws://localhost:8000" T.pack mWsUrl
  mainWidgetWithHead pageHead $ app wsUrl
