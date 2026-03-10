{-# LANGUAGE OverloadedStrings #-}
module Frontend (frontend) where

import Reflex.Dom
import Frontend.Head (pageHead)
import Frontend.Game (app)

frontend :: IO ()
frontend = mainWidgetWithHead pageHead $ app
