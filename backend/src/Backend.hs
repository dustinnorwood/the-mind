{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Backend (run) where

import Control.Concurrent (newMVar)
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Application.Static as Static
import qualified Network.Wai.Handler.WebSockets as WaiWS
import qualified Network.WebSockets as WS
import WaiAppStatic.Types (ssIndices, unsafeToPiece)

import qualified Backend.Server as Server
import qualified Common.Message as Message

run :: IO ()
run = do
  putStrLn "Starting The Mind server on port 8000..."
  wsState <- newMVar (Message.newServerState @WS.Connection)
  let wsApp = Server.application wsState
      staticApp = Static.staticApp staticSettings
      app = WaiWS.websocketsOr WS.defaultConnectionOptions wsApp staticApp
  Warp.run 8000 app

staticSettings :: Static.StaticSettings
staticSettings = settings { ssIndices = [unsafeToPiece "index.html"] }
  where settings = Static.defaultFileServerSettings "static"
