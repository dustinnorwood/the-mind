{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
module Backend where

import Common.Route
import Obelisk.Backend
import Data.Dependent.Sum (DSum (..))
import Data.Functor.Identity
import Control.Concurrent
import qualified Network.WebSockets as WS
import Network.WebSockets.Snap

import qualified Backend.Examples.WebSocketChat.Server as WebSocketChat
import qualified Common.Examples.WebSocketChat.Message as WebSocketChat

backend :: Backend BackendRoute FrontendRoute
backend = Backend
  { _backend_run = \serve -> do
      webSocketChatState <- newMVar (WebSocketChat.newServerState @WS.Connection)
      serve $ \case
        BackendRoute_Missing :=> Identity () -> return ()
        BackendRoute_WebSocketChat :=> Identity () -> do
          runWebSocketsSnap (WebSocketChat.application webSocketChatState)

  , _backend_routeEncoder = backendRouteEncoder
  }
