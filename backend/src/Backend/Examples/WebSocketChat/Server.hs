{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Backend.Examples.WebSocketChat.Server where

import           Control.Concurrent (MVar, modifyMVar, modifyMVar_,
                                     readMVar)
import           Control.Exception  (finally)
import           Control.Lens
import           Control.Monad      (forM_, forever)
import           Data.Aeson         (encode, decode)
import qualified Data.ByteString    as B
import           Data.ByteString.Lazy (toStrict)
import           Data.Char          (isPunctuation, isSpace)
import           Data.Foldable      (for_)
import           Data.Map.Strict    (Map)
import qualified Data.Map.Strict    as M
import           Data.Semigroup     ((<>))
import           Data.Text          (Text)
import qualified Data.Text          as T
import qualified Data.Text.IO       as T
import qualified Network.WebSockets as WS

--------------------------------------------------------------------------------
import           Common.Examples.WebSocketChat.Message
--------------------------------------------------------------------------------

send :: WS.Connection -> S2C -> IO ()
send conn s2c = do
  T.putStrLn . T.pack $ show s2c
  WS.sendTextData conn . toStrict $ encode s2c

broadcastRoomUpdate :: Text -> Maybe GameState -> ServerState WS.Connection -> IO ()
broadcastRoomUpdate roomName gs ss = do
  let roomClients = maybe M.empty _roomClients $ M.lookup roomName $ _rooms ss 
      clientNames = M.keys roomClients
  forM_ roomClients $ \c@(Client name _ conn) -> do
    let myGS = myGameState name <$> gs
    let s2c = S2CRoomUpdate $ RoomSummary roomName clientNames myGS
    T.putStrLn $ "Sending to " <> name <> ": " <> (T.pack $ show s2c)
    send conn s2c

broadcastGameUpdate :: Text -> GameState -> ServerState WS.Connection -> IO ()
broadcastGameUpdate roomName gs ss = do
  let roomClients = maybe M.empty _roomClients $ M.lookup roomName $ _rooms ss 
  forM_ roomClients $ \c@(Client name _ conn) -> do
    let s2c = S2CGameUpdate $ myGameState name gs
    T.putStrLn $ "Sending to " <> name <> ": " <> (T.pack $ show s2c)
    send conn s2c

broadcast :: Text -> S2C -> ServerState WS.Connection -> IO ()
broadcast roomName s2c ss = do
  T.putStrLn . T.pack $ show s2c
  let roomClients = maybe M.empty _roomClients $ M.lookup roomName $ _rooms ss 
  forM_ roomClients $ \(Client _ _ conn) -> send conn s2c

application :: MVar (ServerState WS.Connection) -> WS.ServerApp
application state pending = do
  conn <- WS.acceptRequest pending
  WS.forkPingThread conn 30
  lobby state conn

lobby :: MVar (ServerState WS.Connection) -> WS.Connection -> IO ()
lobby state conn = do
  msgbs <- WS.receiveData conn :: IO B.ByteString
  let msgC = decode $ WS.toLazyByteString msgbs :: Maybe C2S
  case msgC of
      Nothing           ->
          T.putStrLn "Decoded msgC is nothing..."
      Just (C2SCreateRoom pName rc) -> do
        mState <- readMVar state
        let name = _roomName rc
        case M.lookup name $ _rooms mState of
          Just _ -> do
            send conn $ S2CRoomAlreadyExists name
            lobby state conn
          Nothing -> do
            let c = Client pName name conn
            (ss', r) <- modifyMVar state $ \s ->
              let r = newRoom rc c
                  f = rooms %~ M.insert name r
                  s' = f s
                in pure (s', (s', r))
            let rs = RoomSummary name (M.keys $ _roomClients r) Nothing
            send conn (S2CRoomJoined rs)
            talk state c 
      Just (C2SJoinRoom pName rc) -> do
        T.putStrLn $ "YO!" <> pName <> T.pack (show rc)
        mState <- readMVar state
        let name = _roomName rc
        case M.lookup name $ _rooms mState of
          Nothing -> do
            send conn $ S2CRoomDoesntExist rc
            lobby state conn
          Just r | _roomConfig r == rc -> do
            let c = Client pName name conn
            ss' <- modifyMVar state $ \s ->
              let s' = addClient c name s
                in pure (s', s')
            let r' = M.lookup name $ _rooms ss'
            let mGS = _roomGameState =<< r'
            let myGS = myGameState pName <$> mGS
            let rs = RoomSummary name (maybe [] (M.keys . _roomClients) r') myGS
            send conn (S2CRoomJoined rs)
            broadcastRoomUpdate name mGS ss'
            talk state c
      Just msg -> do
        T.putStrLn $ "Got unexpected message: " <> T.pack (show msg)
        lobby state conn

talk :: MVar (ServerState WS.Connection) -> Client WS.Connection -> IO ()
talk state c@(Client user room conn) = flip finally disconnect . forever $ do
    msgbs <- WS.receiveData conn :: IO B.ByteString
    case decode $ WS.toLazyByteString msgbs of
        Nothing           ->
            T.putStrLn "Decoded msgC is nothing..."
        Just C2Sclose     ->
            undefined -- TBD
        Just (C2Sjoin nm) ->
            T.putStrLn $ "C2Sjoin should not happen here, nm =" <> nm
        Just (C2SChat txt) ->
            readMVar state >>= broadcast room (S2Cbroadcast $ user <> ": " <> txt)
        Just C2SStartGame -> do
          mr <- M.lookup room . _rooms <$> readMVar state
          case mr of
            Nothing -> send conn $ S2CRoomDoesntExist $ RoomConfig room ""
            Just r -> do
              let ps = take 6 . M.keys $ _roomClients r
              gs <- newGame ps
              st <- modifyMVar state $ \s -> do
                let s' = (rooms . at room . _Just . roomGameState ?~ gs) s
                pure (s', s')
              broadcastGameUpdate room gs st

        Just (C2SVoteStar didVote) -> do
          T.putStrLn . T.pack $ "C2SVoteStar " ++ show didVote
          mr <- M.lookup room . _rooms <$> readMVar state
          case _roomGameState <$> mr of
            Nothing -> send conn $ S2CRoomDoesntExist $ RoomConfig room ""
            Just Nothing -> send conn $ S2CGameNotStarted
            Just (Just gs) -> do
              let gs' = voteStar user gs
              st <- modifyMVar state $ \s -> do
                let s' = (rooms . at room . _Just . roomGameState ?~ gs') s
                pure (s', s')
              broadcastGameUpdate room gs' st

        Just C2SPlayCard -> do
          T.putStrLn "C2SPlayCard"
          mr <- M.lookup room . _rooms <$> readMVar state
          case _roomGameState <$> mr of
            Nothing -> send conn $ S2CRoomDoesntExist $ RoomConfig room ""
            Just Nothing -> send conn $ S2CGameNotStarted
            Just (Just gs) -> do
              let gs' = playCard user gs
              st <- modifyMVar state $ \s -> do
                let s' = (rooms . at room . _Just . roomGameState ?~ gs') s
                pure (s', s')
              broadcastGameUpdate room gs' st
        Just C2SNextLevel -> do
          T.putStrLn "C2SNextLevel"
          mr <- M.lookup room . _rooms <$> readMVar state
          case _roomGameState <$> mr of
            Nothing -> send conn $ S2CRoomDoesntExist $ RoomConfig room ""
            Just Nothing -> send conn $ S2CGameNotStarted
            Just (Just gs) -> do
              gs' <- tryNextLevel gs
              st <- modifyMVar state $ \s -> do
                let s' = (rooms . at room . _Just . roomGameState ?~ gs') s
                pure (s', s')
              broadcastGameUpdate room gs' st
  where
    disconnect = do
        -- Remove client and return new state
        s <- modifyMVar state $ \s ->
            let s' = removeClient room c s in return (s', s')
        broadcast room (S2Cbroadcast $ user <> " disconnected") s


