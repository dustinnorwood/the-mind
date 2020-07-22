{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecursiveDo       #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Frontend.Examples.WebSocketChat.Main where

import qualified Data.Aeson as Aeson
import           Data.Bool (bool)
import           Data.ByteString    as B
import           Data.ByteString.Lazy (toStrict, fromStrict)
import           Data.Functor.Sum
import           Data.List.NonEmpty
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Maybe
import           Data.Monoid        ((<>))
import qualified Data.Text          as T
import           Data.Text (Text)
import           GHCJS.DOM.HTMLElement       (focus)
import           Language.Javascript.JSaddle
import           Obelisk.Route
import           Reflex
import           Reflex.Dom
import           Control.Monad.Fix (MonadFix)
import           Control.Monad      (void)
import           Text.URI

--------------------------------------------------------------------------------
import           Common.Examples.WebSocketChat.Message
import           Common.Route
--------------------------------------------------------------------------------

-- TODO
--  - factor out the performEvents (see keyboard -example)
--  - factor out the message forming
--  - factor out the textInput-button combos
--  - add close connection button and associated message handling
app
  :: ( DomBuilder t m
     , MonadFix m
     , MonadHold t m
     , PostBuild t m
     , PerformEvent t m
     , TriggerEvent t m
     , Prerender js m
     )
  => Maybe Text
  -> m ()
app r = do
  rec
    msgEvDyn <- widgetHold loginWidget (ffor loggedInEv (startGameWidget msgRecEv))
    let
      msgSendEv = switch (current msgEvDyn)
      msgRecEv = fmapMaybe decodeOneMsg wsRespEv
      loggedInEv = fmapMaybe loginEv msgRecEv
    wsRespEv <- prerender (return never) $ do
      case checkEncoder backendRouteEncoder of
        Left err -> do
          el "div" $ text err
          return never
        Right encoder -> do
          let wsPath = fst $ encode encoder $ InL BackendRoute_WebSocketChat :/ ()
              sendEv = fmap ((:[]) . toStrict . Aeson.encode) msgSendEv
          let mUri = do
                uri' <- mkURI =<< r
                pathPiece <- nonEmpty =<< mapM mkPathPiece wsPath
                wsScheme <- case uriScheme uri' of
                  rtextScheme | rtextScheme == mkScheme "https" -> mkScheme "wss"
                  rtextScheme | rtextScheme == mkScheme "http" -> mkScheme "ws"
                  _ -> Nothing
                return $ uri'
                  { uriPath = Just (False, pathPiece)
                  , uriScheme = Just wsScheme
                  }
          case mUri of
            Nothing -> return never
            Just uri -> do
              ws <- webSocket (render uri) $ def & webSocketConfig_send .~ sendEv
              return (_webSocket_recv ws)
  blank
  where
    loginEv = \case
      (S2CRoomUpdate rs) -> Just rs
      _ -> Nothing
    gameStartEv = \case
      (S2CGameUpdate gs) -> Just gs
      _ -> Nothing

    decodeOneMsg :: B.ByteString -> Maybe S2C
    decodeOneMsg = Aeson.decode . fromStrict

    showMsg :: S2C -> Text
    showMsg = \case
      (S2Cbroadcast txt) -> txt
      (S2Cwelcome txt)  -> "Welcome! Users: " <> txt
      S2Cuserexists     -> "User already exists"
      S2Cnameproblem    -> "Name cannot contain punctuation or "
      S2CRoomDoesntExist rc -> T.pack $ show rc
      S2CRoomAlreadyExists t -> t
      S2CRoomUpdate rs -> "Room entered: " <> _rsName rs
      S2CGameUpdate gs -> T.pack $ show gs

loginWidget
  :: ( DomBuilder t m
     , MonadFix m
     , PostBuild t m
     , PerformEvent t m
     , Prerender js m
     )
  => m (Event t C2S)
loginWidget = el "div" $ do
  rec
    tUser <- inputElement $ def
      & inputElementConfig_setValue .~ fmap (const "") eSubmit
      & inputElementConfig_elementConfig . elementConfig_initialAttributes .~
        ("placeholder" =: "Enter Username")
    tName <- inputElement $ def
      & inputElementConfig_setValue .~ fmap (const "") eSubmit
      & inputElementConfig_elementConfig . elementConfig_initialAttributes .~
        ("placeholder" =: "Enter room name")
    tPass <- inputElement $ def
      & inputElementConfig_setValue .~ fmap (const "") eSubmit
      & inputElementConfig_elementConfig . elementConfig_initialAttributes .~
        ("placeholder" =: "Enter password")
    doFocus tUser
    createRoom <- button "Create room"
    joinRoom <- button "Join room"
    -- Clean the name a bit (todo, clean more):
    let ev f = tag ((\a b c -> f a (RoomConfig b c)) <$> (fmap T.strip . current $ value tUser)
                      <*> (fmap T.strip . current $ value tName)
                      <*> (fmap T.strip . current $ value tPass))
        eCreate = ev C2SCreateRoom createRoom
        eJoin = ev C2SJoinRoom joinRoom
        eSubmit = leftmost [eCreate, eJoin]
  return eSubmit

startGameWidget
  :: ( DomBuilder t m
     , MonadFix m
     , MonadHold t m
     , PostBuild t m
     , PerformEvent t m
     , Prerender js m
     )
  => Event t S2C -> RoomSummary -> m (Event t C2S)
startGameWidget s2cEv rs@(RoomSummary rn ps mGS) = case mGS of
  Just gs -> gameWidget s2cEv rn gs
  Nothing -> do
    let gameStartedEv = fmapMaybe gameStartEv s2cEv
    roomOrGame <- widgetHold (roomWidget s2cEv rs) (ffor gameStartedEv (gameWidget s2cEv rn))
    pure . switch $ current roomOrGame
  where
    gameStartEv = \case
      (S2CGameUpdate gs) -> Just gs
      _ -> Nothing

roomWidget
  :: ( DomBuilder t m
     , MonadHold t m
     , MonadFix m
     , PostBuild t m
     , PerformEvent t m
     , Prerender js m
     )
  => Event t S2C -> RoomSummary -> m (Event t C2S)
roomWidget s2cEv rs = el "div" $ do
  rec
    let ruEv = fmapMaybe isRoomUpdate s2cEv
    roomDyn <- holdDyn rs ruEv
    startGameEv <- elClass "div" "game-wrapper" $ do
      elClass "div" "game-state-wrapper" $ do
        void $ elClass "div" "room-name" $ text $ "Room: " <> _rsName rs
        void $ elClass "div" "player-stats-wrapper" $ do
          let playersDyn = _rsPlayers <$> roomDyn
          simpleList playersDyn $ el "ul" . dynText
        startGame <- button "Start Game"
        pure $ C2SStartGame <$ startGame
  pure startGameEv
  where isRoomUpdate = \case
          S2CRoomUpdate rs -> Just rs
          _ -> Nothing

gameWidget
  :: ( DomBuilder t m
     , MonadHold t m
     , MonadFix m
     , PostBuild t m
     , PerformEvent t m
     , Prerender js m
     )
  => Event t S2C -> Text -> MyGameState -> m (Event t C2S)
gameWidget s2cEv rn initGS = el "div" $ do
  rec
    let gsEv = fmapMaybe isGameStateChange s2cEv
    gameDyn <- holdDyn initGS gsEv
    (playCardEv, voteStarEv) <- elClass "div" "game-wrapper" $ do
      elClass "div" "game-state-wrapper" $ do
        void $ elClass "div" "room-name" $ text $ "Room: " <> rn
        void $ elClass "div" "stats-wrapper" $ do
          elClass "div" "stat-wrapper" $ do
            elClass "div" "stat-name" $ text "Player: "
            elClass "div" "stat-value" $ dynText $ _myName <$> gameDyn
          elClass "div" "stat-wrapper" $ do
            elClass "div" "stat-name" $ text "Level: "
            elClass "div" "stat-value" $ dynText $ T.pack . show . _myLevel <$> gameDyn
          elClass "div" "stat-wrapper" $ do
            elClass "div" "stat-name" $ text "Lives: "
            elClass "div" "stat-value" $ dynText $ T.pack . show . _myLives <$> gameDyn
          elClass "div" "stat-wrapper" $ do
            elClass "div" "stat-name" $ text "Throwing Stars: "
            elClass "div" "stat-value" $ dynText $ T.pack . show . _myStars <$> gameDyn
          elClass "div" "stat-wrapper" $ do
            elClass "div" "stat-name" $ text "Last Card Played: "
            elClass "div" "stat-value" $ dynText $ maybe "Nothing" (T.pack . show) . _myLastPlayedCard <$> gameDyn
        void $ elClass "div" "player-stats-wrapper" $ do
          let playersDyn = _myTeam <$> gameDyn
          listWithKey playersDyn $ \name playerStateDyn -> do
            elClass "div" "player-wrapper" $ do
              elClass "div" "player-name" $ text name
              elClass "div" "player-stat" $ do
                elClass "div" "player-remaining-cards" $ do
                  text "Cards: "
                  dynText $ T.pack . show . _psNumCards <$> playerStateDyn
                elClass "div" "player-top-card-and-vote" $ do
                  let topCardDyn = flip fmap playerStateDyn $ \ps -> do
                        case _psTopCard ps of
                          Just t -> "Top card: " <> (T.pack $ show t)
                          Nothing -> ""
                  dynText topCardDyn
                  dynText $ bool "" "*" . _psHasVoted <$> playerStateDyn
        void $ elClass "div" "player-cards-wrapper" $ do
          text "My Cards"
          let cardsDyn = _myCards <$> gameDyn
              nextCardDyn = listToMaybe <$> cardsDyn
              restCardsDyn = flip fmap cardsDyn (\case
                [] -> []
                (_:xs) -> xs)
          elClass "div" "player-next-card" $ dynText $ maybe "" (T.pack . show) <$> nextCardDyn
          elClass "ul" "player-rest-cards" $ simpleList restCardsDyn (el "li" . dynText . fmap (T.pack . show))
        playCard <- button "Play Card"
        voteStar <- button "Throw Star"
        pure (C2SPlayCard <$ playCard, C2SVoteStar True <$ voteStar)
    chatEv <- chatWidget s2cEv
  return $ leftmost [playCardEv, voteStarEv, chatEv]
  where isGameStateChange = \case
          S2CGameUpdate gs -> Just gs
          _ -> Nothing
        initialGameState = MyGameState "Dustin" 4 2 1 Nothing [27, 35, 36, 41] $ M.fromList
          [ ("Dustin", PlayerSummary 4 True Nothing)
          , ("Dan", PlayerSummary 3 False (Just 7))
          , ("Jonathan", PlayerSummary 2 True (Just 34))
          ]

chatWidget
  :: ( DomBuilder t m
     , MonadHold t m
     , MonadFix m
     , PostBuild t m
     , PerformEvent t m
     , Prerender js m
     )
  => Event t S2C -> m (Event t C2S)
chatWidget s2cEv = el "div" $ do
  rec
    let eRecRespTxt = showMsg <$> s2cEv
    receivedMessages <- foldDyn (\m ms -> ms ++ [m]) [] eRecRespTxt
    void $ el "div" $ do
      el "p" $ text "Chat"
      el "ul" $ simpleList receivedMessages (\m -> el "li" $ dynText m)
    t <- inputElement $ def & inputElementConfig_setValue .~ fmap (const "") newMessage
    doFocus t
    b <- button "Send"
    let newMessage = tag (current $ value t)
          $ leftmost [b, keypress Enter t]
  return $ C2SChat <$> newMessage
  where
    showMsg :: S2C -> Text
    showMsg = \case
      (S2Cbroadcast txt) -> txt
      (S2Cwelcome txt)  -> "Welcome! Users: " <> txt
      S2Cuserexists     -> "User already exists"
      S2Cnameproblem    -> "Name cannot contain punctuation or "
      S2CRoomDoesntExist rc -> T.pack $ show rc
      S2CRoomAlreadyExists t -> t
      S2CRoomUpdate r -> T.pack $ show r
      S2CGameNotStarted -> "Game not started yet!"
      S2CGameUpdate gs -> T.pack $ show gs

doFocus
  :: ( DomBuilder t m
     , PostBuild t m
     , PerformEvent t m
     , Prerender js m
     )
  => InputElement EventResult (DomBuilderSpace m) t
  -> m ()
doFocus ie = prerender (return ()) $ do
  pb <- getPostBuild
  let h = _inputElement_raw ie
  performEvent_ (fmap (liftJSM . const (focus h)) pb)
  return ()
