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
      (S2CRoomJoined rs) -> Just rs
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
      S2CRoomJoined rs -> "Room joined: " <> _rsName rs
      S2CRoomUpdate rs -> "Room update: " <> _rsName rs
      S2CGameUpdate gs -> T.pack $ show gs

loginWidget
  :: ( DomBuilder t m
     , MonadFix m
     , PostBuild t m
     , PerformEvent t m
     , Prerender js m
     )
  => m (Event t C2S)
loginWidget = elClass "div" "login-widget" $ do
  rec
    el "h1" $ text "The Mind"
    (tUser, tName, tPass, createRoom, joinRoom) <- elClass "div" "menu" $ do
      tu <- inputElement $ def
        & inputElementConfig_setValue .~ never
        & inputElementConfig_elementConfig . elementConfig_initialAttributes .~
          ("placeholder" =: "Enter Username")
      tn <- inputElement $ def
        & inputElementConfig_setValue .~ fmap (const "") eSubmit
        & inputElementConfig_elementConfig . elementConfig_initialAttributes .~
          ("placeholder" =: "Enter room name")
      tp <- inputElement $ def
        & inputElementConfig_setValue .~ fmap (const "") eSubmit
        & inputElementConfig_elementConfig . elementConfig_initialAttributes .~
          ("placeholder" =: "Enter password")
      el "br" blank
      doFocus tUser
      cr <- button "Create room"
      jr <- button "Join room"
      pure (tu, tn, tp, cr, jr)
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
  Just gs -> do
    gameEv <- gameWidget s2cEv rn gs
    chatEv <- chatWidget s2cEv
    pure $ leftmost [gameEv, chatEv]
  Nothing -> do
    let gameStartedEv = fmapMaybe gameStartEv s2cEv
    roomOrGame <- widgetHold (roomWidget s2cEv rs) (ffor gameStartedEv (gameWidget s2cEv rn))
    chatEv <- chatWidget s2cEv
    pure $ leftmost  [switch $ current roomOrGame, chatEv]
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
roomWidget s2cEv rs = elClass "div" "menu" $ do
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

elCard'
  :: ( DomBuilder t m
     , MonadHold t m
     , MonadFix m
     , PostBuild t m
     , PerformEvent t m
     , Prerender js m
     )
  => Bool -> Dynamic t Text -> m ()
elCard' primary cardDyn = do
  let width = if primary then "100px; " else "60px; "
      height = if primary then "140px; " else "84px; "
      margin = if primary then "5px; " else "3px; "
      elType = if primary then "h1" else "h2"
      styling = T.concat [ "width: " <> width
                         , "height: " <> height
                         , "margin: " <> margin
                         , "background-color: #CC4040; "
                         , "border-radius: 5px; "
                         , "border-width: 2px; "
                         , "border-color: #000000; "
                         , "display: inline-block; "
                         ]
  elAttr elType ("style" =: styling) $
    dynText cardDyn

elCard
  :: ( DomBuilder t m
     , MonadHold t m
     , MonadFix m
     , PostBuild t m
     , PerformEvent t m
     , Prerender js m
     )
  => Bool -> Dynamic t Int -> m ()
elCard primary = elCard' primary . fmap (T.pack . show)

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
    action <- elClass "div" "game-wrapper" $ do
      elClass "div" "game-state-wrapper" $ do
        void $ elClass "div" "room-name" $ text $ "Room: " <> rn
        void $ elClass "div" "stats-wrapper" $ do
          elClass "div" "stat-wrapper" $ do
            text "Player: "
            dynText $ _myName <$> gameDyn
          elClass "div" "stat-wrapper" $ do
            text "Level: "
            dynText $ T.pack . show . _myLevel <$> gameDyn
          elClass "div" "stat-wrapper" $ do
            text "Lives: "
            dynText $ T.pack . show . _myLives <$> gameDyn
          elClass "div" "stat-wrapper" $ do
            text "Throwing Stars: "
            dynText $ T.pack . show . _myStars <$> gameDyn
          elClass "div" "cards-wrapper" $ do
            text "Played Cards"
            let cardsDyn = _myLastPlayedCard <$> gameDyn
                nextCardDyn = fromMaybe 0 . listToMaybe <$> cardsDyn
                restCardsDyn = flip fmap cardsDyn (\case
                  [] -> []
                  (_:xs) -> xs)
            el "div" $ do
              elCard True nextCardDyn
              simpleList restCardsDyn $ elCard False
        let playersDyn = _myTeam <$> gameDyn
        void $ el "div" $ do
          listWithKey playersDyn $ \name playerStateDyn -> do
            elAttr "div" ("style" =: "margin: 3px; border-color: white; border-width: 1px;") $ do
              elClass "div" "player-name" $ do
                text $ name <> ": "
                dynText $ T.pack . show . _psNumCards <$> playerStateDyn
                text " card"
                dynText $ bool "s" "" . (== 1) . _psNumCards <$> playerStateDyn
                text " left"
                elAttr "b" ("class" =: "player-vote" <> "style" =: "color: #00FF00; font-size: 36px; display: inline-block;") $
                  dynText $ bool "" "*" . _psHasVoted <$> playerStateDyn
                let topCardDyn = flip fmap playerStateDyn $ \ps -> do
                      case _psTopCard ps of
                        Just t -> ", Top card: " <> (T.pack $ show t)
                        Nothing -> ""
                dynText topCardDyn
        let cardsDyn = _myCards <$> gameDyn
            nextCardDyn = listToMaybe <$> cardsDyn
            restCardsDyn = flip fmap cardsDyn (\case
              [] -> []
              (_:xs) -> xs)
            mkHidden False = "hidden" =: ""
            mkHidden True = mempty
            isPlayer = fmap isJust $ M.lookup <$> (_myName <$> gameDyn) <*> (_myTeam <$> gameDyn) 
        el "div" $
          fmap (gate (current isPlayer)) . elDynAttr "div" (mkHidden <$> isPlayer) $ do
          void $ elClass "div" "player-cards-wrapper" $ do
            text "My Cards"
            el "div" $ do
              elCard' True $ maybe "Done!" (T.pack . show) <$> nextCardDyn
              simpleList restCardsDyn $ elCard False
          let levelComplete = (==0) . _myRemainingCards <$> gameDyn
              levelIncomplete = not <$> levelComplete
              levelCompleteHide = mkHidden <$> levelComplete
              levelIncompleteHide = mkHidden <$> levelIncomplete
              hasCardsLeft = isJust <$> nextCardDyn
              cardsLeft = mkHidden <$> hasCardsLeft
          el "div" $ do
            ongoingLevel <- elDynAttr "div" levelIncompleteHide $ do
              playCard <- elDynAttr "div" cardsLeft $ button "Play Card"
              voteStar <- el "div" $ button "Throw Star"
              pure $ leftmost [ gate (current hasCardsLeft) (C2SPlayCard <$ playCard)
                              , C2SVoteStar True <$ voteStar
                              ]
            finishedLevel <- elDynAttr "div" levelCompleteHide $
              (C2SNextLevel <$) <$> button "Next Level"
            pure $ leftmost [ gate (current levelIncomplete) ongoingLevel
                            , gate (current levelComplete) finishedLevel
                            ]
  pure action
  where isGameStateChange = \case
          S2CGameUpdate gs -> Just gs
          _ -> Nothing
        initialGameState = MyGameState "Dustin" 4 2 1 [] 9 [27, 35, 36, 41] $ M.fromList
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
chatWidget s2cEv = elAttr "div" ("style" =: "position: fixed; height: 100%; right: 0%;") $ do
  rec
    let eRecRespTxt = fmapMaybe showMsg s2cEv
    receivedMessages <- foldDyn (\m ms -> Prelude.reverse $ Prelude.take 20 (m:Prelude.reverse ms)) [] eRecRespTxt
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
    showMsg :: S2C -> Maybe Text
    showMsg = \case
      (S2Cbroadcast txt) -> Just txt
      _ -> Nothing

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
