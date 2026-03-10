{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecursiveDo       #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Frontend.Game where

import qualified Data.Aeson as Aeson
import           Data.Bool (bool)
import qualified Data.ByteString as B
import           Data.ByteString.Lazy (toStrict, fromStrict)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Maybe
import qualified Data.Text          as T
import           Data.Text (Text)
import           Reflex
import           Reflex.Dom
import           Control.Monad.Fix (MonadFix)
import           Control.Monad      (void)

--------------------------------------------------------------------------------
import           Common.Message
--------------------------------------------------------------------------------

app
  :: ( DomBuilder t m
     , MonadFix m
     , MonadHold t m
     , PostBuild t m
     , PerformEvent t m
     , TriggerEvent t m
     , Prerender t m
     )
  => Text -> m ()
app wsUrl = do
  rec
    msgEvDyn <- widgetHold (loginWidget msgRecEv) (ffor loggedInEv (startGameWidget msgRecEv))
    let
      msgSendEv = switch (current msgEvDyn)
      msgRecEv = fmapMaybe decodeOneMsg wsRespEv
      loggedInEv = fmapMaybe loginEv msgRecEv
    wsRespEvDyn <- prerender (return never) $ do
      let sendEv = fmap ((:[]) . toStrict . Aeson.encode) msgSendEv
      ws <- webSocket wsUrl $ def & webSocketConfig_send .~ sendEv
      return (_webSocket_recv ws)
    let wsRespEv = switch (current wsRespEvDyn)
  blank
  where
    loginEv = \case
      (S2CRoomJoined rs) -> Just rs
      _ -> Nothing

    decodeOneMsg :: B.ByteString -> Maybe S2C
    decodeOneMsg = Aeson.decode . fromStrict

-- Login screen
loginWidget
  :: ( DomBuilder t m
     , MonadFix m
     , MonadHold t m
     , PostBuild t m
     , PerformEvent t m
     , Prerender t m
     )
  => Event t S2C -> m (Event t C2S)
loginWidget s2cEv = elClass "div" "login-screen" $ do
  rec
    (tUser, tName, createRoom, joinRoom) <- elClass "div" "login-card" $ do
      elClass "div" "login-title" $ text "THE MIND"
      elClass "div" "login-subtitle" $ text "A Multiplayer Card Game"

      -- Error display
      let errorEv = fmapMaybe loginError s2cEv
      errorDyn <- holdDyn Nothing (leftmost [Just <$> errorEv, Nothing <$ eSubmit])
      void $ dyn $ ffor errorDyn $ \case
        Nothing -> blank
        Just err -> elClass "div" "notification notification-error" $ text err

      tu <- elClass "div" "input-group" $ do
        elClass "label" "input-label" $ text "Username"
        inputElement $ def
          & inputElementConfig_setValue .~ never
          & inputElementConfig_elementConfig . elementConfig_initialAttributes .~
            ("placeholder" =: "Enter your name" <> "class" =: "game-input")
      tn <- elClass "div" "input-group" $ do
        elClass "label" "input-label" $ text "Room Name"
        inputElement $ def
          & inputElementConfig_setValue .~ never
          & inputElementConfig_elementConfig . elementConfig_initialAttributes .~
            ("placeholder" =: "Enter room name" <> "class" =: "game-input")
      elClass "div" "button-group" $ do
        cr <- elClass' "button" "btn btn-primary" $ text "Create Room"
        jr <- elClass' "button" "btn btn-secondary" $ text "Join Room"
        pure (tu, tn, domEvent Click (fst cr), domEvent Click (fst jr))
    let ev f = tag ((\a b -> f a b) <$> (fmap T.strip . current $ value tUser)
                      <*> (fmap T.strip . current $ value tName))
        eCreate = ev C2SCreateRoom createRoom
        eJoin = ev C2SJoinRoom joinRoom
        eSubmit = leftmost [eCreate, eJoin]
  return eSubmit
  where
    loginError :: S2C -> Maybe Text
    loginError = \case
      S2CRoomAlreadyExists name -> Just $ "Room \"" <> name <> "\" already exists"
      S2CRoomDoesntExist name   -> Just $ "Room \"" <> name <> "\" does not exist"
      S2CUsernameTaken name     -> Just $ "Username \"" <> name <> "\" is already taken in this room"
      _ -> Nothing

-- Post-login: room lobby or game
startGameWidget
  :: ( DomBuilder t m
     , MonadFix m
     , MonadHold t m
     , PostBuild t m
     , PerformEvent t m
     , Prerender t m
     )
  => Event t S2C -> RoomSummary -> m (Event t C2S)
startGameWidget s2cEv rs@(RoomSummary rn _ps mGS) =
  elClass "div" "app-layout" $ case mGS of
    Just gs -> do
      gameEv <- elClass "div" "game-area" $ gameWidget s2cEv rn gs
      chatEv <- chatWidget s2cEv
      pure $ leftmost [gameEv, chatEv]
    Nothing -> do
      let gameStartedEv = fmapMaybe gameStartEv s2cEv
      roomOrGame <- elClass "div" "game-area" $
        widgetHold (roomWidget s2cEv rs) (ffor gameStartedEv (gameWidget s2cEv rn))
      chatEv <- chatWidget s2cEv
      pure $ leftmost [switch $ current roomOrGame, chatEv]
  where
    gameStartEv = \case
      (S2CGameUpdate gs) -> Just gs
      _ -> Nothing

-- Room lobby
roomWidget
  :: ( DomBuilder t m
     , MonadHold t m
     , MonadFix m
     , PostBuild t m
     , PerformEvent t m
     , Prerender t m
     )
  => Event t S2C -> RoomSummary -> m (Event t C2S)
roomWidget s2cEv rs = elClass "div" "room-lobby" $ do
  rec
    let ruEv = fmapMaybe isRoomUpdate s2cEv
    roomDyn <- holdDyn rs ruEv
    startGameEv <- do
      elClass "div" "room-header" $ do
        elClass "h2" "room-title" $ text $ "Room: " <> _rsName rs
      elClass "div" "players-list" $ do
        elClass "h3" "section-title" $ text "Players"
        let playersDyn = _rsPlayers <$> roomDyn
        void $ simpleList playersDyn $ \pDyn ->
          elClass "div" "player-badge" $ do
            elClass "span" "player-avatar" $ dynText $ T.take 1 <$> pDyn
            dynText pDyn
      elClass "div" "room-actions" $ do
        (startEl, _) <- elClass' "button" "btn btn-primary btn-large" $ text "Start Game"
        pure $ C2SStartGame <$ domEvent Click startEl
  pure startGameEv
  where isRoomUpdate = \case
          S2CRoomUpdate rs' -> Just rs'
          _ -> Nothing

-- Card component
elCard
  :: ( DomBuilder t m
     , MonadHold t m
     , MonadFix m
     , PostBuild t m
     )
  => Text -> Dynamic t Text -> m ()
elCard cls cardDyn =
  elClass "div" ("card " <> cls) $
    elClass "span" "card-number" $ dynText cardDyn

-- Game widget
gameWidget
  :: ( DomBuilder t m
     , MonadHold t m
     , MonadFix m
     , PostBuild t m
     , PerformEvent t m
     , Prerender t m
     )
  => Event t S2C -> Text -> MyGameState -> m (Event t C2S)
gameWidget s2cEv _rn initGS = elClass "div" "game-board" $ do
  rec
    let gsEv = fmapMaybe isGameStateChange s2cEv
        wrongCardEv = fmapMaybe isWrongCard s2cEv
    gameDyn <- holdDyn initGS gsEv

    action <- do
      -- Header bar
      elClass "div" "game-header" $ do
        elClass "h1" "game-title" $ text "THE MIND"
        elClass "div" "game-stats" $ do
          elClass "div" "stat" $ do
            elClass "span" "stat-label" $ text "Level"
            elClass "span" "stat-value" $ dynText $ T.pack . show . _myLevel <$> gameDyn
          elClass "div" "stat" $ do
            elClass "span" "stat-label" $ text "Lives"
            elClass "span" "stat-value stat-lives" $ do
              let livesDyn = _myLives <$> gameDyn
              dynText $ ffor livesDyn $ \n -> T.replicate n "\x2764\xFE0F" <> T.replicate (max 0 (4 - n)) "\x1F5A4"
          elClass "div" "stat" $ do
            elClass "span" "stat-label" $ text "Stars"
            elClass "span" "stat-value stat-stars" $ do
              let starsDyn = _myStars <$> gameDyn
              dynText $ ffor starsDyn $ \n -> T.replicate n "\x2B50" <> T.replicate (max 0 (3 - n)) "\x2606"

      -- Wrong card notification
      wrongCardNotice <- holdDyn Nothing (Just <$> wrongCardEv)
      void $ dyn $ ffor wrongCardNotice $ \case
        Nothing -> blank
        Just wc -> elClass "div" "notification notification-error" $ do
          text $ _wcPlayer wc <> " played " <> T.pack (show $ _wcPlayed wc) <> "! "
          text $ "Min card was " <> T.pack (show $ _wcMinCard wc)

      -- Game over check
      let isOver = (<= 0) . _myLives <$> gameDyn
          isWon = _myGameWon <$> gameDyn

      -- Game over overlay
      void $ dyn $ ffor isOver $ \gameOver ->
        if gameOver
        then elClass "div" "overlay overlay-gameover" $ do
          elClass "h1" "overlay-title" $ text "GAME OVER"
          elClass "p" "overlay-text" $ do
            text "Reached Level "
            dynText $ T.pack . show . _myLevel <$> gameDyn
          blank
        else blank

      -- Win overlay
      void $ dyn $ ffor isWon $ \won ->
        if won
        then elClass "div" "overlay overlay-win" $ do
          elClass "h1" "overlay-title" $ text "YOU WIN!"
          elClass "p" "overlay-text" $ text "All levels completed!"
          blank
        else blank

      -- Card pile area (played cards stacked with rotation)
      dropPlayEv <- elClass "div" "pile-area" $ do
        elClass "h3" "section-title" $ text "Played Cards"
        let cardsDyn = _myLastPlayedCard <$> gameDyn
        -- Drop zone with pile of cards overlaid
        (dropZoneEl, _) <- elAttr' "div"
          ("class" =: "pile-container"
           <> "ondragover" =: "event.preventDefault()"
           <> "ondrop" =: "event.preventDefault()")
          $ do
            -- Show pile cards with rotation
            void $ dyn $ ffor cardsDyn $ \pileCards ->
              if Prelude.null pileCards
              then elClass "div" "pile-drop-zone" $ text "Drop card here"
              else do
                let indexed = Prelude.zip [0 :: Int ..] (Prelude.reverse pileCards)
                    totalCards = Prelude.length pileCards
                mapM_ (\(i, c) -> do
                  let isLatest = i == totalCards - 1
                      -- Each card gets a pseudo-random rotation based on the card value
                      rot = ((c * 7 + i * 13) `mod` 21) - 10  -- range: -10 to +10 degrees
                      xOff = ((c * 3 + i * 5) `mod` 11) - 5   -- range: -5 to +5 px
                      yOff = ((c * 11 + i * 7) `mod` 9) - 4   -- range: -4 to +4 px
                      rotStyle = "transform: rotate(" <> T.pack (show rot) <> "deg) translate("
                                 <> T.pack (show xOff) <> "px, " <> T.pack (show yOff) <> "px);"
                      cls = "card card-primary pile-card" <> if isLatest then " pile-card-latest" else ""
                  elAttr "div" ("class" =: cls <> "style" =: rotStyle) $
                    elClass "span" "card-number" $ text $ T.pack $ show c
                  ) indexed
        -- Return drop event from the drop zone
        pure $ C2SPlayCard <$ domEvent Drop dropZoneEl

      -- Team section
      elClass "div" "team-section" $ do
        elClass "h3" "section-title" $ text "Team"
        let playersDyn = _myTeam <$> gameDyn
        elClass "div" "team-grid" $ do
          void $ listWithKey playersDyn $ \name playerStateDyn ->
            elClass "div" "teammate-card" $ do
              elClass "div" "teammate-avatar" $ text $ T.take 1 name
              elClass "div" "teammate-name" $ text name
              elClass "div" "teammate-cards" $ do
                dynText $ T.pack . show . _psNumCards <$> playerStateDyn
                text " "
                dynText $ bool "cards" "card" . (== 1) . _psNumCards <$> playerStateDyn
              elClass "div" "teammate-vote" $
                dynText $ bool "" "\x2B50" . _psHasVoted <$> playerStateDyn
              void $ dyn $ ffor (_psTopCard <$> playerStateDyn) $ \case
                Nothing -> blank
                Just t -> elClass "div" "teammate-topcard" $ text $ T.pack $ show t

      -- Your hand (draggable cards)
      let cardsDyn = _myCards <$> gameDyn
          isPlayer = fmap isJust $ M.lookup <$> (_myName <$> gameDyn) <*> (_myTeam <$> gameDyn)
          mkHidden :: Bool -> Map Text Text
          mkHidden False = "style" =: "display: none"
          mkHidden True = mempty

      elDynAttr "div" (mkHidden <$> isPlayer) $ do
        elClass "div" "hand-section" $ do
          elClass "h3" "section-title" $ text "Your Hand"
          elClass "div" "hand-cards" $ do
            void $ simpleList cardsDyn $ \cDyn -> do
              let cardAttrs = "class" =: "card card-primary card-hand"
                              <> "draggable" =: "true"
              void $ elAttr' "div" cardAttrs $
                elClass "span" "card-number" $ dynText (T.pack . show <$> cDyn)
            void $ dyn $ ffor (Prelude.null <$> cardsDyn) $ \isEmpty ->
              if isEmpty
              then elClass "div" "hand-empty" $ text "No cards remaining"
              else blank

        -- Action buttons
        let levelComplete = (==0) . _myRemainingCards <$> gameDyn
            hasCards = not . Prelude.null <$> cardsDyn
            gameActive = not <$> isOver

        elClass "div" "action-buttons" $ do
          let showPlay = (&&) <$> (not <$> levelComplete) <*> gameActive
          playEv <- elDynAttr "div" (mkHidden <$> showPlay) $ do
            (playEl, _) <- elDynAttr' "button"
              (ffor hasCards $ \hc -> "class" =: ("btn btn-play" <> if hc then "" else " btn-disabled"))
              $ text "Play Card"
            (starEl, _) <- elClass' "button" "btn btn-star" $ text "Throw Star \x2B50"
            pure $ leftmost [ gate (current hasCards) (C2SPlayCard <$ domEvent Click playEl)
                            , C2SVoteStar True <$ domEvent Click starEl
                            ]
          nextEv <- elDynAttr "div" (mkHidden <$> levelComplete) $ do
            (nextEl, _) <- elClass' "button" "btn btn-primary btn-large" $ text "Next Level"
            pure $ C2SNextLevel <$ domEvent Click nextEl
          restartEv <- elDynAttr "div" (mkHidden <$> isOver) $ do
            (restartEl, _) <- elClass' "button" "btn btn-primary btn-large" $ text "Play Again"
            pure $ C2SStartGame <$ domEvent Click restartEl
          pure $ leftmost [ gate (current $ (&&) <$> (not <$> levelComplete) <*> gameActive) playEv
                          , gate (current levelComplete) nextEv
                          , gate (current isOver) restartEv
                          , dropPlayEv
                          ]
  pure action
  where
    isGameStateChange = \case
      S2CGameUpdate gs -> Just gs
      _ -> Nothing
    isWrongCard = \case
      S2CWrongCard wc -> Just wc
      _ -> Nothing

-- Chat widget (bottom-right, grows upward)
chatWidget
  :: ( DomBuilder t m
     , MonadHold t m
     , MonadFix m
     , PostBuild t m
     , PerformEvent t m
     , Prerender t m
     )
  => Event t S2C -> m (Event t C2S)
chatWidget s2cEv = elClass "div" "chat-container" $ do
  rec
    chatOpen <- toggle True toggleEv
    toggleEv <- do
      (toggleEl, _) <- elClass' "div" "chat-header-bar" $ do
        text "Chat"
        dynText $ bool " \x25B2" " \x25BC" <$> chatOpen
      pure $ domEvent Click toggleEl

    msgEv <- elDynAttr "div" (ffor chatOpen $ \open ->
        "class" =: ("chat-body" <> if open then "" else " chat-body-hidden")) $ do
      let eRecRespTxt = fmapMaybe showMsg s2cEv
          eSystemMsg = fmapMaybe systemMsg s2cEv
          allMsgs = leftmost
            [ fmap (\t -> ("user", t)) eRecRespTxt
            , fmap (\t -> ("system", t)) eSystemMsg
            ]
      receivedMessages <- foldDyn (\m ms -> Prelude.reverse $ Prelude.take 50 (m : Prelude.reverse ms)) [] allMsgs
      elClass "div" "chat-messages" $ do
        void $ simpleList receivedMessages $ \mDyn ->
          elDynClass "div" (ffor mDyn $ \(cls, _) -> "chat-msg chat-msg-" <> cls) $
            dynText (snd <$> mDyn)
      rec
        let newMessage = tag (current $ value ti)
              $ leftmost [domEvent Click sendEl, keypress Enter ti]
        (ti, sendEl) <- elClass "div" "chat-input-area" $ do
          ti' <- inputElement $ def
            & inputElementConfig_setValue .~ fmap (const "") newMessage
            & inputElementConfig_elementConfig . elementConfig_initialAttributes .~
              ("placeholder" =: "Type a message..." <> "class" =: "chat-input")
          (sendEl', _) <- elClass' "button" "btn btn-send" $ text "Send"
          pure (ti', sendEl')
      pure $ C2SChat <$> newMessage
  pure msgEv
  where
    showMsg :: S2C -> Maybe Text
    showMsg = \case
      (S2Cbroadcast txt) -> Just txt
      _ -> Nothing
    systemMsg :: S2C -> Maybe Text
    systemMsg = \case
      S2CRoomUpdate rs -> Just $ "Room updated: " <> T.pack (show $ Prelude.length $ _rsPlayers rs) <> " players"
      S2CWrongCard wc -> Just $ _wcPlayer wc <> " played wrong card (" <> T.pack (show $ _wcPlayed wc) <> ")"
      _ -> Nothing
