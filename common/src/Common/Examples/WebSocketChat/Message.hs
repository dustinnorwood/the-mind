{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Common.Examples.WebSocketChat.Message where

import           Control.Arrow ((***))
import           Control.Lens
import           Data.Aeson (ToJSON, FromJSON, toEncoding, parseJSON,
                            defaultOptions, Options,
                            genericToEncoding, genericParseJSON)
import           Data.Foldable (foldrM)
import           Data.List (partition, sort)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Maybe
import           Data.Text (Text)
import qualified Data.Text as T
import           GHC.Generics (Generic)
import           Test.QuickCheck

data RoomConfig = RoomConfig
  { _roomName :: Text
  , _roomPassword :: Text
  } deriving (Eq, Show, Generic)
makeLenses ''RoomConfig

data GameConfig = GameConfig
  { _gamePlayerNames :: [Text]
  } deriving (Eq, Show, Generic)
makeLenses ''GameConfig

data PlayerState = PlayerState
  { _starVote :: Bool
  , _showingTop :: Bool
  , _cards :: [Int]
  } deriving (Eq, Show, Generic)
makeLenses ''PlayerState

data GameState = GameState
  { _level :: Int
  , _lives :: Int
  , _stars :: Int
  , _lastCard :: [Int]
  , _players :: Map Text PlayerState 
  } deriving (Eq, Show, Generic)
makeLenses ''GameState

data PlayerSummary = PlayerSummary
  { _psNumCards :: Int
  , _psHasVoted :: Bool
  , _psTopCard  :: Maybe Int
  } deriving (Eq, Show, Generic)
makeLenses ''PlayerSummary

data MyGameState = MyGameState
  { _myName  :: Text
  , _myLevel :: Int
  , _myLives :: Int
  , _myStars :: Int
  , _myLastPlayedCard :: [Int]
  , _myRemainingCards :: Int
  , _myCards :: [Int]
  , _myTeam  :: Map Text PlayerSummary
  } deriving (Eq, Show, Generic)
makeLenses ''MyGameState

emptyPlayerState :: PlayerState
emptyPlayerState = PlayerState False False []

newLevel :: Int -> [Text] -> IO (Map Text PlayerState)
newLevel levelNum playerNames = deck
  where deck = snd <$> foldrM buildDeck ([] :: [Int], M.empty) totalCards
        totalCards = replicate levelNum =<< playerNames
        buildDeck name d = do
          num <- generate $ choose (1, 100)
          if num `elem` fst d
            then buildDeck name d
            else pure $ case M.lookup name (snd d) of
              Nothing ->
                let d' = M.insert name emptyPlayerState <$> d
                 in addCard name num d'
              _ -> addCard name num d
        addCard name num =
          let one = id %~ (num:)
              two = at name . mapped . cards %~ sort . (num:) 
           in one *** two

awardLife :: Int -> GameState -> GameState
awardLife l | l == 3 || l == 6 || l == 9 = lives +~ 1
            | otherwise = id

awardStar :: Int -> GameState -> GameState
awardStar l | l == 2 || l == 5 || l == 8 = stars +~ 1
            | otherwise = id

nextLevel :: GameState -> IO GameState
nextLevel gs = do
  let levelNum = 1 + _level gs
      playerNames = M.keys $ _players gs
  ps <- newLevel levelNum playerNames
  pure $ gs & level .~ levelNum
            & players .~ ps
            & lastCard .~ []
            & awardLife levelNum
            & awardStar levelNum

tryNextLevel :: GameState -> IO GameState
tryNextLevel gs =
  if remainingCards gs == 0
    then nextLevel gs
    else pure gs

loseLife :: GameState -> IO GameState
loseLife gs = do
  let levelNum = _level gs
      playerNames = M.keys $ _players gs
      numLives = _lives gs - 1
  ps <- newLevel levelNum playerNames
  pure $ gs & level .~ levelNum
            & players .~ ps
            & lives .~ numLives

numLives :: Int -> Int
numLives n | n <= 2 = 2
           | n == 3 = 3
           | otherwise = 4 

newGame :: [Text] -> IO GameState
newGame playerNames = do
  let lev = 1
      lives = numLives $ length playerNames
      stars = 1
  playerStates <- newLevel 1 playerNames
  pure $ GameState lev lives stars [] playerStates

getTopCard :: PlayerState -> Maybe Int
getTopCard = listToMaybe . _cards

getAllTopCards :: GameState -> Map Text Int
getAllTopCards = M.mapMaybe getTopCard . _players

getTopCardForPlayer :: Text -> GameState -> Maybe Int
getTopCardForPlayer n = M.lookup n . getAllTopCards

minimumCard :: GameState -> Maybe Int
minimumCard = (\x -> if x < 101 then Just x else Nothing)
            . minimum 
            . (101:)
            . M.elems
            . getAllTopCards

remainingCards :: GameState -> Int
remainingCards = sum . map (length . _cards) . M.elems . _players

trimCards :: Maybe Int -> GameState -> GameState
trimCards mCard gs = case mCard of
  Nothing -> gs
  Just c -> let ps = _players gs
                ps' = M.map (cards %~ filter (> c)) ps
             in gs & players .~ ps'
                   & lastCard %~ (c:)

playCard :: Text -> GameState -> GameState
playCard name gs =
  let playerCard = getTopCardForPlayer name gs
      nextCard = minimumCard gs
      gs' = gs & players . at name . mapped . cards %~ tail
               & players . at name . mapped . showingTop .~ False
               & lastCard %~ maybe id (:) playerCard
   in if nextCard == playerCard then gs' else gs & lives -~ 1
                                                 & trimCards playerCard

useStar :: GameState -> GameState
useStar gs = if _stars gs <= 0
  then gs
  else let gs' = gs & stars -~ 1
                    & players %~ M.map (starVote .~ False)
                    & players %~ M.map (showingTop .~ True)
        in gs'

voteStar :: Text -> GameState -> GameState
voteStar name gs =
  let gs' = (players . at name . _Just . starVote %~ not) gs
      starVotes = _starVote <$> M.elems (gs' ^. players)
      shouldUseStar = and starVotes
   in if shouldUseStar then useStar gs' else gs'

myGameState :: Text -> GameState -> MyGameState
myGameState name gs@GameState{..} =
  let _myName = name
      _myLevel = _level
      _myLives = _lives
      _myStars = _stars
      _myLastPlayedCard = _lastCard
      _myRemainingCards = remainingCards gs
      _myCards = maybe [] _cards $ M.lookup name _players
      f PlayerState{..} = PlayerSummary
                            (length _cards)
                            _starVote
                            (if _showingTop 
                              then listToMaybe _cards 
                              else Nothing)
      _myTeam = M.map f _players
   in MyGameState{..}

data Client a = Client
  { _clientName :: Text
  , _clientRoom :: Text
  , _clientConn :: a
  } deriving (Eq, Show, Functor, Generic)
makeLenses ''Client

data Room a = Room
  { _roomConfig :: RoomConfig
  , _roomClients :: Map Text (Client a)
  , _roomGameState :: Maybe GameState
  } deriving (Eq, Show, Functor, Generic)
makeLenses ''Room

data RoomSummary = RoomSummary
  { _rsName :: Text
  , _rsPlayers :: [Text]
  , _rsGameState:: Maybe MyGameState
  } deriving (Eq, Show, Generic)
makeLenses ''RoomSummary

newRoom :: RoomConfig -> Client a -> Room a
newRoom rc c = Room rc cm Nothing
  where cm = M.singleton (_clientName c) c

data ServerState a = ServerState
  { _rooms :: Map Text (Room a)
  } deriving (Eq, Show, Functor, Generic)
makeLenses ''ServerState

newServerState :: ServerState a
newServerState = ServerState M.empty

numClients :: ServerState a -> Int
numClients = sum . map (length . _roomClients) . M.elems . _rooms

clientExists :: Client a -> ServerState a -> Bool
clientExists client ss =
  let c = _clientName client
      f = (== c)
      g = any f . M.keys . _roomClients
   in or . map g . M.elems $ _rooms ss

addClient :: Client a -> Text -> ServerState a -> ServerState a
addClient client r = rooms . at r . _Just . roomClients %~ f
  where f = M.insert (_clientName client) client

removeClient :: Text -> Client a -> ServerState a -> ServerState a
removeClient name client ss' =
  let m = M.singleton (_clientName client) client
      updateRoom ss = case M.lookup name (_rooms ss) of
        Nothing -> ss
        Just r -> let r' = (roomClients %~ (M.\\ m)) r
                   in if M.null (_roomClients r')
                        then (rooms %~ M.delete name) ss
                        else (rooms . at name . mapped .~ r') ss
   in updateRoom ss'

data C2S = C2Sjoin Text
         | C2Sclose
         | C2SChat Text
         | C2SCreateRoom Text RoomConfig
         | C2SJoinRoom Text RoomConfig
         | C2SStartGame
         | C2SVoteStar Bool
         | C2SPlayCard
         | C2SNextLevel
         deriving (Eq, Show, Generic)

data S2C = S2Cwelcome Text
         | S2Cbroadcast Text
         | S2Cuserexists
         | S2Cnameproblem
         | S2CRoomDoesntExist RoomConfig
         | S2CRoomAlreadyExists Text
         | S2CRoomJoined RoomSummary
         | S2CRoomUpdate RoomSummary
         | S2CGameNotStarted
         | S2CGameUpdate MyGameState
         deriving (Eq,Show, Generic)

options :: Options
options = defaultOptions -- { tagSingleConstructors = True }

instance ToJSON RoomConfig where toEncoding = genericToEncoding options
instance FromJSON RoomConfig where parseJSON = genericParseJSON options

instance ToJSON GameConfig where toEncoding = genericToEncoding options
instance FromJSON GameConfig where parseJSON = genericParseJSON options

instance ToJSON PlayerState where toEncoding = genericToEncoding options
instance FromJSON PlayerState where parseJSON = genericParseJSON options

instance ToJSON GameState where toEncoding = genericToEncoding options
instance FromJSON GameState where parseJSON = genericParseJSON options

instance ToJSON PlayerSummary where toEncoding = genericToEncoding options
instance FromJSON PlayerSummary where parseJSON = genericParseJSON options

instance ToJSON MyGameState where toEncoding = genericToEncoding options
instance FromJSON MyGameState where parseJSON = genericParseJSON options

instance ToJSON a => ToJSON (Client a) where toEncoding = genericToEncoding options
instance FromJSON a => FromJSON (Client a) where parseJSON = genericParseJSON options

instance ToJSON a => ToJSON (Room a) where toEncoding = genericToEncoding options
instance FromJSON a => FromJSON (Room a) where parseJSON = genericParseJSON options

instance ToJSON RoomSummary where toEncoding = genericToEncoding options
instance FromJSON RoomSummary where parseJSON = genericParseJSON options

instance ToJSON a => ToJSON (ServerState a) where toEncoding = genericToEncoding options
instance FromJSON a => FromJSON (ServerState a) where parseJSON = genericParseJSON options

instance ToJSON C2S where toEncoding = genericToEncoding options
instance FromJSON C2S where parseJSON = genericParseJSON options

instance ToJSON S2C where toEncoding = genericToEncoding options
instance FromJSON S2C where parseJSON = genericParseJSON options
