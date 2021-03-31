{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Morse.Types
 ( DialogTrans (..)
 , MorseTree(..)
 , MorseQuery
 , StateChange(..), parseStateChange
 , MorseResponder(..)
 , MorseResponse(..)
 , Morse(..)
 , MockMorse(..), runMockMorse
 ) where

import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Writer
import           Control.Monad.RWS.Strict
import           Data.Random.Source.StdGen
import qualified Data.Aeson as JS
import qualified Data.Csv as CSV
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Random.Source
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Text (Text)
import           Data.UUID (UUID)

data StateChange a
 = SameState
 | ClearState
 | SetState a
 deriving (Read, Show, Eq, Ord, Functor)

parseStateChange :: Text -> StateChange Text
parseStateChange "-" = SameState
parseStateChange "!" = ClearState
parseStateChange  s  = SetState s

data DialogTrans a
  = Transition
    { _transStartState  :: Maybe a
    , _transQuery       :: Text
    , _transResp        :: Text
    , _transResultState :: StateChange a
    }
  deriving (Read, Show, Eq, Ord, Functor)

-- | For loading tbl files.
instance CSV.FromNamedRecord (DialogTrans Text) where
  parseNamedRecord r =
    Transition <$> ((\case { "*" -> Nothing; s -> Just s }) <$> (r CSV..: "From"))
               <*>  r CSV..: "Query"
               <*>  r CSV..: "Response"
               <*> (parseStateChange <$> (r CSV..: "Destination"))

type MorseQuery = (Maybe UUID, Text)

-- | Used to construct a response given we have several different methods of changing state.
data MorseResponder
 = MorseResponder
   { _mrStmt  :: Text
   , _mrTrans :: StateChange UUID
   }
 deriving (Read, Show, Eq, Ord)

-- | For loading the rsp files.
--   Not parameterized on the state token because we can only clear it or keep it the same,
--   not set it.
instance CSV.FromNamedRecord MorseResponder where
  parseNamedRecord r =
    MorseResponder <$> r CSV..: "Response"
                   <*> ((r CSV..: "Destination") >>= (\case
                                                         "-" -> pure SameState
                                                         "!" -> pure ClearState
                                                         (_::Text) -> fail "Bad transition type in rsp file!"
                                                     )
                       )

data MorseResponse
 = MorseResponse
   { _mrSay   :: Text
   -- ^ The message to send to the user.
   , _mrState :: UUID
   -- ^ The state they're now in.
   }
 deriving (Read, Show, Eq, Ord)

instance JS.ToJSON MorseResponse where
  toJSON     (MorseResponse say st) = JS.object [ "state" JS..= st,   "say" JS..= say ]
  toEncoding (MorseResponse say st) = JS.pairs  ( "state" JS..= st <> "say" JS..= say )

-- A set of things to say when we don't understamd, and known questions with their responses.
data MorseTree
  = MorseTree
    { _mtDefState :: UUID
    -- ^ The state to respond with when we reset state or have none to start with.
    , _mtDialog   :: Map MorseQuery (Set MorseResponder)
    -- ^ Mappings from (State, Text) to response and state transition.
    , _mtConfused :: Set MorseResponder
    -- ^ Responses to use when confused
    , _mtStateDecode :: Map UUID (Text, Text)
    -- ^ A decoding scheme from UUID state rep to the file and state token inside it.
    , _mtDefResp  :: Text
    -- ^ A response to use if all else fails.
    }
  deriving (Read, Show, Eq, Ord)

-- | Left bias.
instance Semigroup MorseTree where
  (MorseTree ds dtl cl dl def) <> (MorseTree _ dtr cr dr _) =
    MorseTree ds
              (Map.unionWith Set.union dtl dtr)
              (cl `Set.union` cr)
              (dl `Map.union` dr)
              def

class (Monad m, MonadReader MorseTree m, MonadRandom m) => Morse m where
  logNovel :: MorseQuery -> m ()

-- Mock monad for Morse behaviors
newtype MockMorse a
  = MockMorse { runMockMorseT :: RWS MorseTree [MorseQuery] StdGen a }
  deriving (Functor, Applicative, Monad)
  deriving (MonadReader MorseTree, MonadWriter [MorseQuery], MonadState StdGen)

runMockMorse :: MorseTree -> StdGen -> MockMorse a -> (a, [MorseQuery])
runMockMorse t g m = evalRWS (runMockMorseT m) t g 

instance Morse MockMorse where
  logNovel = tell . pure

instance MonadRandom MockMorse where
    getRandomWord8  = MockMorse $ getRandomWord8
    getRandomWord16 = MockMorse $ getRandomWord16
    getRandomWord32 = MockMorse $ getRandomWord32
    getRandomWord64 = MockMorse $ getRandomWord64
    getRandomDouble = MockMorse $ getRandomDouble
    getRandomNByteInteger i = MockMorse $ getRandomNByteInteger i
