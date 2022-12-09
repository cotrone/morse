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
 , StateMatcher(..)
 , MorseQuery, MorseQueryCI
 , StateChange(..), parseStateChange
 , MorseResponder(..)
 , MorseResponse(..)
 , Morse(..)
 , MockMorse(..), runMockMorse
 ) where

import           Control.Monad.Reader
import           Control.Monad.Writer
import           Control.Monad.RWS.Strict
import qualified Data.Aeson as JS
import           Data.Bytes.Serial (Serial)
import           Data.CaseInsensitive  (CI)
import qualified Data.Csv as CSV
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Random.Source
import           Data.Random.Source.StdGen
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.UUID (UUID)
import qualified Data.UUID as UUID
import qualified Data.CaseInsensitive as CI
import Data.Bifunctor (bimap)

data StateChange a
 = SameState
 | ClearState
 | SetState a
 deriving (Read, Show, Eq, Ord, Functor)

instance JS.ToJSON a => JS.ToJSON (StateChange a) where
  toJSON SameState = JS.String "-"
  toJSON ClearState = JS.String "!"
  toJSON (SetState a) = JS.toJSON a


parseStateChange :: Text -> StateChange Text
parseStateChange "-" = SameState
parseStateChange "!" = ClearState
parseStateChange  s  = SetState s

data StateMatcher a
 = AnyState        -- ^ Always can be matched.
 | TableState      -- ^ Match any state in the table.
 | SpecificState a -- ^ Only match a given state.
 deriving (Read, Show, Eq, Ord, Functor) 

data DialogTrans a
  = Transition
    { _transStartState  :: StateMatcher a
    , _transQuery       :: Text
    , _transResp        :: Text
    , _transResultState :: StateChange a
    }
  deriving (Read, Show, Eq, Ord, Functor)

-- | For loading tbl files.
instance CSV.FromNamedRecord (DialogTrans Text) where
  parseNamedRecord r =
    Transition <$> ((\case { "*" -> AnyState; "&" -> TableState; s -> SpecificState s }) <$> (r CSV..: "From"))
               <*>  r CSV..: "Query"
               <*>  r CSV..: "Response"
               <*> (parseStateChange <$> (r CSV..: "Destination"))

type MorseQuery = (Maybe UUID, Text)
type MorseQueryCI = (Maybe UUID, CI Text)

-- | Used to construct a response given we have several different methods of changing state.
data MorseResponder
 = MorseResponder
   { _mrStmt  :: Text
   , _mrTrans :: StateChange UUID
   }
 deriving (Read, Show, Eq, Ord)

instance JS.ToJSON MorseResponder where
  toJSON (MorseResponder stmt trans) =
    JS.object [
      "stmt" JS..= stmt
    , "trans" JS..= trans
    ]

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

type Table = Text

-- A set of things to say when we don't understamd, and known questions with their responses.
data MorseTree
  = MorseTree
    { _mtDefState :: UUID
    -- ^ The state to respond with when we reset state or have none to start with.
    , _mtOpeners  :: Map (CI Text) (Set MorseResponder)
    -- ^ matchers on the open state.
    , _mtTables   :: Map Table (Map MorseQueryCI (Set MorseResponder))
    -- ^ Mappings from (State, Text) to response and state transition for a given table.
    , _mtConfused :: Set MorseResponder
    -- ^ Responses to use when confused
    , _mtStateDecode :: Map UUID (Table, Text)
    -- ^ A decoding scheme from UUID state rep to the file and state token inside it.
    , _mtDefResp  :: Text
    -- ^ A response to use if all else fails.
    }
  deriving (Read, Show, Eq, Ord)

-- | Left bias.
instance Semigroup MorseTree where
  (MorseTree ds dol dtl cl dl def) <> (MorseTree _ dor dtr cr dr _) =
    MorseTree ds
              (Map.unionWith Set.union dol dor)
              (Map.unionWith (Map.unionWith Set.union) dtl dtr)
              (cl `Set.union` cr)
              (dl `Map.union` dr)
              def

class (Monad m, MonadRandom m) => Morse m where
  -- | Log a query we don't know how to reply to, with a unique token related to the asker
  --   for purpose of estimating asker frequency.
  logNovel :: Serial a => a -> MorseQuery -> m ()
  -- | Returns the current MorseTree
  askMorseTree :: m MorseTree

-- Mock monad for Morse behaviors
newtype MockMorse a
  = MockMorse { runMockMorseT :: RWS MorseTree [MorseQuery] StdGen a }
  deriving (Functor, Applicative, Monad)

runMockMorse :: MorseTree -> StdGen -> MockMorse a -> (a, [MorseQuery])
runMockMorse t g m = evalRWS (runMockMorseT m) t g 

instance Morse MockMorse where
  logNovel _ = MockMorse . tell . pure
  askMorseTree = MockMorse ask

instance MonadRandom MockMorse where
    getRandomWord8  = MockMorse $ getRandomWord8
    getRandomWord16 = MockMorse $ getRandomWord16
    getRandomWord32 = MockMorse $ getRandomWord32
    getRandomWord64 = MockMorse $ getRandomWord64
    getRandomDouble = MockMorse $ getRandomDouble
    getRandomNByteInteger i = MockMorse $ getRandomNByteInteger i

staticizeMorseTree :: MorseTree -> JS.Value
staticizeMorseTree (MorseTree { _mtDefState=defState, _mtOpeners=opn, _mtTables=tbls, _mtConfused=cr, _mtStateDecode=sdc, _mtDefResp=defResp}) =
  JS.object [
    "defState" JS..= UUID.toText defState
  , "openers" JS..= (Map.fromList $ bimap (Text.toLower . CI.original) Set.toList <$> Map.toList opn)
  , "tables" JS..= expandMorseTreeTable tbls
  , "confused" JS..= Set.toList cr
  , "stateDecode" JS..= (fmap (fmap encodeStateDecode) $ Map.toList sdc)
  , "defResp" JS..= defResp
  ]
  where
    expandMorseTreeTable :: Map Table (Map MorseQueryCI (Set MorseResponder)) -> Map Text (Map Text (Map Text (Set MorseResponder)))
    expandMorseTreeTable = fmap (Map.fromListWith (Map.unionWith Set.union) . fmap expandQuery . Map.toList)

    -- Expand the query for so the responses for each state are in the contained map
    expandQuery :: (MorseQueryCI, Set MorseResponder) -> (Text, Map Text (Set MorseResponder))
    expandQuery ((Nothing, stmt), resps) = ("empty", Map.singleton (Text.toLower $ CI.original stmt) resps)
    expandQuery ((Just st, stmt), resps) = (UUID.toText st, Map.singleton (Text.toLower $ CI.original stmt) resps)
    
    encodeStateDecode :: (Table, Text) -> JS.Value
    encodeStateDecode (tbl, token) =
      JS.object [
        "table" JS..= tbl
      , "token" JS..= token
      ]
