{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
module Morse.API where

import           Control.Applicative
import           Control.Monad.Reader
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Csv as CSV
import           Data.Foldable
import           Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.Map.Strict as Map
import           Data.Maybe
import           Data.Random
import           Data.Semigroup
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import           Data.UUID (UUID)
import qualified Data.UUID.V5 as V5
import qualified Data.Vector as V
import           Morse.Types
import           System.Directory
import           System.FilePath

lookupMorse :: Morse m => MorseQuery -> m MorseResponse
lookupMorse q = do
    (mt@(MorseTree {_mtDialog=t, _mtConfused=cr })) <- ask
    pure . applyResponder mt q =<< randomSelection =<<
      (maybe
       -- If we failed, its novel so log it and give a confused response.
       (logNovel q >> pure cr)
       -- But first try to actually find it.
       pure $
         -- Try looking it up with its state.
             (Map.lookup q t)
         -- Try looking it up like its from an empty state.
         <|> (Map.lookup (Nothing, snd q) t)
      )
  where
    randomSelection = sample . randomElement . Set.toList

applyResponder :: MorseTree -> MorseQuery -> MorseResponder -> MorseResponse
applyResponder (MorseTree { _mtDefState = defUUID }) (os, _) (MorseResponder stmt sc) =
  MorseResponse stmt $ case sc of
                         SameState   -> fromMaybe defUUID os
                         ClearState  -> defUUID
                         SetState ns -> ns


-- | Given a default state, a default response, and a directory of content files, build our MorseTree 
load :: UUID -> Text -> FilePath -> IO MorseTree
load defState defResp dir = do
  allContent <- listDirectory dir
  let tbls = (dir </>) <$> filter ("tbl" `isExtensionOf`) allContent
  let rsps = (dir </>) <$> filter ("rsp" `isExtensionOf`) allContent
  let blnk = MorseTree defState mempty mempty mempty defResp
  (<>) <$> ((sconcat . (blnk :|)) <$> forM tbls (readMorseTable defState defResp))
       <*> ((sconcat . (blnk :|)) <$> forM rsps (readMorseResponses defState defResp))

uuidStatePair :: UUID -> (Text, Text) -> (UUID, (Text, Text))
uuidStatePair base (tpl@(fl, st)) = (V5.generateNamed base . BS.unpack . TE.encodeUtf8 $ fl<>st, tpl)

splitDialogTrans :: DialogTrans (UUID, (Text, Text)) -> (MorseQuery, Set MorseResponder)
splitDialogTrans (Transition ss q rsp rs) = ((fst <$> ss, q), Set.singleton $ MorseResponder rsp (fst <$> rs))

readMorseTable :: UUID -> Text -> FilePath -> IO MorseTree
readMorseTable defState defResp fp = do
  csvData <- BL.readFile fp
  case CSV.decodeByName csvData of
    Left err -> fail err
    Right (_, (v'::V.Vector (DialogTrans Text))) -> do
      let v = (fmap (uuidStatePair defState . (T.pack $ takeBaseName fp,))) <$> v'
      pure $
        MorseTree
          defState
          (Map.fromListWith Set.union . toList $ splitDialogTrans <$> v)
          mempty
          (Map.fromList .
           mapMaybe (\case { SetState mapping -> Just mapping; _ -> Nothing }) .
           toList $ _transResultState <$> v
          )
          defResp

readMorseResponses :: UUID -> Text -> FilePath -> IO MorseTree
readMorseResponses defState defResp fp = do
  csvData <- BL.readFile fp
  case CSV.decodeByName csvData of
    Left err -> fail err
    Right (_, (v::V.Vector MorseResponder)) ->
      pure $ MorseTree defState mempty (Set.fromList . toList $ v) mempty defResp
