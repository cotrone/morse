{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
module Morse.API where

import           Control.Applicative
import           Control.Monad.Reader
import           Data.Bytes.Serial (Serial)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import           Data.CaseInsensitive  (CI)
import qualified Data.CaseInsensitive as CI
import qualified Data.Csv as CSV
import           Data.Either
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

insensitize :: Text -> CI Text
insensitize = CI.mk . T.replace " " ""

insensitizeQuery :: MorseQuery -> MorseQueryCI
insensitizeQuery = fmap insensitize

lookupMorse :: (Serial t, Morse m) => t -> MorseQuery -> m MorseResponse
lookupMorse user q' = do
    (mt@(MorseTree { _mtOpeners=opn, _mtTables=tbls, _mtConfused=cr, _mtStateDecode=sdc })) <- askMorseTree
    let q = insensitizeQuery q'
    pure . applyResponder mt q =<< randomSelection =<<
      (maybe
       -- If we failed, its novel so log it and give a confused response.
       (logNovel user q' >> pure cr)
       -- But first try to actually find it.
       pure $
         -- Try looking it up with its state.
             (do
                 st <- fst q
                 tbl <- join $ ((`Map.lookup` tbls) . fst) <$> Map.lookup st sdc
                 -- Look it up directly, failing it that look up an open match in the table.
                 Map.lookup q tbl <|> Map.lookup (Nothing, snd q) tbl)
         -- Try looking it up like its from an empty state.
         <|> (Map.lookup (snd q) opn)
      )
  where
    randomSelection = sample . randomElement . Set.toList

applyResponder :: MorseTree -> MorseQueryCI -> MorseResponder -> MorseResponse
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
  let blnk = MorseTree defState mempty mempty mempty mempty defResp
  (<>) <$> ((sconcat . (blnk :|)) <$> forM tbls (readMorseTable defState defResp))
       <*> ((sconcat . (blnk :|)) <$> forM rsps (readMorseResponses defState defResp))

uuidStatePair :: UUID -> (Text, Text) -> (UUID, (Text, Text))
uuidStatePair base (tpl@(fl, st)) = (V5.generateNamed base . BS.unpack . TE.encodeUtf8 $ fl<>st, tpl)

splitDialogTrans :: DialogTrans UUID -> Either (CI Text, Set MorseResponder) (MorseQueryCI, Set MorseResponder)
splitDialogTrans (Transition AnyState q rsp rs) = Left (insensitize q, Set.singleton $ MorseResponder rsp rs)
splitDialogTrans (Transition TableState q rsp rs) = Right (insensitizeQuery (Nothing, q), Set.singleton $ MorseResponder rsp rs)
splitDialogTrans (Transition (SpecificState ss) q rsp rs) = Right (insensitizeQuery (Just ss, q), Set.singleton $ MorseResponder rsp rs)

readMorseTable :: UUID -> Text -> FilePath -> IO MorseTree
readMorseTable defState defResp fp = do
  putStrLn ("Loading "<>fp) 
  csvData <- BL.readFile fp
  case CSV.decodeByName csvData of
    Left err -> fail err
    Right (_, (v'::V.Vector (DialogTrans Text))) -> do
      let tblName = T.pack $ takeBaseName fp
      let v = (fmap (uuidStatePair defState . (tblName,))) <$> v'
      let (opns, tbls) = partitionEithers . toList $ (splitDialogTrans . fmap fst) <$> v
      pure $
        MorseTree
          defState
          (Map.fromListWith Set.union opns)
          (Map.singleton tblName $ Map.fromListWith Set.union tbls)
          mempty
          (Map.fromList .
           mapMaybe (\case { SetState mapping -> Just mapping; _ -> Nothing }) .
           toList $ _transResultState <$> v
          )
          defResp

readMorseResponses :: UUID -> Text -> FilePath -> IO MorseTree
readMorseResponses defState defResp fp = do
  putStrLn ("Loading "<>fp) 
  csvData <- BL.readFile fp
  case CSV.decodeByName csvData of
    Left err -> fail err
    Right (_, (v::V.Vector MorseResponder)) ->
      pure $ MorseTree defState mempty mempty (Set.fromList . toList $ v) mempty defResp
