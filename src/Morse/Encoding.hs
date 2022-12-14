{-# LANGUAGE OverloadedStrings #-}
module Morse.Encoding
 ( encodeMorse
 , decodeMorse
 ) where

import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Tuple

type Morse = Text

-- via https://github.com/ozdemirburak/morse-decoder/blob/0d750bac6c4ab7000c32fcc7c662b87e2faa55bd/src/index.js
morseCode :: [(Char, Morse)]
morseCode =
  [ ('0',  "-----")
  , ('1',  ".----")
  , ('2',  "..---")
  , ('3',  "...--")
  , ('4',  "....-")
  , ('5',  ".....")
  , ('6',  "-....")
  , ('7',  "--...")
  , ('8',  "---..")
  , ('9',  "----.")
  , ('A',  ".-")
  , ('B',  "-...")
  , ('C',  "-.-.")
  , ('D',  "-..")
  , ('E',  ".")
  , ('F',  "..-.")
  , ('G',  "--.")
  , ('H',  "....")
  , ('I',  "..")
  , ('J',  ".---")
  , ('K',  "-.-")
  , ('L',  ".-..")
  , ('M',  "--")
  , ('N',  "-.")
  , ('O',  "---")
  , ('P',  ".--.")
  , ('Q',  "--.-")
  , ('R',  ".-.")
  , ('S',  "...")
  , ('T',  "-")
  , ('U',  "..-")
  , ('V',  "...-")
  , ('W',  ".--")
  , ('X',  "-..-")
  , ('Y',  "-.--")
  , ('Z',  "--..")
  , ('.',  ".-.-.-")
  , (',',  "--..--")
  , ('?',  "..--..")
  , ('\'', ".----.")
  , ('!',  "-.-.--")
  , ('/',  "-..-.")
  , ('(',  "-.--.")
  , (')',  "-.--.-")
  , ('&',  ".-...")
  , (':',  "---...")
  , (';',  "-.-.-.")
  , ('=',  "-...-")
  , ('+',  ".-.-.")
  , ('-',  "-....-")
  , ('_',  "..--.-")
  , ('"',  ".-..-.")
  , ('$',  "...-..-")
  , ('@',  ".--.-.")
  ]

specialCodes :: Map Morse Text
specialCodes = Map.fromList
  [ ("...---...", "SOS")
  ]

toMorseMap :: Map Char Morse
toMorseMap = Map.insert ' ' "/" $ Map.fromList morseCode

fromMorseMap :: Map Morse Text
fromMorseMap = Map.union specialCodes $ Map.insert "" "" $ Map.insert "/" " " $ Map.fromList (fmap (fmap T.singleton . swap) morseCode)

encodeMorse :: Text -> Morse
encodeMorse = T.intercalate " " . map (\c -> Map.findWithDefault "#" c toMorseMap) . T.unpack . T.toUpper

decodeMorse :: Morse -> Text
decodeMorse = T.concat . map (\t -> Map.findWithDefault (T.concat ["###", t, "###"]) t fromMorseMap) . T.splitOn " " . T.replace "_" " "
