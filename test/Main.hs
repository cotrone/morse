{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main (main) where

import           Data.Random.Source.StdGen
import           Data.Text (Text)
import           Data.UUID
import           Morse.API
import           Morse.Types
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.TestVector

main :: IO ()
main = defaultMain tests


tests :: TestTree
tests =
  testGroup "Morse"
  [ loadTests
  , transitionTests
  ]

loadTests :: TestTree
loadTests = testGroup "Loading"
  [  testCase "Golden load" $ do
      g <- load (read "eccfd622-9197-11eb-8001-8c16454fb02a") "Wut mate?" "content"
      g @?= golden 
  ]

transitionTests :: TestTree
transitionTests = testGroup "Transitions"
  [ testVectors
      "Known single transitions"
      (\(r, q) er -> (runMockMorse golden (mkStdGen r) $ lookupMorse ("nonce"::Text) q) == er)
      [ ((1, (Nothing,   "Hello")),              (MorseResponse "Hello to you too!" defState, []))
      , ((1, (Nothing,   "What is your name?")), (MorseResponse "Morse" defState, []))
      , ((1, (Nothing,   "whatisyourname?")),    (MorseResponse "Morse" defState, []))
      , ((1, (Nothing,   "Who are you?")),       (MorseResponse "Morse" defState, []))
      , ((1, (Nothing,   "Unexpected")),         (MorseResponse "I'm done with this" defState, [(Nothing,   "Unexpected")]))
      , ((6, (Nothing,   "Unexpected")),         (MorseResponse "I don't understand" defState, [(Nothing,   "Unexpected")]))
      , ((1, (Just u_de, "Unexpected")),         (MorseResponse "I'm done with this" defState, [(Just u_de, "Unexpected")]))
      , ((6, (Just u_4c, "Unexpected")),         (MorseResponse "I don't understand" u_4c, [(Just u_4c, "Unexpected")]))
      , ((1, (Nothing,   "Say yes")),            (MorseResponse "No" u_de, []))
      , ((1, (Just u_de, "Say yes")),            (MorseResponse "No" u_b7, []))
      , ((6, (Just u_de, "Say yes?")),           (MorseResponse "I don't understand" u_de, [(Just u_de, "Say yes?")]))
      , ((1, (Just u_4c, "Say yes")),            (MorseResponse "Yes" u_4c, []))
      , ((6, (Just u_b7, "Say yes")),            (MorseResponse "Fine" u_4c, []))
      , ((1, (Just u_b7, "Do what you will")),   (MorseResponse "I will" defState, []))
      , ((1, (Nothing,   "Are you answering the question?")), (MorseResponse "I'm done with this" defState, [(Nothing, "Are you answering the question?")]))
      , ((1, (Just u_4c, "Are you answering the question?")), (MorseResponse "I am" u_4c, []))
      , ((1, (Just u_b7, "Are you answering the question?")), (MorseResponse "I am" u_b7, []))
      , ((1, (Just u_de, "Are you answering the question?")), (MorseResponse "I am" u_de, []))
      ]
  ]

-- Hand verified loading of what is in "content"
defState, u_4c, u_b7, u_de :: UUID
defState = read "eccfd622-9197-11eb-8001-8c16454fb02a"
u_4c = read "4c7e974a-2a55-52bf-8c0f-2cd605c9161d"
u_b7 = read "b7cc3cd2-9f0b-5ec3-8c94-8d4c5ef8f7f1"
u_de = read "de5afbd1-ad89-506b-8ffe-9a56d7a1a0fc"

golden :: MorseTree
golden =
  MorseTree
    defState
    [ ("Hello", [MorseResponder "Hello to you too!" ClearState])
    , ("Sayyes", [MorseResponder "No" $ SetState u_de])
    , ("Whatisyourname?", [MorseResponder "Morse" ClearState])
    , ("Whoareyou?", [MorseResponder "Morse" ClearState])
    , ("Dowhatyouwill", [MorseResponder "I will" ClearState])
    ]
    [ ( "SayYes",
        [ ((Just u_4c,"Sayyes"), [MorseResponder "Yes" SameState])
        , ((Just u_b7,"Sayyes"), [MorseResponder "Fine" $ SetState u_4c
                                 ,MorseResponder "Yes" $ SetState u_4c])
        , ((Just u_de,"Sayyes"), [MorseResponder "No" $ SetState u_b7])
        ,((Nothing, "Areyouansweringthequestion?"), [MorseResponder "I am" SameState])
        ])
    , ( "General", [])
    ]
    [ MorseResponder "I don't understand" SameState
    , MorseResponder "I'm done with this" ClearState
    ]
    [ (u_4c, ("SayYes","3"))
    , (u_b7, ("SayYes","2"))
    , (u_de, ("SayYes","1"))
    ]
    "Wut mate?"
