{-# LANGUAGE OverloadedStrings #-}

module Main(main) where

import Lib
import System.IO
import Control.Monad
import Data.Attoparsec.Text
import qualified Data.Text as T

import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain tests

tests = testGroup "Tests" [keyKindParserTests, keyBindParserTests, bindParserTests, binderParserTests]

keyKindParserTests = testGroup "keyKindParserTests"
  [ keyKindParserTest "KEY" Key
  , keyKindParserTest "SYM:0" $ Sym 0
  ]

keyBindParserTests = testGroup "keyBindParserTests"
  [ keyBindParserTest "[KEY:p]" $ KeyBind Key "p"
  , keyBindParserTest "[SYM:1:Space]" $ KeyBind (Sym 1) "Space"
  , keyBindParserTest "[KEY:]]" $ KeyBind Key "]"
  ]

bindParserTests = testGroup "bindParserTests" 
  [ bindParserTest "[BIND:SELECT_ALL:REPEAT_NOT]" $ Bind "SELECT_ALL" "REPEAT_NOT"
  ]

binderParserTests = testGroup "binderParserTests" 
  [ binderParserTest txt $ Binder expectedBind expectedkeys
  ]
    where
      txt = T.pack $ 
          "[BIND:SELECT_ALL:REPEAT_NOT]\n" ++
          "[SYM:1:Enter]\n" ++
          "[SYM:2:Numpad Enter]\n" ++
          "[KEY:b]"
      expectedBind =
        Bind "SELECT_ALL" "REPEAT_NOT"
      expectedkeys =
        [ KeyBind (Sym 1) "Enter"
        , KeyBind (Sym 2) "Numpad Enter"
        , KeyBind Key "b"
        ]


testCase' p txt expected =
  testCase (T.unpack txt) $ Right expected @=? parseOnly p txt

keyKindParserTest = testCase' keyKindParser

keyBindParserTest = testCase' keyBindParser

bindParserTest = testCase' bindParser

binderParserTest = testCase' binderParser
