{-# LANGUAGE OverloadedStrings #-}

module Lib (
    keyKindParser
  , keyBindParser
  , bindParser
  , binderParser
  , parseInterfaceFile
  , Binder(..)
  , Bind(..)
  , KeyBind(..)
  , KeyKind(..)
  ) where

import Prelude hiding (take)
import Data.Attoparsec.Text
import Data.Text (Text)
import Data.Functor (void)
import qualified Data.Text as T

data Binder = Binder { bind :: Bind, keys :: [KeyBind] } deriving (Show, Eq)

data Bind = Bind { bindName :: Text, bindRepeat :: Text } deriving (Show, Eq)
data KeyBind = KeyBind { keyKind :: KeyKind, keyKey :: Text } deriving (Show, Eq)
data KeyKind = Sym Int | Key deriving (Show, Eq)

skip1 :: Char -> Parser ()
skip1 c = skip (== c)

skipOpen = skip1 '['
skipColon = skip1 ':'

keyKindParser :: Parser KeyKind
keyKindParser = do
  symbol <- take 3 
  if symbol == "KEY"
  then return Key
  else do
    skipColon
    n <- decimal
    return $ Sym n

keyBindParser :: Parser KeyBind
keyBindParser = do
  skipOpen
  keyKind <- keyKindParser
  skipColon
  txt <- takeTill isEndOfLine
  -- Remove the last `]`. We could be fancy, but this works
  let withoutLastBracker = T.init txt
  return $ KeyBind keyKind withoutLastBracker

bindParser :: Parser Bind
bindParser = do
  skipOpen
  void $ string "BIND"
  skipColon
  name <- takeTill (== ':')
  skipColon
  repeat <- T.init <$> takeTill isEndOfLine
  return $ Bind name repeat

binderParser :: Parser Binder
binderParser = do
  bind <- bindParser <* endOfLine
  keys <- many' $ keyBindParser <* (option () endOfLine)
  return $ Binder bind keys

parseInterfaceFile :: Parser [Binder]
parseInterfaceFile = many' $ binderParser <* endOfLine
