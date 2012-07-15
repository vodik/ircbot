{-# LANGUAGE OverloadedStrings #-}

module Network.IRC.Encoder ( encode ) where

import Data.Monoid (Monoid, mempty, (<>))
import Data.Maybe (maybe, isJust)
import Data.ByteString.Char8
import Network.IRC.Base
import qualified Data.ByteString.Char8 as B

encode :: Message -> ByteString
encode (Message p c ps) = encodePrefix' p <> c <> encodeParameters ps
  where
    encodePrefix' = maybe mempty $ (<> " ") . encodePrefix

encodePrefix :: Prefix -> ByteString
encodePrefix (ServerPrefix s)   = ":" <> s
encodePrefix (NickPrefix n u h) = ":" <> n
                                      <> maybe mempty ("!" <>) u
                                      <> maybe mempty ("@" <>) h

encodeParameters :: [Parameter] -> ByteString
encodeParameters [] = mempty
encodeParameters (x : [])
    | hasSpace x || B.null x || B.head x == ':' = " :" <> x
    | otherwise = " " <> x
encodeParameters (x : xs) = " " <> x <> encodeParameters xs

hasSpace :: ByteString -> Bool
hasSpace = isJust . B.find (== ' ')
