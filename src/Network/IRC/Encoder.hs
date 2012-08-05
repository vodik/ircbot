{-# LANGUAGE OverloadedStrings #-}

module Network.IRC.Encoder ( encode, prettyPrint ) where

import Data.Monoid (Monoid, mempty, (<>))
import Data.Maybe (maybe, isJust)
import Data.ByteString (ByteString)
import Network.IRC.Base
import qualified Data.ByteString.Char8 as B

encode :: Message -> ByteString
encode msg = prettyPrint msg <> "\r\n"

prettyPrint :: Message -> ByteString
prettyPrint (Message p c ps) = encodePrefix' p <> c <> encodeParameters ps
  where
    encodePrefix' = maybe mempty $ (<> " ") . encodePrefix

encodePrefix :: Prefix -> ByteString
encodePrefix (ServerPrefix s)   = ":" <> s
encodePrefix (NickPrefix n u h) = ":" <> n
                                      <> maybe mempty ("!" <>) u
                                      <> maybe mempty ("@" <>) h

encodeParameters :: [Parameter] -> ByteString
encodeParameters []     = mempty
encodeParameters [x]    = trailer x <> x
encodeParameters (x:xs) = " " <> x <> encodeParameters xs

trailer :: ByteString -> ByteString
trailer x = if any ($ x) [ hasSpace, B.null, prefixed ] then " :" else " "
  where
    hasSpace = isJust . B.find (== ' ')
    prefixed = (== ':') . B.head
