{-# LANGUAGE OverloadedStrings #-}

module Network.IRC.Commands.Mode where

import Data.Maybe (maybe)
import Data.Monoid ((<>))
import Data.ByteString.Char8
import Network.IRC.Base
import Network.IRC.Commands

setOp :: Bool -> Channel -> UserName -> Message
setOp b c u = mkMessage "MODE" [ c, b ^! "o", u]

True  ^! str = cons '+' str
False ^! str = cons '-' str
