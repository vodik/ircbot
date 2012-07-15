{-# LANGUAGE ExistentialQuantification #-}

module Modules where

import Data.ByteString.Char8 (ByteString)
import Base

data Module = forall a. Module
    { moduleName  :: ByteString
    , commandInit :: Irc a
    , commandList :: [a -> Irc ()]
    }

