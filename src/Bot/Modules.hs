{-# LANGUAGE ExistentialQuantification #-}

module Bot.Modules where

import Control.Applicative
import Data.ByteString.Char8 (ByteString)
import Data.Monoid

import Bot.Base

data Module = forall a. Module
    { moduleName  :: ByteString
    , commandList :: [a -> IRC ()]
    , commandInit :: IO a
    }

data IRCHandler = IRCHandler
    { namespace :: ByteString
    , hook      :: IRC ()
    }

mkModule :: ByteString -> [a -> IRC ()] -> IO a -> Module
mkModule = Module

mkModule_ :: ByteString -> [IRC ()] -> Module
mkModule_ name hooks = mkModule name (const <$> hooks) (return ())

initModule :: Module -> IO IRCHandler
initModule (Module name hooks initialize) = do
    x <- initialize
    return . IRCHandler name . mconcat $ fmap ($ x) hooks
