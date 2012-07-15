{-# LANGUAGE ExistentialQuantification #-}

module Modules where

import Control.Applicative
import Control.Monad.Reader
import Data.ByteString.Char8 (ByteString)
import Data.IORef
import Base
import Irc

mkModule_ :: ByteString -> [Irc ()] -> Module
mkModule_ name hooks = mkModule name (const <$> hooks) (return ())

mkModule :: ByteString -> [a -> Irc ()] -> Bot a -> Module
mkModule = Module

initModule :: Module -> Bot Handler
initModule (Module name hooks initialize) = do
    x <- initialize
    return . Handler name $ fmap ($ x) hooks

loadModules :: [Module] -> Bot ()
loadModules modules = do
    h <- mapM initModule modules
    x <- asks handlers
    io $ writeIORef x h
