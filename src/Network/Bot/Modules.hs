{-# LANGUAGE ExistentialQuantification, OverloadedStrings #-}

module Network.Bot.Modules where

import Control.Applicative
import Control.Monad.Reader
import Data.ByteString.Char8 (ByteString)
import Data.IORef
import Data.Word
import Network.Bot.Base
import Network.Bot.Irc
import Network.IRC
import qualified Data.ByteString.Char8 as B

mkModule_ :: ByteString -> [Irc ()] -> Module
mkModule_ name hooks = mkModule name (const <$> hooks) (return ())

mkModule :: ByteString -> [a -> Irc ()] -> Bot a -> Module
mkModule = Module

onBangCommand f = whenCommand "PRIVMSG" $ do
    (x : xs) <- B.words . (!! 1) <$> asks (parameters . msg)
    when (B.head x == '!') $ f x xs

mkCommandHandler :: ByteString -> Word8 -> [ByteString] -> (ByteString -> ByteString) -> Module
mkCommandHandler name prefix commands f = undefined

initModule :: Module -> Bot Handler
initModule (Module name hooks initialize) = do
    x <- initialize
    return . Handler name $ fmap ($ x) hooks

loadModules :: [Module] -> Bot ()
loadModules modules = do
    h <- mapM initModule modules
    modifyEnv (\e -> e { handlers = h })
