{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Bot.Base where

import Control.Applicative
import Control.Exception (bracket_)
import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.Attoparsec.Char8 (Parser, IResult(..), maybeResult)
import Data.ByteString.Char8 (ByteString)
import Data.Monoid
import Network.IRC
import System.Console.ANSI
import qualified Data.Attoparsec.Char8 as A
import qualified Network.IRC.Commands as IRC

type Authenticator = BotConfig -> Bot ()

data BotConfig = BotConfig
    { ircNick     :: !ByteString
    , ircIdent    :: !ByteString
    , ircRealName :: !ByteString
    , ircHost     :: !String
    , ircPort     :: !Int
    , ircAuth     :: Maybe Authenticator
    }

-- data Environment = Environment
--     { nick      :: ByteString
--     , startTime :: UTCTime
--     }

data BotState = BotState
    { botWriter   :: Message -> IO ()
    , botParser   :: Parser Message
    , refill      :: IO ByteString
    -- , environment :: IORef Environment
    }

newtype Bot a = Bot (ReaderT BotState (StateT ByteString IO) a)
                deriving (Monad, Functor, MonadIO, MonadReader BotState, MonadState ByteString)

data IRCState = IRCState
    { ircMessage :: Message
    , ircWriter  :: Message -> IO ()
    -- , ircUser    :: User
    -- , botEnv     :: Environment
    }

newtype IRC a = IRC (ReaderT IRCState IO a)
                deriving (Monad, Functor, MonadIO, MonadReader IRCState)

instance (Monoid a) => Monoid (IRC a) where
    mempty  = return mempty
    mappend = liftM2 mappend

parse :: Bot Message
parse = do
    parser <- A.parse <$> asks botParser
    step . parser =<< get
  where
    step (Partial f) = asks refill >>= liftIO >>= step . f
    step (Done l m)  = put l       >> return m
    step _           = error "Parse failed"

runBot :: BotState -> Bot a -> IO a
runBot state (Bot a) = refill state >>= evalStateT (runReaderT a state)

----------------------------------------------------------------------------------------------

class MonadIRC m where
    write :: Message -> m ()

instance MonadIRC Bot where
    write msg = asks botWriter >>= liftIO . ($ msg)

instance MonadIRC IRC where
    write msg = asks ircWriter >>= liftIO . ($ msg)

send :: MonadIRC m => ByteString -> [ByteString] -> m ()
send c p = write $ IRC.mkMessage c p

pong :: MonadIRC m => Message -> m ()
pong = write . IRC.pong

nick :: MonadIRC m => ByteString -> m ()
nick = write . IRC.nick

user :: MonadIRC m => ByteString -> ByteString -> ByteString -> ByteString -> m ()
user u h s r = write $ IRC.user u h s r

joinChan :: MonadIRC m => ByteString -> m ()
joinChan = write . IRC.joinChan

partChan :: MonadIRC m => ByteString -> Maybe ByteString -> m ()
partChan c m = write $ IRC.partChan c m

privmsg :: MonadIRC m => ByteString -> ByteString -> m ()
privmsg c m = write $ IRC.privmsg c m

----------------------------------------------------------------------------------------------

colorize :: Color -> IO c -> IO c
colorize c = bracket_ (setSGR [ SetColor Foreground Dull c ])
                      (setSGR [ Reset ])
