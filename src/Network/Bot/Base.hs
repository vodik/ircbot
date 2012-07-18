{-# LANGUAGE GeneralizedNewtypeDeriving, Rank2Types, ExistentialQuantification #-}

module Network.Bot.Base where

import Control.Concurrent.Chan
import Control.Monad.Reader
import Data.ByteString.Char8 (ByteString)
import Data.Maybe (maybe)
import Data.IORef
import Data.Time
import Database.HDBC (IConnection, commit)
import Network.IRC
import System.IO
import Prelude hiding (catch)
import qualified Database.HDBC.Sqlite3 as DB
import qualified Network.IRC.Commands as IRC

data Module = forall a. Module
    { moduleName  :: ByteString
    , commandList :: [a -> Irc ()]
    , commandInit :: Bot a
    }

data Handler = Handler
    { namespace :: ByteString
    , hook      :: [Irc ()]
    }

data Environment = Environment
    { nick     :: ByteString
    , handlers :: [Handler]
    }

type Env = IORef Environment

data BotState = BotState
    { readMessage  :: IO (Maybe Message)
    , writeMessage :: Message -> IO ()
    , database     :: DB.Connection
    , environment  :: Env
    , startTime    :: UTCTime
    }

data IrcState = IrcState
    { msg :: Message
    , bot :: BotState
    }

newtype Bot a = Bot { unBot :: ReaderT BotState IO a }
              deriving ( Monad, Functor, MonadIO, MonadReader BotState )

newtype Irc a = Irc { unIrc :: ReaderT IrcState IO a }
              deriving ( Monad, Functor, MonadIO, MonadReader IrcState )

class MonadIrc m where
    write    :: Message -> m ()
    withSql  :: (forall c. IConnection c => c -> IO a) -> m a
    readEnv  :: m Environment
    writeEnv :: Environment -> m ()

instance MonadIrc Bot where
    write msg  = asks writeMessage >>= io . ($ msg)
    withSql f  = asks database     >>= runSql f
    readEnv    = asks environment  >>= io . readIORef
    writeEnv e = asks environment  >>= io . flip writeIORef e

instance MonadIrc Irc where
    write msg  = asks (writeMessage . bot) >>= io . ($ msg)
    withSql f  = asks (database     . bot) >>= runSql f
    readEnv    = asks (environment  . bot) >>= io . readIORef
    writeEnv e = asks (environment  . bot) >>= io . flip writeIORef e

modifyEnv :: (Monad m, MonadIrc m) => (Environment -> Environment) -> m ()
modifyEnv f = readEnv >>= (writeEnv $!) . f

runSql :: (MonadIO m, IConnection conn) => (conn -> IO a) -> conn -> m a
runSql f conn = io $ f conn >>= \result -> commit conn >> return result

getNick :: Bot ByteString
getNick = asks environment >>= io . liftM nick . readIORef

pong :: MonadIrc m => Message -> m ()
pong = write . IRC.pong

quit :: MonadIrc m => Maybe ByteString -> m ()
quit = write . IRC.quit

io :: MonadIO m => IO a -> m a
io = liftIO

whenJust :: Monad m => Maybe a -> (a -> m ()) -> m ()
whenJust m f = maybe (return ()) f m
