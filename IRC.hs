{-# LANGUAGE ExistentialQuantification, FlexibleInstances, GeneralizedNewtypeDeriving,
             MultiParamTypeClasses, TypeSynonymInstances, CPP, DeriveDataTypeable #-}

module IRC where

import Control.Applicative
import Control.Exception
import Control.Monad
import Control.Monad.Reader
import Control.Monad.Writer hiding (listen)
import Data.List
import Data.Maybe
import Data.Monoid
import Language.Haskell.HsColour.ANSI
import Network
import System.Exit
import System.IO
import System.Time
import Text.Printf

import IRC.Base
import IRC.Commands
import IRC.Parser

newtype Net a = Net (ReaderT Bot IO a)
    deriving (Functor, Applicative, Monad, MonadIO, MonadReader Bot)

type ManagedMessage = Message -> Processor ()

data Bot = Bot
    { socket    :: Handle
    , startTime :: !ClockTime
    , ops       :: [String]
    , nick'     :: String
    , chan      :: String
    , proc      :: ManagedMessage
    }

newtype Processor a = Processor (WriterT [Message] Net a)
    deriving (Functor, Monad, MonadIO, MonadReader Bot, MonadWriter [Message])

instance Monoid a => Monoid (Net a) where
    mempty  = return mempty
    mappend = liftM2 mappend

instance Monoid a => Monoid (Processor a) where
    mempty  = return mempty
    mappend = liftM2 mappend

hbot :: String -> Int -> String -> ManagedMessage -> IO ()
hbot server port nick p = bracket (connect server port nick p) disconnect loop
  where
    disconnect = hClose . socket
    loop       = runNet $ run nick "#bots"

connect :: String -> Int -> String -> ManagedMessage -> IO Bot
connect server port n p = bracket_ start end $ do
    t <- getClockTime
    h <- connectTo server . PortNumber $ fromIntegral port
    hSetBuffering h NoBuffering
    return $ Bot h t ["vodik"] n "#bots" p
  where
    start = printf "Connecting to %s ..." server >> hFlush stdout
    end   = putStrLn "done."

run :: String -> String -> Net ()
run n c = do
    write $ nick n
    write $ user n "0" "*" "tutorial bot"
    write $ joinChan c
    listen

myCmd msg@(Message p _ _) proc = ifNotProt msg . ifUser p $ proc msg

listen :: Net ()
listen = withSocket $ \h -> do
    proc <- asks proc
    forever $ do
        s <- io (hGetLine h)
        m <- io (decode (s ++ "\n"))
        case m of
            Nothing -> io . putStrLn . withHL2 $ init s
            Just msg -> do
                io . putStrLn . withHL1 $ init s
                t <- execProcessor $ myCmd msg proc
                mapM_ write t
  where
    withHL1   = highlight [ Foreground Blue ]
    withHL2   = highlight [ Foreground Red ]
    forever a = a >> forever a

runNet :: Net a -> Bot -> IO a
runNet (Net a) = runReaderT a

liftNet :: Net a -> Processor a
liftNet = Processor . lift

execProcessor :: Processor a -> Net [Message]
execProcessor (Processor a) = execWriterT a

io :: MonadIO m => IO a -> m a
io = liftIO

withSocket :: (Handle -> Net a) -> Net a
withSocket f = asks socket >>= f

(<+>) :: Monoid m => m -> m -> m
(<+>) = mappend

ifNotProt :: Message -> Processor () -> Processor ()
ifNotProt msg f = prot msg >>= \b -> unless b f

send :: Message -> Processor ()
send = tell . return

prot :: Message -> Processor Bool
prot msg@(Message _ "PING" xs) = send (pong msg) >> return True
prot _                         = return False

ifUser :: Maybe Prefix -> Processor () -> Processor ()
ifUser (Just (Nick u _ _)) f = asks ops >>= \o -> when (u `elem` o) f
ifUser _                   _ = return ()

exit :: Maybe String -> Net ()
exit msg = write (quit msg) >> io (exitWith ExitSuccess)

write :: Message -> Net ()
write t = withSocket $ \h -> do
    let msg = encode t
    io $ hPrintf h "%s\r\n" msg
    io $ printf    (withHL "> %s\n") msg
  where
    withHL = highlight [ Foreground Green ]
