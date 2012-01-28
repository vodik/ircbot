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

import IRC.Parser

type Command = String

newtype Net a = Net (ReaderT Bot IO a)
    deriving (Functor, Monad, MonadIO, MonadReader Bot)

data Bot = Bot { socket :: Handle, startTime :: !ClockTime, chan :: String }

newtype Processor a = Processor (WriterT Command Net a)
    deriving (Functor, Monad, MonadIO, MonadWriter Command)

instance Monoid a => Monoid (Net a) where
    mempty  = return mempty
    mappend = liftM2 mappend

instance Monoid a => Monoid (Processor a) where
    mempty  = return mempty
    mappend = liftM2 mappend

hbot :: String -> Int -> String -> IO ()
hbot server port nick = bracket (connect server port) disconnect loop
  where
    disconnect = hClose . socket
    loop       = runNet $ run nick "#bots"

connect :: String -> Int -> IO Bot
connect server port = bracket_ start end $ do
    t <- getClockTime
    h <- connectTo server . PortNumber $ fromIntegral port
    hSetBuffering h NoBuffering
    return $ Bot h t "#bots"
  where
    start = printf "Connecting to %s ..." server >> hFlush stdout
    end   = putStrLn "done."

run :: String -> String -> Net ()
run nick chan = do
    write "NICK" nick
    write "USER" (nick ++ " 0 * :tutorial bot")
    write "JOIN" chan
    listen

listen :: Net ()
listen = withSocket $ \h ->
    forever $ do
        s <- io (hGetLine h)
        m <- io (decode (s ++ "\n"))
        case m of
            Nothing  -> io . putStrLn . withHL2 $ init s
            Just msg@(Message p _ xs) -> do
                io . putStrLn . withHL1 $ init s
                handled <- prot msg
                unless handled $ do
                    let cmd = ifUser "vodik" p (eval (tail xs) <+> ids p (words . head $ tail xs))
                    t <- execProcessor cmd
                    unless (null t) $ privmsg t
                    return ()
  where
    withHL1   = highlight [ Foreground Blue ]
    withHL2   = highlight [ Foreground Red ]
    forever a = a >> forever a

runNet :: Net a -> Bot -> IO a
runNet (Net a) = runReaderT a

liftNet :: Net a -> Processor a
liftNet = Processor . lift

-- runProcessor :: Processor a ->
execProcessor :: Processor a -> Net Command
execProcessor (Processor a) = execWriterT a

io :: MonadIO m => IO a -> m a
io = liftIO

withSocket :: (Handle -> Net a) -> Net a
withSocket f = asks socket >>= f

(<+>) :: Monoid m => m -> m -> m
(<+>) = mappend

prot :: Message -> Net Bool
prot (Message _ "PING" xs) = write "PONG" (unwords xs) >> return True
prot _                     = return False

eval :: [String] -> Processor ()
eval ("!uptime":_) = liftNet uptime >>= tell
eval ("!quit":_) = liftNet $ write "QUIT" ":Exiting" >> io (exitWith ExitSuccess)
eval _           = return ()

ifUser :: String -> Maybe Prefix -> Processor () -> Processor ()
ifUser x (Just (Nick u _ _)) f = when (x == u) f
ifUser _ _                   _ = return ()

ids :: Maybe Prefix -> [String] -> Processor ()
ids _                   ("!id":msg) = tell $ unwords msg
ids (Just (Nick u _ _)) ("!ID":msg) = tell $ u ++ ": " ++ unwords msg
ids _                   _           = return ()

privmsg :: String -> Net ()
privmsg s = do
    chan <- asks chan
    write "PRIVMSG" (chan ++ " :" ++ s)

write :: String -> String -> Net ()
write s t = withSocket $ \h -> do
    io $ hPrintf h "%s %s\r\n" s t
    io $ printf    (withHL "> %s %s\n") s t
  where
    withHL = highlight [ Foreground Green ]

uptime :: Net String
uptime = do
    now  <- io getClockTime
    zero <- asks startTime
    return . pretty $ diffClockTimes now zero

pretty :: TimeDiff -> String
pretty td = join . intersperse " " . filter (not . null) . map f $
    [ (years         , "y"), (months `mod` 12, "m")
    , (days  `mod` 28, "d"), (hours  `mod` 24, "h")
    , (mins  `mod` 60, "m"), (secs   `mod` 60, "s") ]
  where
    secs   = abs $ tdSec td
    mins   = secs   `div` 60
    hours  = mins   `div` 60
    days   = hours  `div` 24
    months = days   `div` 28
    years  = months `div` 12
    f (i,s) | i == 0    = []
            | otherwise = show i ++ s
