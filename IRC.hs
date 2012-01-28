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
    deriving (Functor, Monad, MonadIO, MonadReader Bot)

data Bot = Bot
    { socket    :: Handle
    , startTime :: !ClockTime
    , ops       :: [String]
    , chan      :: String }

newtype Processor a = Processor (WriterT [Message] Net a)
    deriving (Functor, Monad, MonadIO, MonadReader Bot, MonadWriter [Message])

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
    return $ Bot h t ["vodik"] "#bots"
  where
    start = printf "Connecting to %s ..." server >> hFlush stdout
    end   = putStrLn "done."

run :: String -> String -> Net ()
run n c = do
    -- write "NICK" nick
    -- write "USER" (nick ++ " 0 * :tutorial bot")
    -- write "JOIN" chan
    write $ nick n
    write $ user n "0" "*" "tutorial bot"
    write $ joinChan c
    listen

myCmd msg@(Message p _ xs) = ifNotProt msg . ifUser p $ mconcat
    [ eval (tail xs)
    , ids p (words. head $ tail xs)
    ]

listen :: Net ()
listen = withSocket $ \h ->
    forever $ do
        s <- io (hGetLine h)
        m <- io (decode (s ++ "\n"))
        case m of
            Nothing -> io . putStrLn . withHL2 $ init s
            Just msg -> do
                io . putStrLn . withHL1 $ init s
                t <- execProcessor $ myCmd msg
                mapM_ write t
                return ()
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

eval :: [String] -> Processor ()
eval ("!uptime":_) = liftNet uptime >>= \x -> send $ privmsg "#bots" x
eval ("!quit":_)   = liftNet . exit $ Just "!quit"
eval _             = return ()

ifUser :: Maybe Prefix -> Processor () -> Processor ()
ifUser (Just (Nick u _ _)) f = asks ops >>= \o -> when (u `elem` o) f
ifUser _                   _ = return ()

ids :: Maybe Prefix -> [String] -> Processor ()
ids _                   ("!id":msg) = send . privmsg "#bots" $ unwords msg
ids (Just (Nick u _ _)) ("!ID":msg) = send . privmsg "#bots" $ u ++ ": " ++ unwords msg
ids _                   _           = return ()

-- privmsg' :: String -> Net ()
-- privmsg' s = do
--     chan <- asks chan
--     write "PRIVMSG" (chan ++ " :" ++ s)

-- write :: String -> String -> Net ()
-- write s t = withSocket $ \h -> do
--     io $ hPrintf h "%s %s\r\n" s t
--     io $ printf    (withHL "> %s %s\n") s t
--   where
--     withHL = highlight [ Foreground Green ]

exit :: Maybe String -> Net ()
exit msg = write (quit msg) >> io (exitWith ExitSuccess)

write :: Message -> Net ()
write t = withSocket $ \h -> do
    let msg = encode t
    io $ hPrintf h "%s\r\n" msg
    io $ printf    (withHL "> %s\n") msg
  where
    withHL = highlight [ Foreground Green ]

uptime :: Net String
uptime = do
    now  <- io getClockTime
    zero <- asks startTime
    return . pretty $ diffClockTimes now zero

pretty :: TimeDiff -> String
pretty td = join . intersperse " " . filter (not . null) . fmap f $
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
