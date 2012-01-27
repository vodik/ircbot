{-# LANGUAGE ExistentialQuantification, FlexibleInstances, GeneralizedNewtypeDeriving,
             MultiParamTypeClasses, TypeSynonymInstances, CPP, DeriveDataTypeable #-}

import Control.Applicative
import Control.Exception
import Control.Monad
import Control.Monad.Reader
import Control.Monad.Writer hiding (listen)
import Data.List
import Data.Monoid
import Language.Haskell.HsColour.ANSI
import Network
import System.Exit
import System.IO
import System.Time
import Text.Printf

server = "irc.freenode.org"
port   = 6667
chan   = "#bots"
nick   = "fbugsdf"

newtype Net a = Net (ReaderT Bot IO a)
    deriving (Functor, Monad, MonadIO, MonadReader Bot)

data Bot = Bot { socket :: Handle, startTime :: !ClockTime }

type Command = String

newtype Processor a = Processor (WriterT Command Net a)
    deriving (Functor, Monad, MonadIO, MonadWriter Command)

instance Monoid a => Monoid (Net a) where
    mempty  = return mempty
    mappend = liftM2 mappend

instance Monoid a => Monoid (Processor a) where
    mempty  = return mempty
    mappend = liftM2 mappend

runNet :: Net a -> Bot -> IO a
runNet (Net a) = runReaderT a

liftNet :: Net a -> Processor a
liftNet = Processor . lift

-- runProcessor :: Processor a ->
execProcessor :: Processor a -> Net Command
execProcessor (Processor a) = execWriterT a

io :: MonadIO m => IO a -> m a
io = liftIO

main :: IO ()
main = bracket connect disconnect loop
  where
    disconnect = hClose . socket
    loop       = runNet run

connect :: IO Bot
connect = bracket_ start end $ do
    t <- getClockTime
    h <- connectTo server . PortNumber $ fromIntegral port
    hSetBuffering h NoBuffering
    return $ Bot h t
  where
    start = printf "Connecting to %s ..." server >> hFlush stdout
    end   = putStrLn "done."

run :: Net ()
run = do
    write "NICK" nick
    write "USER" (nick ++ " 0 * :tutorial bot")
    write "JOIN" chan
    listen

withSocket :: (Handle -> Net a) -> Net a
withSocket f = asks socket >>= f

(<+>) :: Monoid m => m -> m -> m
(<+>) = mappend

listen :: Net ()
listen = withSocket $ \h ->
    forever $ do
        s <- init <$> io (hGetLine h)
        io . putStrLn $ withHL s
        handled <- prot $ words s
        unless handled $ do
            let u   = user s
                xs  = words $ clean s
                cmd = eval xs <+> ifUser "vodik" u $ ids u xs
            t <- execProcessor cmd
            unless (null t) $ privmsg t
            return ()
  where
    withHL    = highlight [ Foreground Red ]
    forever a = a >> forever a
    clean     = drop 1 . dropWhile (/= ':') . drop 1
    user      = drop 1 . takeWhile (/= '!')

prot :: [String] -> Net Bool
prot ("PING":xs) = write "PONG" (unwords xs) >> return True
prot _           = return False

eval :: [String] -> Processor ()
eval ("!uptime":_) = liftNet uptime >>= tell
eval ("!quit":_)   = liftNet $ write "QUIT" ":Exiting" >> io (exitWith ExitSuccess)
eval _             = return ()

ifUser :: String -> String -> Processor () -> Processor ()
ifUser x u = when (x == u)

ids :: String -> [String] -> Processor ()
ids _ ("!id":msg) = tell $ unwords msg
ids u ("!ID":msg) = tell $ u ++ ": " ++ unwords msg
ids _ _           = return ()

privmsg :: String -> Net ()
privmsg s = write "PRIVMSG" (chan ++ " :" ++ s)

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
