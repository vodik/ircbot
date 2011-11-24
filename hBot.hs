import Data.List
import Network
import System.IO
import System.Time
import System.Exit
import Control.Monad.Reader
import Control.OldException
import Text.Printf
import Prelude hiding (catch)

server = "irc.freenode.org"
port   = 6667
chan   = "#bots"
nick   = "fbugsdf"

type Net = ReaderT Bot IO
data Bot = Bot { socket :: Handle, startTime :: ClockTime }

main :: IO ()
main = bracket connect disconnect loop
    where
        disconnect = hClose . socket
        loop st    = catch (runReaderT run st) (const $ return ())

connect :: IO Bot
connect = notify $ do
    t <- getClockTime
    h <- connectTo server $ PortNumber $ fromIntegral port
    hSetBuffering h NoBuffering
    return $ Bot h t
    where
        notify a = bracket_ (printf "Connecting to %s ..." server >> hFlush stdout)
                            (putStrLn "done.")
                            a

run :: Net ()
run = do
    write "NICK" nick
    write "USER" (nick ++ " 0 * :tutorial bot")
    write "JOIN" chan
    listen

listen :: Net ()
listen = do
    h <- asks socket
    forever $ do
        s <- init `fmap` liftIO (hGetLine h)
        liftIO $ putStrLn s
        handled <- prot $ words s
        if handled
           then return ()
           else eval (user s) (words $ clean s)
    where
        forever a = a >> forever a
        clean     = drop 1 . dropWhile (/= ':') . drop 1
        user      = drop 1 . takeWhile (/= '!')

prot :: [String] -> Net Bool
prot ("PING":xs)   = write "PONG" (unwords xs) >> return True
prot _             = return False

eval :: String -> [String] -> Net ()
eval _ ("PING":xs)   = write "PONG" (unwords xs)
eval _ ("!uptime":_) = uptime >>= privmsg
eval _ ("!quit":_)   = write "QUIT" ":Exiting" >> liftIO (exitWith ExitSuccess)
eval _ ("!id":msg)   = privmsg $ unwords msg
eval u ("!ID":msg)   = privmsg $ u ++ ": " ++ unwords msg
eval _ _             = return ()

privmsg :: String -> Net ()
privmsg s = write "PRIVMSG" (chan ++ " :" ++ s)

write :: String -> String -> Net ()
write s t = do
    h <- asks socket
    liftIO $ hPrintf h "%s %s\r\n" s t
    liftIO $ printf    "> %s %s\n" s t

uptime :: Net String
uptime = do
    now  <- liftIO getClockTime
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
