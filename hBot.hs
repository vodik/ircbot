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
    asks socket >>= listen

listen :: Handle -> Net ()
listen h = forever $ do
    s <- init `fmap` io (hGetLine h)
    io $ putStrLn s
    if ping s
       then pong s
       else eval $ clean s
    where
        forever a = a >> forever a
        clean     = drop 1 . dropWhile (/= ':') . drop 1
        ping x    = "PING :" `isPrefixOf` x
        pong x    = write "PONG" (':' : drop 6 x)

eval :: String -> Net ()
eval     "!uptime"             = uptime >>= privmsg
eval     "!quit"               = write "QUIT" ":Exiting" >> io (exitWith ExitSuccess)
eval x | "!id " `isPrefixOf` x = privmsg (drop 4 x)
eval     _                     = return ()

privmsg :: String -> Net ()
privmsg s = write "PRIVMSG" (chan ++ " :" ++ s)

write :: String -> String -> Net ()
write s t = do
    h <- asks socket
    io $ hPrintf h "%s %s\r\n" s t
    io $ printf    "> %s %s\n" s t

uptime :: Net String
uptime = do
    now  <- io getClockTime
    zero <- asks startTime
    return . pretty $ diffClockTimes now zero

pretty :: TimeDiff -> String
pretty td = join . intersperse " " . filter (not . null) . map f $
    [(years          ,"y") ,(months `mod` 12,"m")
    ,(days   `mod` 28,"d") ,(hours  `mod` 24,"h")
    ,(mins   `mod` 60,"m") ,(secs   `mod` 60,"s")]
    where
        secs    = abs $ tdSec td  ; mins   = secs   `div` 60
        hours   = mins   `div` 60 ; days   = hours  `div` 24
        months  = days   `div` 28 ; years  = months `div` 12
        f (i,s) | i == 0    = []
                | otherwise = show i ++ s

io :: IO a -> Net a
io = liftIO
