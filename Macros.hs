module Macros where

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

import IRC
import IRC.Base
import IRC.Commands

ifPrivMsg :: Message -> (String -> String -> [String] -> Processor ()) -> Processor ()
ifPrivMsg (Message (Just (Nick u _ _)) "PRIVMSG" [c,xs]) f = f u c (words xs)
ifPrivMsg _ _ = return ()

eval :: String -> String -> [String] -> Processor ()
eval u c ("!uptime":_) = liftNet uptime >>= \x -> send $ privmsg (ch u c) x
eval _ c ("!quit":_)   = liftNet . exit $ Just "!quit"
eval _ c _             = return ()

ids :: String -> String -> [String] -> Processor ()
ids u c ("!id":msg) = send . privmsg (ch u c) $ unwords msg
ids u c ("!ID":msg) = send . privmsg (ch u c) $ u ++ ": " ++ unwords msg
ids _ c _           = return ()

ch u c = if c == "fbugsdf" then u else c

uptime :: Net String
uptime = pretty <$> (diffClockTimes <$> io getClockTime <*> asks startTime)

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
    f (i, s) | i == 0    = []
             | otherwise = show i ++ s
