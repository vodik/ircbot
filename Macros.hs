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

eval :: [String] -> Processor ()
eval ("!uptime":_) = liftNet uptime >>= \x -> send $ privmsg "#bots" x
eval ("!quit":_)   = liftNet . exit $ Just "!quit"
eval _             = return ()

ids :: Maybe Prefix -> [String] -> Processor ()
ids _                   ("!id":msg) = send . privmsg "#bots" $ unwords msg
ids (Just (Nick u _ _)) ("!ID":msg) = send . privmsg "#bots" $ u ++ ": " ++ unwords msg
ids _                   _           = return ()

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
