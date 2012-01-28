module Macros where

import Control.Applicative
import Control.Exception
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
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

ifPrivMsg :: Message -> (String -> [String] -> Processor (Maybe String)) -> Processor ()
ifPrivMsg (Message (Just (Nick u _ _)) "PRIVMSG" [c,xs]) f = gets nick' >>= \n -> do
    msg <- f u $ words xs
    case msg of
        Just m  -> send $ privmsg (if c == n then u else c) m
        Nothing -> return ()
ifPrivMsg _ _ = return ()

eval :: String -> [String] -> Processor (Maybe String)
eval _ ("!uptime":_) = Just <$> liftNet uptime
eval _ ("!nick":n:_) = liftNet (write $ nick n) >> modify (\s -> s { nick' = n }) >> return Nothing
eval _ ("!quit":_)   = liftNet (exit $ Just "Goodbye World") >> return Nothing
eval _ _             = return Nothing

ids :: String -> [String] -> Processor (Maybe String)
ids _ ("!id":msg) = return . Just $ unwords msg
ids u ("!ID":msg) = return . Just $ u ++ ": " ++ unwords msg
ids _ _           = return Nothing

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
