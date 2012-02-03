module Macros where

import Control.Applicative hiding ((<|>), many)
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

import Text.Parsec.String
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.Prim

import IRC
import IRC.Base
import IRC.Commands

tokenize :: (Monad m) => ParsecT String u m a -> ParsecT String u m a
tokenize p = p >>= \x -> spaces *> return x

direct :: (Monad m) => String -> ParsecT String u m Bool
direct n = try (string n *> oneOf ":," *> spaces *> return True)

bang :: (Monad m) => ParsecT String u m Bool
bang = char '!' *> return True

command' :: (Monad m) => String -> ParsecT String u m (String, [String])
command' n = do
    (direct n <|> bang) >>= \p -> unless p $ fail ""
    cmd <- many $ noneOf " "
    arg <- optionMaybe $ char ' ' *> many (noneOf " ") `sepEndBy1` char ' '
    return (cmd, fromMaybe [] arg)

command :: String -> String -> Maybe (String, [String])
command n = either (const Nothing) Just . parse (command' n) ""

respond :: String -> Maybe String -> Processor ()
respond c (Just m) = send $ privmsg c m
respond _ Nothing  = return ()

channel :: String -> String -> String -> String
channel c u n = if c == n then u else c

runMacros :: Message -> (String -> String -> [String] -> Processor (Maybe String)) -> Processor ()
runMacros (Message (Just (Nick u _ _)) "PRIVMSG" [c,xs]) f =
    gets nick' >>= \n ->
    case command n xs of
        Nothing        -> return ()
        Just (cmd,arg) -> f u cmd arg >>= respond (channel c u n)
runMacros _ _ = return ()

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
