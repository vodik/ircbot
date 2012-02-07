import Data.Monoid

import Control.Applicative
import Control.Monad.State
import Data.List

import IRC
import IRC.Base
import IRC.Commands
import Macros
import Stats

myServer = "irc.freenode.org"
myPort   = 6667
myNick   = "netopir"
myChans  = [ "#bots", "#derpgmzlj" ]

myProc stats msg@(Message p _ _) = case p of
    Just (Nick u _ _) -> (authorize u $ mconcat
        [ runMacros msg $ mconcat [ eval, ids ]
        ])
        <+> collectStats stats msg
    _ -> return ()

changeNick n = send (nick n) >> modify (\s -> s { nick' = n })

eval :: String -> String -> [String] -> Processor (Maybe String)
eval _ "uptime" _   = Just <$> liftNet uptime
eval _ "nick"   [n] = changeNick n >> return (Just $ "Now known as " ++ n)
eval _ "quit"   _   = liftNet (exit $ Just "Goodbye World") >> return Nothing
eval _ _        _   = return Nothing

ids :: String -> String -> [String] -> Processor (Maybe String)
ids _ "id"  msg = return . Just $ unwords msg
ids _ "id2" msg = return . Just $ show msg
ids u "ID"  msg = return . Just $ u ++ ": " ++ unwords msg
ids _ _     _   = return Nothing

main = do
    stats <- emptyStats
    hbot myServer myPort myNick myChans (myProc stats)
