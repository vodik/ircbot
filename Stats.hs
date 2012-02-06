module Stats where

import Control.Applicative
import Control.Exception
import Control.Monad
import Control.Monad.Reader
import Control.Monad.Writer hiding (listen)
import Control.Monad.State
import Data.List
import qualified Data.Map as M
import Data.Maybe
import Data.Monoid
import Language.Haskell.HsColour.ANSI
import Network
import System.Exit
import System.IO
import System.Time
import Text.Printf
import Data.IORef

import IRC
import IRC.Base
import IRC.Commands

type Stats = IORef (M.Map String (M.Map String Int))

emptyStats :: IO Stats
emptyStats = newIORef $ M.empty

collectStats :: Stats -> Message -> Processor ()
collectStats env (Message (Just (Nick u _ _)) "PRIVMSG" [c,_]) =
    updateStats env c u
collectStats _   _ = return ()

updateStats :: Stats -> String -> String -> Processor ()
updateStats stats c n = do
    env <- liftIO $ readIORef stats
    let nickMap  = fromMaybe M.empty $ M.lookup c env
        nickMap' = M.insertWith (+) n 1 nickMap
        env'     = M.insert c nickMap' env
    liftIO $ writeIORef stats env'
    liftIO $ putStrLn "Got some stats! ----"
    liftIO $ putStrLn $ show env'
