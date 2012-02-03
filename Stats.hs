module Stats where

import IRC
import IRC.Base
import IRC.Commands

collectStats :: Message -> Processor ()
collectStats (Message (Just (Nick u _ _)) "PRIVMSG" [c,_]) = return ()
collectStats _ = return ()
