import Data.Monoid

import IRC
import IRC.Base
import Macros

server = "irc.freenode.org"
port   = 6667
nick   = "fbugsdf"

myProc msg@(Message p _ xs) = mconcat
    [ eval (tail xs)
    , ids p (words. head $ tail xs)
    ]

main = hbot server port nick myProc
