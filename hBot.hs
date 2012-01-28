import Data.Monoid

import IRC
import IRC.Base
import Macros

server = "irc.freenode.org"
port   = 6667
nick   = "netopir"
chans  = [ "#bots", "#derpgmzlj" ]

myProc msg = ifPrivMsg msg $ mconcat [ eval , ids ]

main = hbot server port nick chans myProc
