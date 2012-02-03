module IRC.Commands where

import IRC.Base

type Channel  = String
type Password = String

mkMessage :: String -> [Parameter] -> Message
mkMessage = Message Nothing

pong :: String -> Message
pong m = mkMessage "PONG" [m]

nick :: UserName -> Message
nick u = mkMessage "NICK" [u]

user :: UserName -> ServerName -> ServerName -> RealName -> Message
user u h s r = mkMessage "USER" [u, h, s, r]

joinChan :: Channel -> Message
joinChan c = mkMessage "JOIN" [c]

part :: Channel -> Message
part c = mkMessage "PART" [c]

quit :: Maybe String -> Message
quit (Just m) = mkMessage "QUIT" [m]
quit Nothing  = mkMessage "QUIT" []

privmsg :: String -> String -> Message
privmsg c m = mkMessage "PRIVMSG" [c, m]
