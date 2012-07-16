{-# LANGUAGE OverloadedStrings #-}

module Network.IRC.Commands where

import Data.Maybe (maybe)
import Data.Monoid ((<>))
import Data.ByteString.Char8
import Network.IRC.Base

mkMessage :: Command -> [Parameter] -> Message
mkMessage = Message Nothing

pong :: Message -> Message
pong m = mkMessage "PONG" (parameters m)

nick :: UserName -> Message
nick u = mkMessage "NICK" [u]

user :: UserName -> ServerName -> ServerName -> RealName -> Message
user u h s r = mkMessage "USER" [u, h, s, r]

joinChan :: Channel -> Message
joinChan c = mkMessage "JOIN" [c]

part :: Channel -> Maybe ByteString -> Message
part c = mkMessage "PART" . ([c] <>) . maybe [] return

quit :: Maybe ByteString -> Message
quit = mkMessage "QUIT" . maybe [] return

privmsg :: Channel -> ByteString -> Message
privmsg c m = mkMessage "PRIVMSG" [c, m]

kick :: Channel -> UserName -> Maybe ByteString -> Message
kick c u = mkMessage "KICK" . ([c, u] <>) . maybe [] return
