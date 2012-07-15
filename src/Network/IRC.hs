module Network.IRC
    ( (==?), toLower, (=?)
    , module IRC
    ) where

import Data.ByteString.Char8
import Data.Function (on)
import Network.IRC.Base as IRC
-- import Network.IRC.Commands as IRC
import Network.IRC.Encoder as IRC
import Network.IRC.Decoder as IRC
import qualified Data.ByteString.Char8 as B
import qualified Data.Char as Char (toLower)

(==?) :: ByteString -> ByteString -> Bool
(==?) = (==) `on` toLower

(=?) :: Message -> ByteString -> Bool
(=?) = (==) . command

toLower :: ByteString -> ByteString
toLower = B.map lower
  where
    -- See IRC RFC
    lower '['  = '{'
    lower ']'  = '}'
    lower '\\' = '|'
    lower '~'  = '^'
    lower x    = Char.toLower x
