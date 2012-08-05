module Network.IRC
    ( (==?), lower, (=?)
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

instance Show Message where
    show = B.unpack . prettyPrint

(==?) :: ByteString -> ByteString -> Bool
(==?) = (==) `on` B.map lower

(=?) :: Message -> ByteString -> Bool
(=?) = (==) . command

-- See IRC RFC
lower :: Char -> Char
lower '['  = '{'
lower ']'  = '}'
lower '\\' = '|'
lower '~'  = '^'
lower x    = Char.toLower x
