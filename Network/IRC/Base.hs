module Network.IRC.Base where

import Data.ByteString.Char8
import qualified Data.ByteString.Char8 as B

type Parameter  = ByteString
type ServerName = ByteString
type NickName   = ByteString
type UserName   = ByteString
type RealName   = ByteString
type Command    = ByteString
type Channel    = ByteString

data Message = Message
    { prefix     :: Maybe Prefix
    , command    :: Command
    , parameters :: [Parameter]
    }
    deriving (Show)

data Prefix = ServerPrefix ServerName
            | NickPrefix NickName (Maybe UserName) (Maybe ServerName)
            deriving (Show)
