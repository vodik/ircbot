module IRC.Base where

data Message = Message
    { prefix' :: Maybe Prefix
    , cmds    :: Command
    , args    :: [Parameter]
    } deriving (Eq, Read, Show)

data Prefix = Server ServerName
            | Nick String (Maybe UserName) (Maybe ServerName)
    deriving (Show,Read,Eq)

type Parameter  = String
type ServerName = String
type UserName   = String
type RealName   = String
type Command    = String

encode :: Message -> String
encode (Message p c ps) = showMaybe p ++ c ++ showParameters ps
  where showMaybe = maybe "" ((++ " ") . (':':) . showPrefix)

showPrefix :: Prefix -> String
showPrefix (Server s)   = s
showPrefix (Nick n u h) = n ++ showMaybe '!' u ++ showMaybe '@' h
  where showMaybe c = maybe "" (c:)

showParameters :: [Parameter] -> String
showParameters []     = []
showParameters params = ' ' : unwords (showp params)
  where showp [p] | ' ' `elem` p || null p || head p == ':' = [':' : p]
                  | otherwise = [p]
        showp (p:ps) = p : showp ps
        showp []     = []
