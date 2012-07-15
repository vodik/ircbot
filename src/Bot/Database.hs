{-# LANGUAGE Rank2Types #-}

import Database.HDBC
import Database.HDBC.Sqlite3
import Irc

-- withSql :: (forall c. IConnection c => c -> IO a) -> Irc a
-- -- withSql f = do
--     -- conn <- asks $ db . bot
--     io $ do
--         x <- f conn
--         commit conn
--         return x
