{-# LANGUAGE OverloadedStrings #-}

module Bot.Sasl (authenticate) where

import Control.Arrow (first)
import Control.Applicative
import Control.Monad
import Data.Bits
import Data.ByteString.Char8 (ByteString)
import Data.List (unfoldr)
import Data.Serialize.Get
import Data.Serialize.Put
import Data.Word
import System.Entropy (getEntropy)
import qualified Codec.Encryption.Blowfish as Blowfish
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8

type Username = ByteString
type Password = ByteString

data Challenge = Plain
               | Challenge { keySize   :: Int
                           , prime     :: Integer
                           , generator :: Integer
                           , publicKey :: Integer
                           }

readChallenge :: ByteString -> Either String Challenge
readChallenge "+"   = Right Plain
readChallenge input = Base64.decode input >>= runGet challenge
  where
    challenge = do
        n <- fi <$> getWord16be
        Challenge n <$> getInteger n  -- read the prime number
                    <*> get           -- read the generator
                    <*> get           -- read the public key
    get = fi <$> getWord16be >>= getInteger

getInteger :: Int -> Get Integer
getInteger size = fromBytes <$> sequence [ getWord8 | _ <- [ 1 .. size ] ]

putInteger :: Int -> Integer -> Put
putInteger size n = do
    let size' = size * 8 - 8
    sequence_ [ putWord8 . fi $ n `shiftR` b | b <- [ size', size' - 8 .. 0 ] ]

putNullString :: ByteString -> Put
putNullString = putByteString . (`B8.snoc` '\0')

generateKey :: Int -> IO Integer
generateKey size = fromBytes . B.unpack <$> getEntropy (size - 1)

diffieHellman :: Challenge -> IO (Integer, Integer)
diffieHellman (Challenge n p g y) = do
    privKey <- generateKey n

    let public = powMod g privKey p
        secret = powMod y privKey p
    return (public, secret)

doChallenge :: Username -> Password -> Challenge -> IO ByteString
doChallenge user pass challenge = do
    (public, secret) <- diffieHellman challenge

    return . runPut $ do
        putWord16be . fi $ keySize challenge
        putInteger (keySize challenge) public     -- write our public key
        putNullString user                        -- write the username
        mapM_ putWord64be $ blowfish secret pass  -- write the crypted password

authenticate :: Username -> Password -> ByteString -> IO (Either String ByteString)
authenticate user pass input = case readChallenge input of
    Left  left  -> return $ Left left
    Right Plain -> return . Right . Base64.encode $ runPut plain
    Right right ->          Right . Base64.encode <$> doChallenge user pass right
  where
    plain = replicateM_ 2 (putNullString user) >> putNullString pass

powMod :: Integral a => a -> a -> a -> a
powMod _ 0 _    = 1
powMod a b m
    | even b    = (x * x)     `mod` m
    | otherwise = (x * x * a) `mod` m
  where
    x = powMod a (b `div` 2) m

blowfish :: Integral a => a -> ByteString -> [Word64]
blowfish key pass = Blowfish.encrypt key <$> chunk pass

chunk :: ByteString -> [Word64]
chunk = unfoldr g . B.unpack
  where
    g [] = Nothing
    g xs = Just . first fromBytes $ splitAt 8 xs

fromBytes :: (Num a, Integral a, Num b, Bits b) => [a] -> b
fromBytes = foldl (\a x -> a `shiftL` 8 .|. fi x) 0

fi :: (Integral a, Num b) => a -> b
fi = fromIntegral
