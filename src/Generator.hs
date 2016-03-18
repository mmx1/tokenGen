{-# LANGUAGE OverloadedStrings #-}
module Generator
    ( genPassPhrase,
      hGramToList,
      LexList
    ) where

import Lexer
import Crypto.Random.DRBG
import Control.Monad.CryptoRandom
import System.Exit
import Data.Int
import Data.Array 
import qualified Data.Text as T
import qualified Data.Map.Lazy as M

type LexList = Array Int T.Text

hGramToList :: LexHgram -> LexList
hGramToList m = listArray (0, M.size m) ( M.keys $ m)

genPassPhrase :: Int ->  Int -> LexList -> IO [T.Text]
genPassPhrase n size array = sequence $ loop [] n size array
    where loop acc n' size array'  | n' > 0 = loop  (acc ++ [genWord size array]) (n'-1) size array'
                                   | otherwise = acc

genWord :: Int -> LexList -> IO T.Text
--genWord _ = return $ T.pack "test"
genWord n l = do
  r <- getRandom
  return (l!(r `mod` n))
  -- r `mod` n

--genPassPhrase :: Int -> LexHgram -> IO[String]
--genPassPhrase = do
--  r <- getRandom
--  putStrLn $ show r

getRandom :: IO Int
getRandom = do
  g <- newGenIO :: IO CtrDRBG
  case (crandom g :: Either GenError (Int, CtrDRBG))of
    Left err -> (error $ show err) >> exitFailure
    Right (result, g2 ) -> return result