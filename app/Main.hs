{-# LANGUAGE OverloadedStrings #-}
module Main where

import Lexer
import qualified Data.Text as T
import qualified Data.Map.Lazy as M
import qualified Data.Text.IO as TIO
import System.Environment
import System.Directory
import System.Exit
import Control.Exception
import Control.Monad
import Crypto.Random.DRBG
import Control.Monad.CryptoRandom
import Data.Int
--import qualified Data.ByteString as B
import qualified Data.ByteString as B

main :: IO ()
main = readArg `catch` handler

readArg :: IO()
readArg = do
  (path:_) <- getArgs
  maps <-  M.filter (> 1) <$> readDir path
  --putStrLn $ show maps
  putStrLn $ "Generated a dictionary of size: " ++ (show $ M.size maps)
  r <- getRandom
  putStrLn $ show r

--genPassPhrase :: Int -> LexHgram

getRandom :: IO Int64
getRandom = do
  g <- newGenIO :: IO CtrDRBG
  case (crandom g :: Either GenError (Int64, CtrDRBG))of
    Left err -> (error $ show err) >> exitFailure
    Right (result, g2 ) -> return result



recReadDir :: FilePath -> IO LexHgram
recReadDir p = do
  dfe <- doesDirectoryExist p
  if dfe
    then readDir $ p ++ "/"
    else mapFile p

readDir :: FilePath ->  IO LexHgram
readDir p = (M.unionsWith (+) ) <$> (dirPaths p >>= (mapM recReadDir))

dirPaths :: FilePath -> IO [FilePath]
--listDirectory unavailable, drop .. and .  
dirPaths p = (map (p ++ ) )<$> (filter(\x -> head x /= '.')) <$> (dirContents p )


dirContents :: FilePath -> IO [FilePath]
dirContents p = do
  putStrLn $ "Examining directory: " ++ p
  contents <- try $ getDirectoryContents p
  case (contents :: Either IOError [FilePath]) of
    Left _ -> return []
    Right contents -> return contents 

mapFile :: FilePath -> IO LexHgram
mapFile p = do
  contents <- try $ TIO.readFile p
  case (contents :: Either IOError T.Text) of
    Left _ -> return M.empty 
    Right contents -> return $ lexMap contents

handler :: SomeException -> IO()
handler = putStrLn . show 