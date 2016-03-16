
module Main where

import Lexer
import Traverser
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
import qualified Data.ByteString as B

main :: IO ()
main = readArg `catch` handler

readArg :: IO()
readArg = do
  (path:_) <- getArgs
  maps <-  M.filter (> 1) <$> readDir path
  --putStrLn $ show maps
  putStrLn $ "Generated a dictionary of size: " ++ (show $ M.size maps)
  putStrLn $ "Requires " ++ (show $ numTokens (M.size maps) 128)  ++ " tokens for 2^128 entropy"
  putStrLn $ "Requires " ++ (show $ numTokens (M.size maps) 75)  ++ " tokens for 2^75 entropy"

-- number of tokens from set size (param1) required for 2^(param2)
numTokens :: Int -> Int -> Int 
numTokens size entropy = ceiling $ (fromIntegral entropy )* logBase (fromIntegral size) 2 

--genPassPhrase :: Int -> LexHgram -> IO[String]
genPassPhrase = do
  r <- getRandom
  putStrLn $ show r

getRandom :: IO Int64
getRandom = do
  g <- newGenIO :: IO CtrDRBG
  case (crandom g :: Either GenError (Int64, CtrDRBG))of
    Left err -> (error $ show err) >> exitFailure
    Right (result, g2 ) -> return result


handler :: SomeException -> IO()
handler = putStrLn . show 