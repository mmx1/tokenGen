
module Main where

import Lexer
import BNCParser
import Traverser
import Generator
import qualified Data.Text as T
import qualified Data.Map.Lazy as M
import qualified Data.Text.IO as TIO
import Data.Array
import System.Environment
import System.Directory
import Control.Exception
import Control.Monad
import qualified Data.ByteString as B

main :: IO ()
main = readArg `catch` handler

readArg :: IO()
readArg = do
  (path:_) <- getArgs
  readBNC
  makePassPhrase path


makePassPhrase::FilePath -> IO()
makePassPhrase path = do
  maps <-  M.filter (> 1) <$> readDir path
  --putStrLn $ show maps
  putStrLn $ "Generated a dictionary of size: " ++ (show $ M.size maps)
  let listArr  = hGramToList maps
  printPassPhrase 128 (M.size maps) listArr
  printPassPhrase 75 (M.size maps) listArr

printPassPhrase :: Int -> Int -> LexList -> IO()
printPassPhrase n arrSize arr = do
  let nTokens = numTokens arrSize n
  putStrLn $ "Requires " ++ (show nTokens)  ++ " tokens for 2^" ++ (show n) ++  " entropy"
  passPhrase <- genPassPhrase nTokens arrSize arr
  putStrLn $ show passPhrase

-- number of tokens from set size (param1) required for 2^(param2)
numTokens :: Int -> Int -> Int 
numTokens size entropy = ceiling $ (fromIntegral entropy )* logBase (fromIntegral size) 2 


handler :: SomeException -> IO()
handler = putStrLn . show 