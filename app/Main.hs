
module Main where

import Lexer
import BNCParser
import Traverser
import Generator
import qualified Data.Text as T
import qualified Data.Map.Lazy as M
import qualified Data.Text.IO as TIO
import Data.Array
import Data.List
import Data.Ord
import System.Environment
import System.Directory
import Control.Exception
import Control.Monad
import qualified Data.ByteString as B
import Options.Applicative
import Text.Printf

main :: IO ()
main = execParser opts >>= run --`catch` handler
  where 
    opts = info (helper <*> opt)
      (fullDesc
      <> progDesc "Builds a vocabulary from the content of the specified path\n and creates a 2^n bit entropy password (default 75)"
      <> header "tokenGen - a token-based passphrase generator")
--main = readArg `catch` handler

run :: Options -> IO ()
run (Options path entropy fill detailed) = do
  if entropy < 40
    then do
      putStrLn $ (show entropy) ++ " is too weak of a password strength. Try something higher than 40"
      return()
    else do
      bnc <- getBNC
      dict <- getDict path
      when detailed $ compareDict bnc dict
      let nDict = fillDict fill dict bnc 
      makePassPhrase path nDict entropy

compareDict :: BNCMap -> LexHgram -> IO()
compareDict b l = do
  let lSize = M.size l
  -- get l sized leading chunk of b
  let smallB = M.fromList . (take lSize) . reverse $ sortBy (comparing snd) $ M.toList b
  let intersect = M.intersection smallB l
  let percent = 100 * ( fromIntegral $ M.size intersect) / (fromIntegral lSize)  
  printf "Your dictionary overlaps %.2f %% with the most common English Words \n" (percent :: Float)

fillDict :: Bool -> LexHgram -> BNCMap -> LexHgram
fillDict False l _ = l
fillDict True  l b = M.union l (M.fromList . (take n) . M.toList $ i)
  where i = M.difference b l
        n = 2^15 - (M.size l)




data Options = Options
  { path     :: String
  , entropy  :: Int
  , fill     :: Bool
  , detailed :: Bool}

opt :: Parser Options
opt = Options
  <$> strOption
      ( long "path"
     <> metavar "TARGET"
     <> help "Path TARGET to search for words" )
  <*> option auto 
      ( long "entropy"
     <> value 75
     <> short 'e'
     <> help "Number of bits of entropy desired" )
  <*> switch
      ( long "fill"
     <> short 'f'
     <> help "Fill up to 2^32 with the most common English words" )
  <*> switch
      ( long "detailed"
     <> short 'd'
     <> help "Perform detailed analysis" )

getDict::FilePath -> IO LexHgram
getDict path = do
  printf $ "Reading path " ++ path 
  dict <- M.filter (> 1) <$> readDir path
  printf "\n"
  --putStrLn $ "Read " ++ (show $ M.size dict) ++ " symbols"
  if (M.size dict) > 2^15
  then do
    putStrLn "Truncating to 2^15 most common"
    --return dict
    return $ M.fromList . (take (2^15)) . reverse $ sortBy (comparing snd) $ M.toList dict
  else return dict

makePassPhrase::FilePath -> LexHgram -> Int -> IO()
makePassPhrase path dict entropy = do
  let listArr  = hGramToList dict
  printPassPhrase entropy (M.size dict) listArr

printPassPhrase :: Int -> Int -> LexList -> IO()
printPassPhrase n arrSize arr = do
  let nTokens = numTokens arrSize n
  putStrLn $ "From a dictionary of size: " ++ (show arrSize)
  putStrLn $ "Requires " ++ (show nTokens)  ++ " tokens for 2^" ++ (show n) ++  " entropy"
  passPhrase <- genPassPhrase nTokens arrSize arr
  putStrLn $ show passPhrase

-- number of tokens from set size (param1) required for 2^(param2)
numTokens :: Int -> Int -> Int 
numTokens size entropy = ceiling $ (fromIntegral entropy )* logBase (fromIntegral size) 2 


handler :: SomeException -> IO()
handler = putStrLn . show 