module Main where

import Lexer
import qualified Data.Text as T
import qualified Data.Map as M
import qualified Data.Text.IO as TIO
import System.Environment
import System.Directory
import Control.Exception
import Control.Monad

main :: IO ()
main = readArg `catch` handler

readArg :: IO()
readArg = do
  (path:_) <- getArgs
  readDir path


--readDir :: FilePath ->  IO LexHgram
readDir p = do
  --listDirectory unavailable, drop .. and .	
  dirContents <-  filter (/= "..") <$> (filter(/= ".")) <$> (getDirectoryContents p )
  putStrLn $ show $ dirContents
  maps <- mapM mapFile dirContents
  putStrLn $ show maps
  --dirContents <- listDirectory p 
  --return mapM mapFile dirContents
--  return $ M.unionsWith (+) (mapM mapFile dirContents)

mapFile :: FilePath -> IO LexHgram
mapFile p = do
  contents <- TIO.readFile p
  return $ lexMap contents

handler :: SomeException -> IO()
handler = putStrLn . show 