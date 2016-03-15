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
  maps <-  readDir path
  putStrLn $ show maps
  putStrLn $ show $ M.size maps


readDir :: FilePath ->  IO LexHgram
readDir p = (M.unionsWith (+) ) <$> (dirPaths p >>= (mapM mapFile))

dirPaths :: FilePath -> IO [FilePath]
--listDirectory unavailable, drop .. and .	
dirPaths p = (map (p ++ )) <$> filter (/= "..") <$> (filter(/= ".")) <$> (getDirectoryContents p )

mapFile :: FilePath -> IO LexHgram
mapFile p = do
  contents <- try $ TIO.readFile p
  case (contents :: Either IOError T.Text) of
    Left _ -> return M.empty 
    Right contents -> return $ lexMap contents

handler :: SomeException -> IO()
handler = putStrLn . show 