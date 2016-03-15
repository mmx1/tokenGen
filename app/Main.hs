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
dirPaths p = (map (p ++ )) <$> filter (/= "..") <$> (filter(/= ".")) <$> (dirContents p )

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