module BNCParser
    ( getBNC
    ) where

import qualified Data.Text as T
import qualified Data.Map as M
import qualified Data.Text.IO as TIO
import System.Exit
import Text.Parsec
import Data.Char
import Data.List
import Data.Text.Read
import Data.Tuple
import Data.Ord

type BNCMap = M.Map T.Text Int -- maps words to frequency /100M in British National Corpus


getBNC :: IO BNCMap
getBNC = do
  contents <- TIO.readFile "alls.num"
  let l = T.lines contents
  --putStrLn $ show l
  let m = M.fromList $ (take (2^15)) . reverse $ sortBy (comparing fst) (map parseLineW l)
  --putStrLn $ show m
  return m

bncMap :: T.Text -> BNCMap
bncMap t = case parse parseBNC "" t of
  (Left err) -> M.empty
  (Right l)  -> M.fromList l

parseLineW::T.Text -> (Int, T.Text)
parseLineW t = case parse parseLine "" t of
  (Left err) -> (0, T.empty) -- zero occurence will fall off the end of the map
  (Right l)  -> l

parseBNC:: Parsec T.Text st [(T.Text, Int)]
parseBNC = map swap  <$> many (parseLine <*  optional spaces)--`sepBy` space--many (parseLine <* skipMany1 spaces)

parseLine::Parsec T.Text st (Int, T.Text)
parseLine = (,) <$> (parseInt <* space) <*> (parseWord) 

eol :: Parsec T.Text st String
eol =   try (string "\n\r")
    <|> try (string "\r\n")
    <|> string "\n"
    <|> string "\r"
    <?> "eol"

parseInt :: Parsec T.Text st Int
parseInt = read <$> many1 digit

parseWord :: Parsec T.Text st T.Text
parseWord = T.pack <$> many1 (satisfy isLetter)


