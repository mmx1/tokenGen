module Lexer
    ( lexMap,
      LexHgram
    ) where

import qualified Data.Text as T
import qualified Data.Map as M
import Text.Parsec
import Data.Char
import Data.List

type LexHgram = M.Map T.Text Int

lexMap :: T.Text -> LexHgram
lexMap t = case parse lexer "" t of
  (Left err) -> M.empty
  (Right l)  -> M.fromList . map (\x->(head x, length x)) . group . sort $ l

lexer::Parsec T.Text st [T.Text]
lexer = optional cDelimeter *> many (lexeme <* cDelimeter) <* eof

lexeme::Parsec T.Text st T.Text
lexeme = T.pack <$> many1 ( satisfy (not . isCDelimeter) )

cDelimeter:: Parsec T.Text st [Char]
cDelimeter = many1 $ satisfy isCDelimeter

isCDelimeter:: Char -> Bool
isCDelimeter = not. isAlpha
--isCDelimeter c = (isSpace c ) || (isPunctuation c)
