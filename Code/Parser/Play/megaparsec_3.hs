{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module Main (main) where

import Control.Applicative hiding (some, many)
import Control.Monad (void)
import Data.Text (Text)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

lineComment :: Parser ()
lineComment = L.skipLineComment "--"

scn :: Parser ()
scn = L.space space1 lineComment empty

sc :: Parser ()
sc = L.space (void $ some (char ' ' <|> char '\t')) lineComment empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

reserved :: Text -> Parser()
reserved w = string w *> notFollowedBy alphaNumChar *> sc

rws = ["let", "in"]

identifier :: Parser String
identifier = (lexeme . try) (p >>= check)
 where
   p       = (:) <$> letterChar <*> many alphaNumChar
   check x =
     if x `elem` rws
     then fail $ "keyword " ++ show x ++ " cannot be an identifier"
     else return x

data Expr =
  Var String |
  Apply String [String] |
  Let String Expr Expr
  deriving (Eq, Ord, Show)

var :: Parser Expr
var = Var <$> identifier

apply = Apply <$> identifier <*> some identifier

letExpr = do
  try $ reserved "let"
  name <- identifier
  reserved "="
  val <- try apply <|> var
  reserved "in"
  body <- expr
  return $ Let name val body


expr = letExpr <|> try apply <|> var

parseMega = parseTest (expr <* eof)


main :: IO ()
main = return ()

-- pItem :: Parser String
-- pItem = lexeme (some (alphaNumChar <|> char '-')) <?> "list item"


-- pComplexItem :: Parser (String, [String])
-- pComplexItem = L.indentBlock scn p
--   where
--     p = do
--       header <- pItem
--       return (L.IndentMany Nothing (return . (header, )) pLineFold)

-- pLineFold :: Parser String
-- pLineFold = L.lineFold scn $ \sc' ->
--   let ps = some (alphaNumChar <|> char '-') `sepBy1` try sc'
--   in unwords <$> ps <* scn


-- pItemList :: Parser (String, [(String, [String])])
-- pItemList = L.nonIndented scn (L.indentBlock scn p)
--   where
--     p = do
--       header <- pItem
--       return (L.IndentSome Nothing (return . (header, )) pComplexItem)