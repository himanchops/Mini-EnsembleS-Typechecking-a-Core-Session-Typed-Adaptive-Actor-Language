import Text.Megaparsec
import Text.Megaparsec.Char
import Prelude
import Control.Monad
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Applicative

lineComment :: Parser ()
lineComment = L.skipLineComment "--"

scn :: Parser ()
scn = L.space (void spaceChar) lineComment empty

sc :: Parser ()
sc = L.space (void $ oneOf " \t") lineComment empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

reserved :: String -> Parser ()
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