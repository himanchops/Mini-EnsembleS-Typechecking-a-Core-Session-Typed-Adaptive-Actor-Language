{-# LANGUAGE OverloadedStrings #-}

import Data.Text (Text)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L -- (1)
import qualified Control.Monad.Combinators.Expr as E

type Parser = Parsec Void Text

sc :: Parser ()                  -- Space Consumer
sc = L.space
  space1                         -- (2)
  (L.skipLineComment "//")       -- (3)
  (L.skipBlockComment "/*" "*/") -- (4)

lexeme :: Parser a -> Parser a   -- Trailing space
lexeme = L.lexeme sc

symbol :: Text -> Parser Text    -- matches given text
symbol = L.symbol sc

charLiteral :: Parser Char       -- 'c'
charLiteral = between (char '\'') (char '\'') L.charLiteral

stringLiteral :: Parser String   -- "abc"
stringLiteral = char '\"' *> manyTill L.charLiteral (char '\"')

integer :: Parser Integer        
integer = lexeme L.decimal

signedInteger :: Parser Integer
signedInteger = L.signed sc integer

signedFloat :: Parser Double
signedFloat = L.signed sc L.float

reserved :: Text -> Parser()
reserved w = string w *> notFollowedBy alphaNumChar *> sc

rws = ["continue", "let", "in"]

identifier :: Parser String
identifier = (lexeme . try) (p >>= check)
 where
   p       = (:) <$> letterChar <*> many alphaNumChar
   check x =
     if x `elem` rws
     then fail $ "keyword " ++ show x ++ " cannot be an identifier"
     else return x



data Expr
  = Var String
  | Int Int
  | Negation Expr
  | Sum      Expr Expr
  | Subtr    Expr Expr
  | Product  Expr Expr
  | Division Expr Expr
  deriving (Eq, Ord, Show)

pVariable :: Parser Expr
pVariable = Var <$> lexeme
  ((:) <$> letterChar <*> many alphaNumChar <?> "variable")

pInteger :: Parser Expr
pInteger = Int <$> lexeme L.decimal

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

pTerm :: Parser Expr
pTerm = choice
  [ parens pExpr
  , pVariable
  , pInteger
  ]

pExpr :: Parser Expr
pExpr = E.makeExprParser pTerm operatorTable

data Operator m a -- N.B.
  = InfixN  (m (a -> a -> a)) -- ^ Non-associative infix
  | InfixL  (m (a -> a -> a)) -- ^ Left-associative infix
  | InfixR  (m (a -> a -> a)) -- ^ Right-associative infix
  | Prefix  (m (a -> a))      -- ^ Prefix
  | Postfix (m (a -> a))      -- ^ Postfix

operatorTable :: [[E.Operator Parser Expr]]
operatorTable =
  [ [ prefix "-" Negation
    , prefix "+" id
    ]
  , [ binary "*" Product
    , binary "/" Division
    ]
  , [ binary "+" Sum
    , binary "-" Subtr
    ]
  ]

binary :: Text -> (Expr -> Expr -> Expr) -> E.Operator Parser Expr
binary  name f = E.InfixL  (f <$ symbol name)

prefix, postfix :: Text -> (Expr -> Expr) -> E.Operator Parser Expr
prefix  name f = E.Prefix  (f <$ symbol name)
postfix name f = E.Postfix (f <$ symbol name)