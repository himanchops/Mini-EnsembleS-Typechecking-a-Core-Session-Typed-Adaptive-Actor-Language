{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}



module Main (main) where

import Control.Applicative hiding (some)
import Control.Monad
import Data.Text (Text)
import Data.Void
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import Text.Megaparsec.Debug
import qualified Data.Text as T
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

data Scheme
  = SchemeData
  | SchemeFile
  | SchemeFtp
  | SchemeHttp
  | SchemeHttps
  | SchemeIrc
  | SchemeMailto
  deriving (Eq, Show)

pScheme :: Parser Scheme
pScheme = choice
  [ SchemeData   <$ string "data"
  , SchemeFile   <$ string "file"
  , SchemeFtp    <$ string "ftp"
  , SchemeHttps  <$ string "https"  
  , SchemeHttp   <$ string "http"
  , SchemeIrc    <$ string "irc"
  , SchemeMailto <$ string "mailto" ]


data Uri = Uri
  { uriScheme    :: Scheme
  , uriAuthority :: Maybe Authority
  } deriving (Eq, Show)

data Authority = Authority
  { authUser :: Maybe (Text, Text) -- (user, password)
  , authHost :: Text
  , authPort :: Maybe Int
  } deriving (Eq, Show)

pUri :: Parser Uri
pUri = do
  uriScheme <- pScheme <?> "valid scheme"
  void (char ':')
  uriAuthority <- optional $ do
    void (string "//")
    authUser <- optional . try $ do
      user <- T.pack <$> some alphaNumChar <?> "username"
      void (char ':')
      password <- T.pack <$> some alphaNumChar <?> "password"
      void (char '@')
      return (user, password)
    authHost <- T.pack <$> some (alphaNumChar <|> char '.') <?> "hostname"
    authPort <- optional (char ':' *> label "port number" L.decimal)
    return Authority {..}
  return Uri {..}

main :: IO ()
main = return ()