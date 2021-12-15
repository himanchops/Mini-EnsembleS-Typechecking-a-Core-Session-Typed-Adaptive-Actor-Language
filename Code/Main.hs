{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-#LANGUAGE ScopedTypeVariables#-}

import System.Environment as SE
import System.Exit

import Text.Megaparsec (runParser, errorBundlePretty)
import qualified Data.Text.IO as TI
import Parser (pProgram)
import TypeChecker (checkProgram)


-- Displays parsed output for input file
parsefromFile file = do
  input <- TI.readFile file
  case runParser pProgram "" input of
    Left s -> return $ errorBundlePretty s
    Right parsed -> return $ show parsed

-- Performs typechecking and returns errors
tc file = do
  input <- TI.readFile file
  case runParser pProgram "" input of
    Left s -> error $ errorBundlePretty s
    Right parsed ->
      case checkProgram parsed of
        Left e ->  return (show e)
        Right _ -> return "No type errors"

main = getArgs >>= argParse >>= putStrLn


argParse ["-h"]           = usage   >> exit
argParse ["-v"]           = version >> exit
argParse ["--parse", fs]  = parsefromFile fs
-- argParse ["--parse"]      = interact (unlines . parsefromInput) >> exit
argParse [fs]             = tc fs
argParse _                = usage   >> Main.die

usage   = putStrLn "Usage:\ttypecheck [-vh]\n\ttypecheck [--parse] [file]"
version = putStrLn "Haskell typecheck 0.1"
exit    = exitWith ExitSuccess
die     = exitWith (ExitFailure 1)