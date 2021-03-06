module Main where

import Text.ParserCombinators.Parsec hiding (spaces)
import System

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

readExpr :: String -> String
readExpr input = case parse symbol "lisp" input of
                   Left err -> "No match: " ++ show err
                   Right val -> "Found value"

main :: IO ()
main = do args <- getArgs
          putStrLn (readExpr (args!!0))
