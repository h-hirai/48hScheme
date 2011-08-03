module Main where

import Text.ParserCombinators.Parsec hiding (spaces)
import System
import Control.Monad
import Numeric (readInt)
import Data.Char (digitToInt)

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool
               deriving Show

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

parseString :: Parser LispVal
parseString = do char '"'
                 x <- many ((char '\\' >> oneOf "\"nrt\\") <|> noneOf "\"")
                 char '"'
                 return $ String x

parseAtom :: Parser LispVal
parseAtom = do first <- letter <|> symbol
               rest <- many (letter <|> digit <|> symbol)
               let atom = first:rest
               return $ case atom of
                          "#t" -> Bool True
                          "#f" -> Bool False
                          _ -> Atom atom

parseNumber :: Parser LispVal
parseNumber = liftM (Number . read) $ many1 digit

parseBinaryNumber :: Parser LispVal
parseBinaryNumber =
    do char '#' >> oneOf "bB"
       digits <- many1 $ oneOf "01"
       let [(n, _)] = readInt 2 (`elem` "01") digitToInt digits
       return $ Number n

parseExpr :: Parser LispVal
parseExpr = parseAtom
            <|> parseString
            <|> parseNumber

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
                   Left err -> "No match: " ++ show err
                   Right val -> "Found value: " ++ show val

main :: IO ()
main = do args <- getArgs
          putStrLn (readExpr (args!!0))
