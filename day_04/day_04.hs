{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.List
import qualified Text.Megaparsec               as P
import qualified Text.Megaparsec.Char          as C
import qualified Data.Text.IO.Utf8             as Utf8

import           Control.Monad.Combinators     ( (<|>), sepEndBy1, sepEndBy)
import qualified Data.Text                     as T
import qualified Data.Void                     
import           Data.Functor ( ($>))

import           Text.Show.Pretty               ( pPrint )

fileName = "example.txt" -- "input.txt"

main :: IO ()
main = do
  fileContent <- Utf8.readFile fileName
  let passeportList = P.parse passeportsDefinitions "" fileContent
  pPrint passeportList

--

type Passeport = [(String, String)] -- Assoc list
type Parser = P.Parsec Data.Void.Void T.Text

-- Parsers
passeportsDefinitions :: Parser [Passeport]
passeportsDefinitions = sepEndBy passeport emptyLine

newline ::  Parser ()
newline = C.char '\n' $> ()

emptyLine :: Parser ()
emptyLine =  do 
              newline
              newline 

-- Whitespaces including newline except blank line
fieldSeparator :: Parser ()
fieldSeparator =  C.char ' ' $> () 
              <|> P.try (newline *> P.notFollowedBy newline)

passeport :: Parser Passeport
passeport = sepEndBy1 passeportField fieldSeparator

passeportField :: Parser (String, String)
passeportField = do
  fieldName <- allowedString
  _ <- C.char ':' -- separator
  fieldValue <- allowedString
  return (fieldName, fieldValue)

allowedString :: Parser String
allowedString =  P.many (C.alphaNumChar <|> C.char '#')