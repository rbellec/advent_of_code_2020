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
import           Data.List

fileName = "example.txt" -- "input.txt"

main :: IO ()
main = do
  fileContent <- Utf8.readFile fileName
  let parseResult = P.parse passeportsDefinitions "" fileContent
  case parseResult of
    Left e -> do
      putStrLn "Error :"
      pPrint e 
    Right passeportList -> do
      -- pPrint passeportList
      let validPasseportNumber = length $ filter passeportIsValid passeportList
      putStrLn ("nb of valid passeports: " ++ show validPasseportNumber)

-- ------------------------------------------------------------------------
-- validity handlers
-- byr (Birth Year)
-- iyr (Issue Year)
-- eyr (Expiration Year)
-- hgt (Height)
-- hcl (Hair Color)
-- ecl (Eye Color)
-- pid (Passport ID)
-- cid (Country ID)

allowedFields :: [String]
allowedFields =  ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid", "cid"]

requiredFields :: [String]
requiredFields = delete "cid" allowedFields 

passeportIsValid :: Passeport -> Bool
passeportIsValid passeport = 
  let 
    fields = map fst passeport
    haveField f = f `elem` fields
  in
    all haveField requiredFields

-- ------------------------------------------------------------------------
-- Parsers

type Passeport = [(String, String)] -- Assoc list
type Parser = P.Parsec Data.Void.Void T.Text

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