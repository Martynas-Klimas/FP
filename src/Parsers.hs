{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Parsers
    ( query,
    Parser,
    parse, 
    Query(..),
    Guitar(..),
    Amplifier(..),
    Accessory(..),
    Item(..),
    isGuitar,
    showItem,
    parseString
    ) where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except
import Control.Monad.State
import Data.Char (isAlpha, isDigit)
import Data.List (takeWhile, dropWhile)
import Control.Applicative (Alternative, (<|>))

newtype Parser a = Parser { parse :: ExceptT String (State String) a }

data Query = 
    AddGuitar Guitar |
    AddAmplifier Amplifier |
    AddAccessory Accessory |
    ViewInventory |
    TestGuitars
    deriving(Show, Eq)

data Item = GuitarItem Guitar | AmplifierItem Amplifier | AccessoryItem Accessory
    deriving (Show, Eq)

data Guitar = GuitarData {
    guitarId        :: Int,
    guitarName      :: String,
    guitarStock     :: Int,
    guitarPrice     :: Int,
    guitarType      :: String,
    relatedGuitar   :: Maybe Guitar 
  } deriving (Show, Eq)

data Amplifier = AmplifierData {
    amplifierId        :: Int,
    amplifierName      :: String,
    amplifierStock     :: Int,
    amplifierPrice     :: Int,
    amplifierType      :: String,
    relatedAmplifier   :: Maybe Amplifier 
  } deriving (Show, Eq)

data Accessory = AccessoryData {
    accessoryId        :: Int,
    accessoryName      :: String,
    accessoryStock     :: Int,
    accessoryPrice     :: Int,
    accessoryType      :: String,
    relatedAccessory   :: Maybe Accessory
  } deriving (Show, Eq)

instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap f (Parser p) = Parser (fmap f p)

instance Applicative Parser where
  pure :: a -> Parser a
  pure x = Parser (pure x)

  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  (Parser pf) <*> (Parser p) = Parser (pf <*> p)

instance Alternative Parser where
  empty :: Parser a
  empty = Parser $ throwE "Parsing failed"

  (<|>) :: Parser a -> Parser a -> Parser a
  (Parser p1) <|> (Parser p2) = Parser $ ExceptT $ do
    s <- get
    case runState (runExceptT p1) s of
      (Left _, _) -> runState (runExceptT p2) s
      success -> success
                            
instance Monad Parser where
  (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  (Parser p) >>= f = Parser $ p >>= (parse . f)

query :: Parser Query
query = parseAddGuitar <|> parseAddAccessory <|> parseViewInventory <|> parseTestGuitars <|> parseAddAmplifier

-- Parser for AddGuitar
parseAddGuitar :: Parser Query
parseAddGuitar = do
    _ <- parseString "AddGuitar" 
    _ <- parseChar '('
    guitar <- parseGuitar
    return $ AddGuitar guitar

-- Parser for AddAmplifier
parseAddAmplifier :: Parser Query
parseAddAmplifier = do
    _ <- parseString "AddAmplifier"
    _ <- parseChar '('
    amplifier <- parseAmplifier
    return $ AddAmplifier amplifier

-- Parser for AddAccessory
parseAddAccessory :: Parser Query
parseAddAccessory = do
    _ <- parseString "AddAccessory"
    _ <- parseChar '('
    accessory <- parseAccessory
    return $ AddAccessory accessory

parseViewInventory :: Parser Query
parseViewInventory = do
    _ <- parseString "ViewInventory"
    return ViewInventory

parseTestGuitars :: Parser Query
parseTestGuitars = do
    _ <- parseString "TestGuitars"
    return TestGuitars

-- <guitar> ::= "Guitar(" <id> "," <name> "," <price> "," <stock> "," <type> "," <related_guitar> ")"
parseGuitar:: Parser Guitar
parseGuitar =  do
    id <- parseId
    name <- parseName
    stock <- parseStock
    price <- parsePrice
    guitarType <- parseType
    relatedGuitar <- parseMaybeGuitar
    _ <- parseChar ')'
    return $ GuitarData id name price stock guitarType relatedGuitar

-- <amplifier> ::= "Amplifier(" <id> "," <name> "," <price> "," <stock> "," <type> "," <related_amplifier> ")"
parseAmplifier:: Parser Amplifier
parseAmplifier =  do
    id <- parseId
    name <- parseName
    stock <- parseStock
    price <- parsePrice
    amplifierType <- parseType
    relatedAmplifier <- parseMaybeAmplifier
    _ <- parseChar ')'
    return $ AmplifierData id name price stock amplifierType relatedAmplifier

-- <accessory> ::= "Accessory(" <id> "," <name> "," <price> "," <stock> "," <type> "," <related_accessory> ")"
parseAccessory:: Parser Accessory
parseAccessory =  do
    id <- parseId
    name <- parseName
    stock <- parseStock
    price <- parsePrice
    accessoryType <- parseType
    relatedAccessory <- parseMaybeAccesory
    _ <- parseChar ')'
    return $ AccessoryData id name price stock accessoryType relatedAccessory

-- <id> ::= <int>
parseId :: Parser Int
parseId = do
    instrumentId <- parseNumber
    _ <- parseChar ','
    return instrumentId

-- <name> ::= <string>
parseName :: Parser String
parseName = do
    instrumentName <- parseWord
    _ <- parseChar ','
    return instrumentName

-- <stock> ::= <int>
parseStock :: Parser Int
parseStock = do
    instrumentStock <- parseNumber
    _ <- parseChar ','
    return instrumentStock

-- <price> ::= <int>
parsePrice :: Parser Int
parsePrice = do
    instrumentPrice <- parseNumber
    _ <- parseChar ','
    return instrumentPrice

-- <type> ::= <string>
parseType :: Parser String
parseType = do
    instrumentType <- parseWord
    _ <- parseChar ',' 
    return instrumentType

-- <related_guitar> ::= "none" | <guitar>
-- combine parseNoneGuitar and parseRelatedGuitar to get either Nothing or guitar
parseMaybeGuitar :: Parser (Maybe Guitar)
parseMaybeGuitar = parseRelatedGuitar <|> parseNoneGuitar

--parse Related guitar part if it is present
parseRelatedGuitar :: Parser (Maybe Guitar)
parseRelatedGuitar = do 
    _ <- parseString "Guitar"
    _ <- parseChar '('
    relatedGuitar <- parseGuitar
    return (Just relatedGuitar)
    
--parse "none" for guitar
parseNoneGuitar :: Parser (Maybe Guitar)
parseNoneGuitar = do
    _ <- parseString "none"
    return Nothing

-- <related_amplifier> ::= "none" | <amplifier>
-- combine parseNoneAmplifier and parseRelatedAmplifier to get either Nothing or amplifier
parseMaybeAmplifier :: Parser (Maybe Amplifier)
parseMaybeAmplifier = parseNoneAmplifier <|> parseRelatedAmplifier 

--parseRelated Amplifier if it is present
parseRelatedAmplifier :: Parser (Maybe Amplifier)
parseRelatedAmplifier = do 
    _ <- parseString "Amplifier"
    _ <- parseChar '('
    relatedAmplifier <- parseAmplifier
    return (Just relatedAmplifier)

-- parse "none" for amplifier
parseNoneAmplifier :: Parser (Maybe Amplifier)
parseNoneAmplifier = do
    _ <- parseString "none"
    return Nothing

-- <related_accessory> ::= "none" | <accessory>
-- combine parseRelatedAccessory and parseNoneAccessory to get either Nothing or Accessory
parseMaybeAccesory :: Parser (Maybe Accessory)
parseMaybeAccesory = parseRelatedAccessory <|> parseNoneAccessory

-- parse relatedAccessory if it is present
parseRelatedAccessory :: Parser (Maybe Accessory)
parseRelatedAccessory = do 
    _ <- parseString "Accessory"
    _ <- parseChar '('
    relatedAccessory <- parseAccessory
    return (Just relatedAccessory)

-- parse "none" for accessory
parseNoneAccessory :: Parser (Maybe Accessory)
parseNoneAccessory = do
    _ <- parseString "none"
    return Nothing

parseNumber :: Parser Int
parseNumber = Parser $ ExceptT $ state $ \input ->
  let digits = takeWhile isDigit input
      rest = drop (length digits) input
  in if not (null digits)
     then Right (read digits, rest)
     else throwE $ "Expected a number"

parseChar :: Char -> Parser Char
parseChar a = do
    input <- lift get
    case input of
        [] -> throwE "Empty input"
        (x:xs) -> if x == a
            then lift $ put xs >> return x
            else
                throwE $ a:" is not found"

parseWord :: Parser String
parseWord = do
  input <- get
  let word = takeWhile isAlpha input
      rest = dropWhile isAlpha input
  if not (null word)
    then put rest >> return word
    else throwE "Expected a word"

parseString :: String -> Parser String
parseString [] = return []
parseString (c:cs) = do
    _ <- parseChar c
    rest <- parseString cs
    return (c : rest)

isGuitar :: Item -> Bool
isGuitar (GuitarItem _) = True
isGuitar _ = False

showItem :: Item -> String
showItem (GuitarItem guitar)    = "Guitar: " ++ show guitar
showItem (AmplifierItem amplifier)    = "Amplifier: " ++ show amplifier
showItem (AccessoryItem accessory)    = "Accessory: " ++ show accessory

