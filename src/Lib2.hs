{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Lib2
    ( query,
    Parser,
    parse,
    parseQuery,
    State(..),
    Query(..),
    Guitar(..),
    Amplifier(..),
    Accessory(..),
    Item(..),
    emptyState,
    stateTransition,
    parseId,
    parseName,
    parseStock,
    parsePrice,
    parseType,
    parseNoneGuitar,
    parseNoneAmplifier,
    parseNoneAccessory,
    parseRelatedGuitar,
    parseMaybeGuitar,
    parseRelatedAmplifier,
    parseMaybeAmplifier,
    parseRelatedAccessory,
    parseMaybeAccesory,
    parseString,
    parseChar
    ) where

-- Spec.hs is a test file. We need to test parseQuery function (add test cases).

import Data.Char as C
import Data.List as L
import Control.Applicative (Alternative ((<|>)))
import GHC.Base (Alternative(empty))
import Debug.Trace (trace)

newtype Parser a = Parser { parse :: String -> Either String (a, String) }
-- | An entity which represets user input.
-- It should match the grammar from Laboratory work #1.
-- Currently it has no constructors but you can introduce
-- as many as needed.
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
  fmap f (Parser p) = Parser $ \input -> 
    case p input of
      Left err -> Left err
      Right (result, rest) -> Right (f result, rest)

instance Applicative Parser where
  pure x = Parser $ \input -> Right (x, input)
  (Parser pf) <*> (Parser p) = Parser $ \input -> 
    case pf input of
      Left err -> Left err
      Right (f, rest) -> 
        case p rest of
          Left err' -> Left err'
          Right (result, rest') -> Right (f result, rest')

instance Alternative Parser where
    empty :: Parser a
    empty = Parser $ \input -> Left $ "Could not parse " ++ input
    (<|>) :: Parser a -> Parser a -> Parser a
    p1 <|> p2 = Parser $ \inp ->
        case parse p1 inp of
            Right r1 -> Right r1
            Left e1 -> case parse p2 inp of
                            Right r2 -> Right r2
                            Left e2 -> Left e2
                            
instance Monad Parser where
  (Parser p) >>= f = Parser $ \input -> 
    case p input of
      Left err -> Left err
      Right (result, rest) -> parse (f result) rest

query :: Parser Query
query = parseAddGuitar <|> parseAddAccessory <|> parseViewInventory <|> parseTestGuitars <|> parseAddAmplifier

parseQuery :: String -> Either String Query
parseQuery str =
  case parse query str of
    Right (q, r) -> if null r then Right q else Left ("Unrecognized characters:" ++ r)
    Left err -> Left err

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
parseNumber = Parser $ \input ->
    let
        digits = L.takeWhile C.isDigit input
        rest = drop (length digits) input
    in
        case digits of
            [] -> Left "not a number"
            _ -> Right (read digits, rest)

parseChar :: Char -> Parser Char
parseChar c = Parser $ \input ->
    case input of
        [] -> Left ("Expected '" ++ [c] ++ "', but input is empty")
        (h:t) -> if c == h then Right (c, t) else Left ("Expected '" ++ [c] ++ "', but found '" ++ [h] ++ "'")

-- parse a word and return (word, rest)
parseWord :: Parser String
parseWord = Parser $ \str ->
    let
        word = L.takeWhile C.isAlpha str
        rest = L.dropWhile C.isAlpha str
    in
        if not (null word)
        then Right (word, rest)
        else Left "Expected a word"

-- parse a word only if a specific word is found
parseString :: String -> Parser String
parseString [] = return []
parseString (c:cs) = do
    _ <- parseChar c
    rest <- parseString cs
    return (c : rest)

-- | An entity which represents your program's state.
-- Currently it has no constructors but you can introduce
-- as many as needed.
data State = State {
    inventory :: [Item]
} deriving(Show)

-- | Creates an initial program's state.
-- It is called once when the program starts.
emptyState :: State
emptyState = State {inventory = []}

-- | Updates a state according to a query.
-- This allows your program to share the state
-- between repl iterations.
-- Right contains an optional message to print and
-- an updated program's state.
stateTransition :: State -> Query -> Either String (Maybe String, State)
stateTransition st query = 
    case query of 
        AddGuitar guitar ->
            let 
                guitarItem = GuitarItem guitar 
                newState = st {inventory = guitarItem : inventory st}
            in Right (Just "Guitar added successfully", newState)    
        AddAmplifier amplifier ->
            let 
                amplifierItem = AmplifierItem amplifier
                newState = st {inventory = amplifierItem : inventory st}
            in Right (Just  "Amplifier added successfully", newState)
        AddAccessory accessory ->
            let 
                accessoryItem = AccessoryItem accessory
                newState = st {inventory = accessoryItem : inventory st}
            in Right (Just "Accessory added successfully", newState)
        ViewInventory ->
            let inventoryContent = if null (inventory st)
                                    then "No items in inventory"
                                    else unlines(map showItem (inventory st))
            in Right(Just $ "Inventory " ++ inventoryContent, st)
        TestGuitars ->
            let guitars = filter isGuitar (inventory st)
                message = map playGuitar guitars 
                response = if null message
                             then "No guitars in inventory"
                             else unlines message
            in Right(Just response, st)

isGuitar :: Item -> Bool
isGuitar (GuitarItem _) = True
isGuitar _ = False

playGuitar :: Item -> String
playGuitar (GuitarItem guitar) = 
    case guitarType guitar of
        "Electric"  -> "Playing rock music on a electric guitar: " ++ guitarName guitar
        "Acoustic"  -> "Strumming chords on a acoustic guitar: " ++ guitarName guitar
        "Classical" -> "Playing a classical melody on a classical guitar: " ++ guitarName guitar
        _           -> "Testing a " ++ guitarType guitar ++ " " ++ guitarName guitar ++ " guitar"
playGuitar _ = ""  -- for non guitar items we dont do anything

showItem :: Item -> String
showItem (GuitarItem guitar)    = "Guitar: " ++ show guitar
showItem (AmplifierItem amplifier)    = "Amplifier: " ++ show amplifier
showItem (AccessoryItem accessory)    = "Accessory: " ++ show accessory