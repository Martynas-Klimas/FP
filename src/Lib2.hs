{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Lib2
    ( parseQuery,
    Parser,
    parse,
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
    parseSpecificWord,
    parseChar
    ) where

-- Spec.hs is a test file. We need to test parseQuery function (add test cases).

import Data.Char as C
import Data.List as L
import Control.Applicative (Alternative ((<|>)))
import GHC.Base (Alternative(empty))

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

-- | The instances are needed basically for tests
{-
instance Eq Query where
  (==) :: Query -> Query -> Bool
  (==) _ _= False

instance Show Query where
  show _ = ""
-}
-- | Parses user's input.
-- The function must have tests.

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

parseQuery :: Parser Query
parseQuery = Parser $ \input ->
    case parse parseWord input of
        Right ("AddGuitar", rest1) ->
            case parse parseBrackets rest1 of
                Right (_, restGuitar) ->
                    case parse parseGuitar restGuitar of
                        Right (guitar, _) -> Right (AddGuitar guitar, "Adding guitar")
                        Left err -> Left $ "Failed adding guitar: " ++ err
                Left err -> Left err
        Right ("AddAmplifier", rest1) ->
            case parse parseBrackets rest1 of
                Right (_, restAmplifier) ->
                    case parse parseAmplifier restAmplifier of
                        Right (amplifier, _) -> Right (AddAmplifier amplifier, "Adding amplifier")
                        Left err -> Left $ "Failed adding amplifier: " ++ err
                Left err -> Left err
        Right ("AddAccessory", rest1) ->  
            let bracketParse = parseChar '('
            in case parse bracketParse rest1 of
                Right (_, restAccessory) ->
                    case parse parseAccessory restAccessory of
                        Right (accessory, _) -> Right (AddAccessory accessory, "Adding accessory")
                        Left err -> Left $ "Failed adding accessory: " ++ err
                Left err -> Left err
        Right ("ViewInventory", _) -> Right (ViewInventory, "Viewing Guitars")
        Right ("TestGuitars", _) -> Right (TestGuitars, "Testing Guitars")
        Left err -> Left $ "Failed to parse query: " ++ err

-- <guitar> ::= "Guitar(" <id> "," <name> "," <price> "," <stock> "," <type> "," <related_guitar> ")"
parseGuitar:: Parser Guitar
parseGuitar =
    and6' (\id name stock price guitarType relatedGuitar -> GuitarData id name stock price guitarType relatedGuitar) 
     parseId
     parseName
     parseStock
     parsePrice
     parseType
     parseMaybeGuitar
     <* parseChar ')'

-- <amplifier> ::= "Amplifier(" <id> "," <name> "," <price> "," <stock> "," <type> "," <related_amplifier> ")"
parseAmplifier:: Parser Amplifier
parseAmplifier =
    and6' (\id name stock price amplifierType relatedAmplifier -> AmplifierData id name stock price amplifierType relatedAmplifier) 
     parseId
     parseName
     parseStock
     parsePrice
     parseType
     parseMaybeAmplifier
     <* parseChar ')'

-- <accessory> ::= "Accessory(" <id> "," <name> "," <price> "," <stock> "," <type> "," <related_accessory> ")"
parseAccessory:: Parser Accessory
parseAccessory =
    and6' (\id name stock price accesoryType relatedAccessory -> AccessoryData id name stock price accesoryType relatedAccessory) 
     parseId
     parseName
     parseStock
     parsePrice
     parseType
     parseMaybeAccesory
     <* parseChar ')'

-- <id> ::= <int>
parseId :: Parser Int
parseId = and2' (\id _ -> id) parseNumber (parseChar ',')

-- <name> ::= <string>
parseName :: Parser String
parseName = and2' (\name _ -> name) parseWord (parseChar ',')

-- <stock> ::= <int>
parseStock :: Parser Int
parseStock = and2' (\stock _ -> stock) parseNumber (parseChar ',')

-- <price> ::= <int>
parsePrice :: Parser Int
parsePrice = and2' (\price _ -> price) parseNumber (parseChar ',')

-- <type> ::= <string>
parseType :: Parser String
parseType = and2'(\instrumentType _ -> instrumentType) parseWord (parseChar ',')

-- <related_guitar> ::= "none" | <guitar>
-- combine parseNoneGuitar and parseRelatedGuitar to get either Nothing or guitar
parseMaybeGuitar :: Parser (Maybe Guitar)
parseMaybeGuitar  =
    or2 parseNoneGuitar parseRelatedGuitar

--parse Related guitar part if it is present
parseRelatedGuitar :: Parser (Maybe Guitar)
parseRelatedGuitar = Parser $ \input ->
    let parser = and4' (\_ _ guitar _ -> guitar) (parseSpecificWord "Guitar") (parseChar '(') parseGuitar (parseChar ')') 
    in case parse parser input of
        Right (guitar, rest) -> Right (Just guitar, rest)  
        Left err             -> Left err 

--parse "none" for guitar
parseNoneGuitar :: Parser (Maybe Guitar)
parseNoneGuitar = Parser $ \input ->
    if take 4 input == "none" then
        Right (Nothing, drop 4 input) 
    else
        Left "Expected 'none'"

-- <related_amplifier> ::= "none" | <amplifier>
-- combine parseNoneAmplifier and parseRelatedAmplifier to get either Nothing or amplifier
parseMaybeAmplifier :: Parser (Maybe Amplifier)
parseMaybeAmplifier = or2 parseNoneAmplifier parseRelatedAmplifier

--parseRelated Amplifier if it is present
parseRelatedAmplifier :: Parser (Maybe Amplifier)
parseRelatedAmplifier = Parser $ \input -> 
    let parser = and4' (\_ _ amplifier _ -> amplifier) (parseSpecificWord "Amplifier") (parseChar '(') parseAmplifier (parseChar ')')
    in case parse parser input of
        Right (amplifier, rest) -> Right(Just amplifier, rest)
        Left err                -> Left err

-- parse "none" for amplifier
parseNoneAmplifier :: Parser (Maybe Amplifier)
parseNoneAmplifier = Parser $ \input ->
    if take 4 input == "none" then
        Right (Nothing, drop 4 input)  
    else
        Left "Expected 'none'"

-- <related_accessory> ::= "none" | <accessory>
-- combine parseRelatedAccessory and parseNoneAccessory to get either Nothing or Accessory
parseMaybeAccesory :: Parser (Maybe Accessory)
parseMaybeAccesory = or2 parseNoneAccessory parseRelatedAccessory

-- parse relatedAccessory if it is present
parseRelatedAccessory :: Parser (Maybe Accessory)
parseRelatedAccessory = Parser $ \input -> 
    let parser = and4' (\_ _ accessory _ -> accessory) (parseSpecificWord "Accessory") (parseChar '(') parseAccessory (parseChar ')')
    in case parse parser input of
        Right (accessory, rest) -> Right(Just accessory, rest)
        Left err                -> Left err

-- parse "none" for accessory
parseNoneAccessory :: Parser (Maybe Accessory)
parseNoneAccessory = Parser $ \input ->
    if take 4 input == "none" then
        Right (Nothing, drop 4 input) 
    else
        Left "Expected 'none'"

--helper functions to combine parsers
and2' ::(a -> b -> c) -> Parser a -> Parser b -> Parser c 
and2' c a b = Parser $ \input -> 
    case parse a input of
      Right(v1, r1) ->
         case parse b r1 of
            Right(v2, r2) -> Right(c v1 v2, r2)
            Left e2 -> Left e2
      Left e1 -> Left e1

and4' ::(a -> b -> c -> d -> e) -> Parser a -> Parser b -> Parser c -> Parser d -> Parser e
and4' e a b c d = Parser $ \input ->
    case parse a input of
      Right(v1, r1) ->
         case parse b r1 of
            Right(v2, r2) ->
                case parse c r2 of
                    Right(v3, r3) -> 
                        case parse d r3 of 
                            Right(v4, r4) -> Right(e v1 v2 v3 v4, r4)
                            Left e4 -> Left e4
                    Left e3 -> Left e3
            Left e2 -> Left e2
      Left e1 -> Left e1


and6' :: (a -> b -> c -> d -> e -> f -> g) -> Parser a -> Parser b -> Parser c ->
    Parser d -> Parser e -> Parser f -> Parser g
and6' g a b c d e f = Parser $ \input ->
    case parse a input of 
        Right(v1, r1) ->
            case parse b r1 of 
                Right(v2, r2) ->
                    case parse c r2 of 
                        Right(v3, r3) ->
                            case parse d r3 of
                                Right(v4, r4) ->
                                    case parse e r4 of 
                                        Right(v5, r5) -> 
                                            case parse f r5 of
                                                Right(v6, r6) -> Right(g v1 v2 v3 v4 v5 v6, r6)
                                                Left e6 -> Left e6
                                        Left e5 -> Left e5
                                Left e4 -> Left e4
                        Left e3 -> Left e3 
                Left e2 -> Left e2 
        Left e1 -> Left e1
                 
or2 :: Parser a -> Parser a -> Parser a
or2 a b = Parser $ \input -> 
    case parse a input of 
        Right r1 -> Right r1 
        Left e1 ->
            case parse b input of 
                Right r2 -> Right r2
                Left e2 -> Left e2 

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
        [] ->  Left ("Cannot find " ++ [c] ++ " in an empty input")
        s@(h:t) -> if c == h then Right (c, t) else Left (c : " is not found in " ++ s)

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
parseSpecificWord :: String -> Parser String
parseSpecificWord target = Parser $ \str -> 
    let
        word = L.takeWhile C.isAlpha str
        rest = L.dropWhile C.isAlpha str
    in if word == target
        then Right (word, rest)
        else Left $ "Expected the word " ++ target ++ "but found " ++ word

parseBrackets :: Parser Char
parseBrackets = parseChar '('

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