{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Lib2
    ( parseQuery,
    State(..),
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
    parseMaybeAccesory
    ) where

-- Spec.hs is a test file. We need to test parseQuery function (add test cases).

import Data.Char as C
import Data.List as L

type Parser a = String -> Either String (a, String)
-- | An entity which represets user input.
-- It should match the grammar from Laboratory work #1.
-- Currently it has no constructors but you can introduce
-- as many as needed.
data Query = 
    AddGuitar Guitar |
    AddAmplifier Amplifier |
    AddAccessory Accessory |
    ViewInventory

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
instance Eq Query where
  (==) :: Query -> Query -> Bool
  (==) _ _= False

instance Show Query where
  show _ = ""

-- | Parses user's input.
-- The function must have tests.
parseQuery :: String -> Either String Query
parseQuery input =
    case parseWord input of
        Right ("AddGuitar", rest1) ->
            case parseChar '(' rest1 of
                Right (_, restGuitar) ->
                    case parseGuitar restGuitar of
                        Right (guitar, rest) -> Right (AddGuitar guitar)
                        Left err -> Left $ "Failed adding guitar: " ++ err
                Left err -> Left err
        Right ("AddAmplifier", rest1) ->
            case parseChar '(' rest1 of
                Right (_, restAmplifier) ->
                    case parseAmplifier restAmplifier of
                        Right (amplifier, rest) -> Right (AddAmplifier amplifier)
                        Left err -> Left $ "Failed adding amplifier: " ++ err
                Left err -> Left err
        Right ("AddAccessory", rest1) ->  
            case parseChar '(' rest1 of
                Right (_, restAccessory) ->
                    case parseAccessory restAccessory of
                        Right (accessory, rest) -> Right (AddAccessory accessory)
                        Left err -> Left $ "Failed adding accessory: " ++ err
                Left err -> Left err
        Right ("ViewInventory", _) -> Right ViewInventory 
        Left err -> Left $ "Failed to parse query: " ++ err

-- <guitar> ::= "Guitar(" <id> "," <name> "," <price> "," <stock> "," <type> "," <related_guitar> ")"
parseGuitar:: Parser Guitar
parseGuitar =
    and6' (\id name price stock guitarType relatedGuitar -> GuitarData id name price stock guitarType relatedGuitar) 
     parseId
     parseName
     parsePrice
     parseStock
     parseType
     parseMaybeGuitar
     <* parseChar ')'

-- <amplifier> ::= "Amplifier(" <id> "," <name> "," <price> "," <stock> "," <type> "," <related_amplifier> ")"
parseAmplifier:: Parser Amplifier
parseAmplifier =
    and6' (\id name price stock amplifierType relatedAmplifier -> AmplifierData id name price stock amplifierType relatedAmplifier) 
     parseId
     parseName
     parsePrice
     parseStock
     parseType
     parseMaybeAmplifier
     <* parseChar ')'

-- <accessory> ::= "Accessory(" <id> "," <name> "," <price> "," <stock> "," <type> "," <related_accessory> ")"
parseAccessory:: Parser Accessory
parseAccessory =
    and6' (\id name price stock accesoryType relatedAccessory -> AccessoryData id name price stock accesoryType relatedAccessory) 
     parseId
     parseName
     parsePrice
     parseStock
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
parseRelatedGuitar input =
    case and4' (\_ _ guitar _ -> guitar) (parseSpecificWord "Guitar") (parseChar '(') parseGuitar (parseChar ')') input of
        Right (guitar, rest) -> Right (Just guitar, rest)  
        Left err             -> Left err 

--parse "none" for guitar
parseNoneGuitar :: Parser (Maybe Guitar)
parseNoneGuitar input =
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
parseRelatedAmplifier input = 
    case and4' (\_ _ amplifier _ -> amplifier) (parseSpecificWord "Amplifier") (parseChar '(') parseAmplifier (parseChar ')') input of
        Right (amplifier, rest) -> Right(Just amplifier, rest)
        Left err                -> Left err

-- parse "none" for amplifier
parseNoneAmplifier :: Parser (Maybe Amplifier)
parseNoneAmplifier input =
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
parseRelatedAccessory input = 
    case and4' (\_ _ accessory _ -> accessory) (parseSpecificWord "Accessory") (parseChar '(') parseAccessory (parseChar ')') input of
        Right (accessory, rest) -> Right(Just accessory, rest)
        Left err                -> Left err

-- parse "none" for accessory
parseNoneAccessory :: Parser (Maybe Accessory)
parseNoneAccessory input =
    if take 4 input == "none" then
        Right (Nothing, drop 4 input) 
    else
        Left "Expected 'none'"

--helper functions to combine parsers
and2' ::(a -> b -> c) -> Parser a -> Parser b -> Parser c 
and2' c a b input = 
    case a input of
      Right(v1, r1) ->
         case b r1 of
            Right(v2, r2) -> Right(c v1 v2, r2)
            Left e2 -> Left e2
      Left e1 -> Left e1

and4' ::(a -> b -> c -> d -> e) -> Parser a -> Parser b -> Parser c -> Parser d -> Parser e
and4' e a b c d input = 
    case a input of
      Right(v1, r1) ->
         case b r1 of
            Right(v2, r2) ->
                case c r2 of
                    Right(v3, r3) -> 
                        case d r3 of 
                            Right(v4, r4) -> Right(e v1 v2 v3 v4, r4)
                            Left e4 -> Left e4
                    Left e3 -> Left e3
            Left e2 -> Left e2
      Left e1 -> Left e1


and6' :: (a -> b -> c -> d -> e -> f -> g) -> Parser a -> Parser b -> Parser c ->
    Parser d -> Parser e -> Parser f -> Parser g
and6' g a b c d e f input = 
    case a input of 
        Right(v1, r1) ->
            case b r1 of 
                Right(v2, r2) ->
                    case c r2 of 
                        Right(v3, r3) ->
                            case d r3 of
                                Right(v4, r4) ->
                                    case e r4 of 
                                        Right(v5, r5) -> 
                                            case f r5 of
                                                Right(v6, r6) -> Right(g v1 v2 v3 v4 v5 v6, r6)
                                                Left e6 -> Left e6
                                        Left e5 -> Left e5
                                Left e4 -> Left e4
                        Left e3 -> Left e3 
                Left e2 -> Left e2 
        Left e1 -> Left e1
                 
or2 :: Parser a -> Parser a -> Parser a
or2 a b input = 
    case a input of 
        Right r1 -> Right r1 
        Left e1 ->
            case b input of 
                Right r2 -> Right r2
                Left e2 -> Left e2 

parseNumber :: Parser Int
parseNumber [] = Left "empty input, cannot parse a number"
parseNumber str =
    let
        digits = L.takeWhile C.isDigit str
        rest = drop (length digits) str
    in
        case digits of
            [] -> Left "not a number"
            _ -> Right (read digits, rest)

parseChar :: Char -> Parser Char
parseChar _ [] = Left "empty input, cannot parse a char"
parseChar c input = 
    case input of
        (x:xs) -> if x == c
            then Right(c, xs)
            else Left("Expected " ++ [c] ++ ", but got " ++ [x])

-- parse a word and return (word, rest)
parseWord :: Parser String
parseWord str =
    let
        word = L.takeWhile C.isAlpha str
        rest = L.dropWhile C.isAlpha str
    in if not (null word) 
        then Right(word, rest)
        else Left "Expected a word"

-- parse a word only if a specific word is found
parseSpecificWord :: String -> Parser String
parseSpecificWord target str =
    let
        word = L.takeWhile C.isAlpha str
        rest = L.dropWhile C.isAlpha str
    in if word == target
        then Right (word, rest)
        else Left $ "Expected the word " ++ target ++ "but found " ++ word

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
            in Right (Just "Amplifier added successfully", newState)
        AddAccessory accessory ->
            let 
                accessoryItem = AccessoryItem accessory
                newState = st {inventory = accessoryItem : inventory st}
            in Right (Just "Accessory added successfully", newState)
        ViewInventory ->
            let inventoryContent = if null (inventory st)
                                    then "No items in inventory"
                                    else unlines(map showItem (inventory st))
            in Right(Just inventoryContent, st)
            
showItem :: Item -> String
showItem (GuitarItem guitar)    = "Guitar: " ++ show guitar
showItem (AmplifierItem amplifier)    = "Amplifier: " ++ show amplifier
showItem (AccessoryItem accessory)    = "Accessory: " ++ show accessory
