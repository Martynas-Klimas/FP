{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Lib2
    (
    parseQuery,
    emptyState,
    stateTransition,
    State,
    inventory
    ) where

import Parsers

parseQuery :: String -> Either String Query
parseQuery str =
  case parse query str of
    Right (q, r) -> if null r then Right q else Left ("Unrecognized characters:" ++ r)
    Left err -> Left err

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

playGuitar :: Item -> String
playGuitar (GuitarItem guitar) = 
    case guitarType guitar of
        "Electric"  -> "Playing rock music on a electric guitar: " ++ guitarName guitar
        "Acoustic"  -> "Strumming chords on a acoustic guitar: " ++ guitarName guitar
        "Classical" -> "Playing a classical melody on a classical guitar: " ++ guitarName guitar
        _           -> "Testing a " ++ guitarType guitar ++ " " ++ guitarName guitar ++ " guitar"
playGuitar _ = ""  -- for non guitar items we dont do anything

