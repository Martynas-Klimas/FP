{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant return" #-}
{-# LANGUAGE BlockArguments #-}
module Lib3
    ( stateTransition,
    StorageOp (..),
    storageOpLoop,
    parseCommand,
    parseStatements,
    marshallState,
    renderStatements,
    Statements(..),
    ) where

import Control.Concurrent ( Chan , readChan, writeChan)
import Control.Concurrent.STM( STM, TVar, atomically, writeTVar, readTVar )
import qualified Lib2
import Lib2(State, emptyState, inventory)
import Parsers
import Control.Applicative ((<|>), Alternative (many))
import Control.Monad(forever)
import System.Directory (doesFileExist)
import Control.Concurrent.STM.TVar (readTVarIO)
import Control.Concurrent.Chan (newChan)
import Debug.Trace (trace)

data StorageOp = Save String (Chan ()) | Load (Chan(Maybe String))
-- | This function is started from main
-- in a dedicated thread. It must be used to control
-- file access in a synchronized manner: read requests
-- from chan, do the IO operations needed and respond
-- to a channel provided in a request.
-- Modify as needed.
storageOpLoop :: Chan StorageOp -> IO ()
storageOpLoop storageOpChannel = forever $ do
    storageOp <- readChan storageOpChannel
    case storageOp of
        Save string channel -> do
            writeFile fileName string
            writeChan channel () -- saving is complete
        Load channel -> do
            exists <- doesFileExist fileName
            if exists
                then do
                    loadStr <- readFile fileName
                    writeChan channel (Just loadStr) --return result from load
            else writeChan channel Nothing

fileName :: String
fileName = "state.txt"

data Statements = Batch [Query] |
               Single Query
               deriving (Show, Eq)

data Command = StatementCommand Statements |
               LoadCommand |
               SaveCommand
               deriving (Show, Eq)

-- | Parses user's input.
parseCommand :: String -> Either String (Command, String)
parseCommand = parse (StatementCommand <$> statements <|> parseLoad <|> parseSave)

-- | Parses the "load" command.
parseLoad :: Parser Command
parseLoad = do
    _ <- parseString "load"
    return LoadCommand

--Parses the "save" command
parseSave :: Parser Command
parseSave = do
    _ <- parseString "save"
    return SaveCommand

-- | Parses Statement.
-- Must be used in parseCommand.
-- Reuse Lib2 as much as you can.
-- You can change parseQuery signature if needed.
parseStatements :: String -> Either String (Statements, String)
parseStatements = parse statements


-- | Converts program's state into Statements
-- (probably a batch, but might be a single query)
marshallState :: State -> Statements
marshallState state =
    let
        instrumentQueries = map itemToQuery (inventory state)
    in
        -- If there are multiple queries, return a Batch
        if length instrumentQueries > 1
        then Batch instrumentQueries
        -- If there's only one query, return a Single
        else Single (head instrumentQueries)

-- Convert each Item to its corresponding Query
itemToQuery :: Item -> Query
itemToQuery (GuitarItem guitar) = AddGuitar guitar
itemToQuery (AmplifierItem amplifier) = AddAmplifier amplifier
itemToQuery (AccessoryItem accessory) = AddAccessory accessory

-- | Renders Statements into a String which
-- can be parsed back into Statements by parseStatements
-- function. The String returned by this function must be used
-- as persist program's state in a file. 
-- Must have a property test
-- for all s: parseStatements (renderStatements s) == Right(s, "")
renderStatements :: Statements -> String
renderStatements (Single query) = renderQuery query
renderStatements (Batch queries) = "BEGIN\n" ++ concatMap ((++ ";\n") . renderQuery) queries ++ "END\n"

renderQuery :: Query -> String
renderQuery (AddGuitar guitar) =
    "AddGuitar(" ++ renderGuitar guitar ++ ")"
renderQuery (AddAmplifier amplifier) =
    "AddAmplifier(" ++ renderAmplifier amplifier ++ ")"
renderQuery (AddAccessory accessory) =
    "AddAccessory(" ++ renderAccessory accessory ++ ")"
renderQuery ViewInventory =
    "ViewInventory"
renderQuery TestGuitars =
    "TestGuitars"

renderGuitar :: Guitar -> String
renderGuitar guitar =
    show(guitarId guitar) ++ "," ++
    guitarName guitar ++ "," ++
    show (guitarPrice guitar) ++ "," ++
    show (guitarStock guitar) ++ "," ++
    guitarType guitar ++ "," ++
    renderRelatedGuitar (relatedGuitar guitar)

-- Render the related guitar if it exists
renderRelatedGuitar :: Maybe Guitar -> String
renderRelatedGuitar Nothing = "none"
renderRelatedGuitar (Just relatedGuitar) =
    "Guitar(" ++ renderGuitar relatedGuitar ++ ")"

renderAmplifier :: Amplifier -> String
renderAmplifier amplifier =
    show(amplifierId amplifier) ++ "," ++
    amplifierName amplifier ++ "," ++
    show (amplifierPrice amplifier) ++ "," ++
    show (amplifierStock amplifier) ++ "," ++
    amplifierType amplifier ++ "," ++
    renderRelatedAmplifier (relatedAmplifier amplifier)

renderRelatedAmplifier :: Maybe Amplifier -> String
renderRelatedAmplifier Nothing = "none"
renderRelatedAmplifier (Just relatedAmplifier) =
    "Amplifier(" ++ renderAmplifier relatedAmplifier ++ ")"

renderAccessory :: Accessory -> String
renderAccessory accessory =
    show(accessoryId accessory) ++ "," ++
    accessoryName accessory ++ "," ++
    show (accessoryPrice accessory) ++ "," ++
    show (accessoryStock accessory) ++ "," ++
    accessoryType accessory ++ "," ++
    renderRelatedAccessory (relatedAccessory accessory)

renderRelatedAccessory :: Maybe Accessory -> String
renderRelatedAccessory Nothing = "none"
renderRelatedAccessory (Just relatedAccessory) =
    "Accessory(" ++ renderAccessory relatedAccessory ++ ")"

-- | Updates a state according to a command.
-- Performs file IO via ioChan if needed.
-- This allows your program to share the state
-- between repl iterations, save the state to a file,
-- load the state from the file so the state is preserved
-- between program restarts.
-- Keep IO as small as possible.
-- State update must be executed atomically (STM).
-- Right contains an optional message to print, updated state
-- is stored in transactinal variable
stateTransition :: TVar State -> Command -> Chan StorageOp ->
                   IO (Either String (Maybe String))
stateTransition stateVar command ioChan =
    case command of
        --save the state
        SaveCommand -> do
            currentState <- readTVarIO stateVar
            responseChan <- newChan
            writeChan ioChan (Save (renderStatements $ marshallState currentState) responseChan)
            readChan responseChan
            return $ Right (Just "State saved successfully")

        --load state from file
        LoadCommand -> do
            atomically $ writeTVar stateVar emptyState
            responseChan <- newChan
            writeChan ioChan (Load responseChan)
            maybeContent <- readChan responseChan
            case maybeContent of
                Nothing -> return $ Left "No data found in file"
                Just content -> case parseStatements content of
                    Right (parsedStatements, _) -> stateTransition stateVar (StatementCommand parsedStatements) ioChan
                    Left err -> return $ Left ("Failed to load: " ++ err)
        -- statements
        StatementCommand st -> do
            atomically $ atomicStatements stateVar st

transitionThroughList :: State -> [Query] -> Either String (Maybe String, State)
transitionThroughList _ [] = Left "Empty query list"
transitionThroughList state (query:remaining) =
  case Lib2.stateTransition state query of
    Right (message, newState) ->
      if null remaining
        then Right (message, newState)
        else case transitionThroughList newState remaining of
                Right (nextMessage, finalState) -> Right (combine message nextMessage, finalState)
                Left err -> Left err
    Left err -> Left err

combine :: Maybe String -> Maybe String -> Maybe String
combine Nothing Nothing = Nothing
combine (Just msg1) Nothing = Just msg1
combine Nothing (Just msg2) = Just msg2
combine (Just msg1) (Just msg2) = Just (msg1 ++ "\n" ++ msg2)

atomicStatements :: TVar Lib2.State -> Statements -> STM (Either String (Maybe String))
atomicStatements stateVar statement = do
  currentState <- readTVar stateVar
  case statement of
    Batch queries ->
      case transitionThroughList currentState queries of
        Right (message, newState) -> writeTVar stateVar newState >> return (Right message)
        Left err -> return $ Left err
    Single query ->
      case  Lib2.stateTransition currentState query of
        Right (message, newState) -> writeTVar stateVar newState >> return (Right message)
        Left err -> return $ Left err


statements :: Parser Statements
statements = ( do
    _ <- parseString "BEGIN\n"
    statement <- many (do
                    statement <- query
                    _ <- parseString ";\n"
                    return statement)
    _ <- parseString "END\n"
    return $ Batch statement
    )
        <|> (Single <$> query)