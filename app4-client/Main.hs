module Main (main) where

import Data.ByteString
import Control.Monad.Free (Free (..), liftF)
import Network.Wreq
import Data.String.Conversions
import Control.Lens

--all possible commands
data Command next 
    = AddGuitar Int String Int Int String Maybe Guitar next
    | AddAmplifier Int String Int Int String Maybe Amplifier next
    | AddAccessory Int String Int Int String Maybe Accessory next
    | ViewInventory
    | TestGuitars

type GuitarShopDSL = Free Command

addGuitar :: Int -> String -> Int -> Int -> String -> Maybe Guitar -> GuitarShopDSL ()
addGuitar id name stock price type relatedGuitar = 
    LiftF $ AddGuitar id name stock price type relatedGuitar ()

addAmplifier :: Int -> String -> Int -> Int -> String -> Maybe Amplifier -> GuitarShopDSL ()
addAmplifier id name stock price ampType relatedAmplifier =
    liftF $ AddAmplifier id name stock price ampType relatedAmplifier ()

addAccessory :: Int -> String -> Int -> Int -> String -> Maybe Accessory -> GuitarShopDSL ()
addAccessory id name stock price accessoryType relatedAccessory =
    liftF $ AddAccessory id name stock price accessoryType relatedAccessory ()

viewInventory :: GuitarShopDSL ()
viewInventory = liftF ViewInventory

testGuitars :: GuitarShopDSL ()
testGuitars = liftF TestGuitars

--single http interpreter


main :: IO ()
main = do
    