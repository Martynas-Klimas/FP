{-# LANGUAGE ImportQualifiedPost #-}
import Test.Tasty ( TestTree, defaultMain, testGroup )
import Test.Tasty.HUnit ( testCase, (@?=) )
import Data.Either (isRight, isLeft)
import Test.Tasty.QuickCheck as QC

import Data.List
import Data.Ord

import Lib1 qualified
import Lib2 qualified
import Lib3 qualified

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests, propertyTests]

unitTests :: TestTree
unitTests = testGroup "Lib1 tests"
  [ testCase "List of completions is not empty" $
      null Lib1.completions @?= False,

    testCase "Parsing correct AddGuitar command" $
      isRight (Lib2.parseQuery "AddGuitar(1,Fender,200,2,Electric,none)") @?= True,
    testCase "Parsing incorrect AddAmplifier command (should be none)" $
      isLeft (Lib2.parseQuery "AddAmplifier(2,Marshal,300,1,Tube,n)") @?= True,

   testCase "Parsing a harder variant" $
      isRight (Lib2.parseQuery "AddAmplifier(2,Marshall,10,500,Tube,Amplifier(5,Marshall,10,300,other,none))") @?= True,

   testCase "Parsing an even harder variant" $
      isRight (Lib2.parseQuery "AddAccessory(4,HardPick,50,5,Plastic,Accessory(5,SoftPick,25,2,Plastic,Accessory(6,MediumPick,10,2,other,none)))") @?= True, 

   testCase "Lib3 Simple Test" $
      isRight(Lib3.parseCommand "AddGuitar(1,Name,20,200,Type,none)") @?= True,

   testCase "Lib3 Complex Test" $
      isRight(Lib3.parseCommand "AddAmplifier(2,Marshall,10,500,Tube,Amplifier(5,Marshall,10,300,other,none))") @?= True
   ]

propertyTests :: TestTree
propertyTests = testGroup "Property Tests"
  [


    QC.testProperty "sort == sort . reverse" $
      \list -> sort (list :: [Int]) == sort (reverse list)
  ]