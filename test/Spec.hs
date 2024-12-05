{-# LANGUAGE ImportQualifiedPost #-}
{-# OPTIONS_GHC -Wno-orphans #-}
import Test.Tasty ( TestTree, defaultMain, testGroup )
import Test.Tasty.HUnit ( testCase, (@?=) )
import Data.Either (isRight, isLeft)
import Test.Tasty.QuickCheck as QC
import Data.Char (isAlpha)
import Data.List
import Data.Ord

import Lib1 qualified
import Lib2 qualified
import Lib3 qualified
import Parsers

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

instance Arbitrary Lib2.Guitar where
  arbitrary = sized $ \n ->
    Lib2.GuitarData
      <$> positiveInt
      <*> nonEmptyString
      <*> positiveInt
      <*> positiveInt
      <*> nonEmptyString
      <*> (if n == 0 then pure Nothing else arbitrary)

instance Arbitrary Lib2.Amplifier where
  arbitrary = sized $ \n ->
    Lib2.AmplifierData
      <$> positiveInt
      <*> nonEmptyString
      <*> positiveInt
      <*> positiveInt
      <*> nonEmptyString
      <*> (if n == 0 then pure Nothing else arbitrary)

instance Arbitrary Lib2.Accessory where
  arbitrary = sized $ \n ->
    Lib2.AccessoryData
      <$> positiveInt
      <*> nonEmptyString
      <*> positiveInt
      <*> positiveInt
      <*> nonEmptyString
      <*> (if n == 0 then pure Nothing else arbitrary)

nonEmptyString :: Gen String
nonEmptyString = listOf1 $ elements ['A'..'Z']

positiveInt :: Gen Int
positiveInt = arbitrary `suchThat` (> 0)

instance Arbitrary Lib2.Query where
  arbitrary = oneof
    [ Lib2.AddGuitar <$> arbitrary -- Generates a random Guitar and wraps it in AddGuitar
    , Lib2.AddAmplifier <$> arbitrary -- Generates a random Amplifier and wraps it in AddAmplifier
    , Lib2.AddAccessory <$> arbitrary -- Generates a random Accessory and wraps it in AddAccessory
    , pure Lib2.ViewInventory -- Generates the ViewInventory query
    , pure Lib2.TestGuitars -- Generates the TestGuitars query
    ]

instance Arbitrary Lib3.Statements where
  arbitrary = oneof
    [ Lib3.Single <$> arbitrary
    , Lib3.Batch <$> listOf arbitrary
    ]

propertyTests :: TestTree
propertyTests = testGroup "Property Tests"
  [
    QC.testProperty "Rendered and parsed statements" $ 
      \st ->
        let renderedSt = Lib3.renderStatements st
            parsedSt = Lib3.parseStatements renderedSt
            renderedParsedSt = case parsedSt of
              Right (parsedSt, _) -> Lib3.renderStatements parsedSt
              Left _ -> ""
        in counterexample
            ("Rendered statement: \n" ++ renderedSt ++ "\nParsed statement: \n" ++ renderedParsedSt)
            (case parsedSt of 
              Right (parsedSt, "") -> renderedSt == renderedParsedSt
              _ -> False)
  ]