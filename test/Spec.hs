{-# LANGUAGE ImportQualifiedPost #-}
import Test.Tasty ( TestTree, defaultMain, testGroup )
import Test.Tasty.HUnit ( testCase, (@?=) )

import Lib1 qualified
import Lib2 qualified

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests = testGroup "Lib1 tests"
  [ testCase "List of completions is not empty" $
      null Lib1.completions @?= False,

   testCase "Parse correct id" $
      Lib2.parseId "123,abcd" @?= Right(123, "abcd"),
   testCase "Parse incorrect id" $
      Lib2.parseId "abcd123" @?= Left("not a number"),

   testCase "Parse correct name" $
      Lib2.parseName "Name,etc" @?= Right("Name", "etc"),
   testCase "Parse incorrect name" $
      Lib2.parseName "123,Name" @?= Left("Expected a word"),

    testCase "Parsing AddGuitar command" $
      isRight(Lib2.parseQuery "AddGuitar(1,Fender,200,2,Electric, none)") @?= True,
    testCase "Parsing incorrect AddAmplifier command (should be none)" $
      isLeft(Lib2.parseQuery "AddAmplifier(2,Marshal,300,1,Tube,n)") @?= False 
  ]