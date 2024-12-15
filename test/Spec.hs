{-# LANGUAGE ImportQualifiedPost #-}
import Test.Tasty ( TestTree, defaultMain, testGroup )
import Test.Tasty.HUnit ( testCase, (@?=) )

import Lib1 qualified
import Lib2 qualified

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "All Tests" [lib1Tests, lib2Tests]

lib1Tests :: TestTree
lib1Tests = testGroup "Lib1 tests"
  [ testCase "List of completions is not empty" $
      null Lib1.completions @?= False
  ]

lib2Tests :: TestTree
lib2Tests = testGroup "Lib2 tests"
  [ testParseQuery
  , testEmptyState
  , testStateTransition
  ]

testParseQuery :: TestTree
testParseQuery = testGroup "parseQuery tests"
  [ testCase "Parse Add command" $ do
      let input = "Add rose red 50"
      Lib2.parseQuery input @?= Right (Lib2.Add (Lib2.Flower "rose" "red" 50))

  , testCase "Parse Remove command" $ do
      let input = "Remove rose red 50"
      Lib2.parseQuery input @?= Right (Lib2.Remove (Lib2.Flower "rose" "red" 50))

  , testCase "Parse Add_Garden command" $ do
      let input = "Add_Garden rose red 50 tulip yellow 30"
          expected = Lib2.Add_Garden (Lib2.Garden [Lib2.Flower "rose" "red" 50, Lib2.Flower "tulip" "yellow" 30])
      Lib2.parseQuery input @?= Right expected

  , testCase "Parse ShowState command" $ do
      let input = "ShowState"
      Lib2.parseQuery input @?= Right Lib2.ShowState

  , testCase "Parse invalid command" $ do
      let input = "InvalidCommand"
      Lib2.parseQuery input @?= Left "Query Error: unrecognized query command"
  ]


testEmptyState :: TestTree
testEmptyState = testGroup "emptyState tests"
  [ testCase "Empty state has an empty garden" $ do
      let state = Lib2.emptyState
      Lib2.garden state @?= Lib2.Garden []
  ]


testStateTransition :: TestTree
testStateTransition = testGroup "stateTransition tests"
  [ testCase "Add a flower to an empty garden" $ do
      let flower = Lib2.Flower "rose" "red" 50
          state = Lib2.emptyState
      Lib2.stateTransition state (Lib2.Add flower) @?= Right (Nothing, state {Lib2.garden = Lib2.Garden [flower]})

  , testCase "Remove a flower from the garden" $ do
      let flower = Lib2.Flower "rose" "red" 50
          initialState = Lib2.State (Lib2.Garden [flower])
      Lib2.stateTransition initialState (Lib2.Remove flower) @?= Right (Nothing, Lib2.emptyState)

  , testCase "Remove a non-existing flower" $ do
      let flower = Lib2.Flower "rose" "red" 50
          state = Lib2.emptyState
      Lib2.stateTransition state (Lib2.Remove flower) @?= Left "Flower not found in the garden."

  , testCase "Replace the garden with a new garden" $ do
      let oldFlower = Lib2.Flower "rose" "red" 50
          newFlowers = [Lib2.Flower "tulip" "yellow" 30, Lib2.Flower "lily" "white" 40]
          initialState = Lib2.State (Lib2.Garden [oldFlower])
          newGarden = Lib2.Garden newFlowers
      Lib2.stateTransition initialState (Lib2.Add_Garden newGarden) @?= Right (Nothing, Lib2.State newGarden)

  , testCase "ShowState" $ do
      let flowers = [Lib2.Flower "rose" "red" 50, Lib2.Flower "tulip" "yellow" 30]
          state = Lib2.State (Lib2.Garden flowers)
      Lib2.stateTransition state Lib2.ShowState @?= Right (Just (show state), state)
  ]