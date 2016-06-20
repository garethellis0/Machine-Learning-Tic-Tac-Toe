module Tests where

import Test.HUnit
import qualified Main


main :: IO Counts
main = runTestTT $ TestList [testDrawIsDraw]


drawnGame :: Main.GameState
drawnGame = Main.Game [Main.X, Main.O, Main.X,
                  Main.O, Main.X, Main.O,
                  Main.O, Main.X, Main.O]
                  Main.Human

testDrawIsDraw :: Test
testDrawIsDraw = TestCase $ assertEqual "This should be a draw"
                            True
                            (Main.draw drawnGame)
