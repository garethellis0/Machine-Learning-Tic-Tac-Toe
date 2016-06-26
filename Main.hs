module Main where

import AI
import BaseGame

import Data.Time.Clock.POSIX as Time
import System.Random

numOfIterations = 1000

main :: IO ()
main = mainLoop 0 initialGameState initialMoveToMake []


mainLoop :: Int -> GameState -> AIDecisions -> [(GameState, Move)] -> IO ()
mainLoop currIteration gState moveToMake aiMovesMade = do
    -- | Render the game
    renderGame gState
    -- | Check if the game is over
    if gameOver gState || draw gState then
            if draw gState then do
                        putStrLn "Draw" 
                        let newMoveToMake = updateAI aiMovesMade moveToMake Draw
                        mainLoop (currIteration+1) initialGameState newMoveToMake []
            else
                -- | Create a new game, updating the AI's strategy depending on if it won or lost
                if activePlayer gState == Human then do
                    putStrLn "AI wins!"
                    let newMoveToMake = updateAI aiMovesMade moveToMake Win
                    mainLoop (currIteration+1) initialGameState newMoveToMake []
                else do
                    putStrLn "Human wins!"
                    let newMoveToMake = updateAI aiMovesMade moveToMake Lose
                    mainLoop (currIteration+1) initialGameState newMoveToMake []
    -- | Get the next move and apply it
    else do
    -- | Generate a random value 
        -- | Get system time as a random seed
        seed <- round `fmap` Time.getPOSIXTime
        gen <- getStdGen
        gen' <- newStdGen
        randomMove <- getRandomMove gen
        putStrLn "Random Move:" ++ show randomMove
        if activePlayer gState == Human then do
            move <- getHumanMove
            mainLoop currIteration (applyMove move gState) moveToMake aiMovesMade
            {-
            if (currIteration >= numOfIterations) then do
                move <- getHumanMove
                mainLoop currIteration (applyMove move gState) moveToMake aiMovesMade
            else do 
                mainLoop currIteration (applyMove randomMove gState) moveToMake aiMovesMade
            -}
        else do
            let move = getAIMove randomMove moveToMake gState
            -- | Get the AI move and apply it, remember the (state,move) for later updating of values
            mainLoop currIteration (applyMove move gState) moveToMake ((gState, move):aiMovesMade)
