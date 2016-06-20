module AI where

import BaseGame
import Data.Map.Strict as Map
import System.Random

-- | The Structures for saving all previous State-Move-Value
-- | values for Q-Learning
type AIDecisions = Map.Map GameState (Map.Map Move Value)

initialMoveToMake :: AIDecisions
initialMoveToMake = Map.empty


-- | Go through all moves made, adjusting their weights depending on win/lose/draw
-- | Main Q-learning algorithm
updateAI :: [(GameState, Move)] -> AIDecisions -> GameResult -> AIDecisions
updateAI [] moveToMake _ = moveToMake
updateAI ((s,a):rest) moveToMake gameResult = updateAI rest newMoveToMake gameResult
    where newMoveToMake = if (Map.member s moveToMake) && (Map.member a (moveToMake Map.! s)) then
                              -- | Adjust value if already seen
                              (Map.adjust (Map.adjust qVal a) s moveToMake)
                          else 
                              -- | Insert State-Move pair if not seen before
                              (Map.insert s (Map.singleton a reward) moveToMake)
          qVal :: Value -> Value
          qVal value = value + 0.5 * (0.5 * (reward-0.5) - value)
          reward :: Float
          reward | gameResult == Win = 0.5
                 | gameResult == Lose = -0.5
                 | gameResult == Draw = 0.1


-- | Determines which move the AI should make
getAIMove :: Move -> AIDecisions -> GameState -> Move
getAIMove randomMove moveToMake gState= if (Map.member gState moveToMake) && (snd move) > 0 then
                                            fst move  
                                        else 
                                            randomMove
                                        where move = bestMove (moveToMake Map.! gState)

-- | Determines the best move to take from a map of moves and their
-- | corresponding values
bestMove :: Map.Map Move Value -> (Move, Value)
bestMove moves = bestMove' (tail movesList) (movesList !! 0)  
    where 
    movesList = Map.toList moves
    bestMove' [] bestSoFar = bestSoFar
    bestMove' (currentMove:moves) bestSoFar = if (snd currentMove) > (snd bestSoFar) then
                                                bestMove' moves currentMove
                                              else
                                                bestMove' moves bestSoFar
