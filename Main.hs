module Main where

import qualified Data.Map.Strict as Map
import Data.Time.Clock.POSIX as Time
import System.Random

data Player = AI | Human deriving (Eq, Show, Ord)
data Square = X | O | Empty deriving (Eq, Ord)
instance Show Square where
    show X = "X"
    show O = "O"
    show Empty = "/"
type Move = Int
type Board = [Square]
data GameState = Game {board :: Board, activePlayer :: Player} deriving (Eq, Ord, Show)
type Value = Float
type AIDecisions = Map.Map GameState (Map.Map Move Value)

initialGameState :: GameState
initialGameState = Game [Empty,Empty,Empty, 
                         Empty,Empty,Empty, 
                         Empty,Empty,Empty]
                         AI
initialMoveToMake :: AIDecisions
initialMoveToMake = Map.empty
numOfIterations = 1000

main :: IO ()
main = do
    mainLoop 0 initialGameState initialMoveToMake []

mainLoop :: Int -> GameState -> AIDecisions -> [(GameState, Move)] -> IO ()
mainLoop currIteration gState moveToMake aiMovesMade = do
    -- | Render the game
    renderGame gState
    -- | Check if the game is over
    if gameOver gState || draw gState then
            if draw gState then do
                        putStrLn "Draw" 
                        let newMoveToMake = (updateAI aiMovesMade moveToMake 0.7)
                        mainLoop (currIteration+1) initialGameState newMoveToMake []
            else
                -- | Create a new game, updating the AI's strategy depending on if it won or lost
                if (activePlayer gState == Human) then do
                    putStrLn "AI wins!"
                    let newMoveToMake = (updateAI aiMovesMade moveToMake 1)
                    mainLoop (currIteration+1) initialGameState newMoveToMake []
                else do
                    putStrLn "Human wins!"
                    let newMoveToMake = (updateAI aiMovesMade moveToMake 0)
                    mainLoop (currIteration+1) initialGameState newMoveToMake []
    -- | Get the next move and apply it
    else do
    -- | Generate a random value 
        -- | Get system time as a random seed
        seed <- round `fmap` Time.getPOSIXTime
        gen <- getStdGen
        gen' <- newStdGen
        randomMove <- getRandomMove gen
        putStrLn ("Random Move:" ++ (show randomMove))
        if (activePlayer gState) == Human then do
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
            let move = (getAIMove randomMove moveToMake gState)
            -- | Get the AI move and apply it, remember the (state,move) for later updating of values
            mainLoop currIteration (applyMove move gState) moveToMake ((gState, move):aiMovesMade)


getRandomMove :: StdGen -> IO Move
getRandomMove gen = do 
                let (randNumber, newGen) = randomR (1,9) gen :: (Int, StdGen)
                return randNumber

-- | Go through all moves made, adjusting their weights
-- | Main Q-learning algorithm
updateAI :: [(GameState, Move)] -> AIDecisions -> Float -> AIDecisions
updateAI [] moveToMake _ = moveToMake
updateAI ((s,a):rest) moveToMake reward = updateAI rest newMoveToMake reward
    where newMoveToMake = if (Map.member s moveToMake) && (Map.member a (moveToMake Map.! s)) then
                              (Map.adjust (Map.adjust qVal a) s moveToMake)
                          else 
                            (Map.insert s (Map.singleton a (reward-0.5)) moveToMake)
          qVal :: Value -> Value
          qVal value = value + 0.5 * (0.5 * (reward-0.5) - value)

getHumanMove :: IO Move
getHumanMove = do
    putStrLn $ ("Make a move, petty human!:  ")
    fmap read getLine

getAIMove :: Move -> AIDecisions -> GameState -> Move
getAIMove randomMove moveToMake gState= if (Map.member gState moveToMake) && 
                                            (bestMove (moveToMake Map.! gState)) > 0 then
                                                bestMove (moveToMake Map.! gState)   
                                              else 
                                                  randomMove
                                                  -- permittedMoves !! 0
bestMove :: Map.Map Move Value -> Move
bestMove moves = bestMove' (tail movesList) (movesList !! 0)  
    where 
    movesList = Map.toList moves
    bestMove' [] bestSoFar = if (snd bestSoFar) < 0 then
                                -1
                             else
                                (fst bestSoFar)
    bestMove' (currentMove:moves) bestSoFar = if (snd currentMove) > (snd bestSoFar) then
                                                bestMove' moves currentMove
                                              else
                                                bestMove' moves bestSoFar

getPermittedMoves :: GameState -> [Move]
getPermittedMoves gState = getPermittedMoves' 1 gState

getPermittedMoves' :: Int -> GameState -> [Move]
getPermittedMoves' _ (Game [] _) = []
getPermittedMoves' count (Game (x:xs) p) = if x == Empty then
                                             count:(getPermittedMoves' (count+1) (Game xs p))
                                           else
                                             getPermittedMoves' (count+1) (Game xs p) 
draw :: GameState -> Bool
draw (Game board player) = allFilled && noWinner
    where allFilled = foldl (\acc x -> if (x == Empty) then False else acc) True board 
          noWinner = gameOver (Game board player) == False

gameOver :: GameState -> Bool
gameOver (Game board _) = any full $ diagonals board ++ verticals board ++ horizontals board
    where full [a,b,c] = a /= Empty && a == b && b == c
          diagonals [a, _ , b,
                     _, c , _,
                     d, _ , e] = [[a,c,e], [d,c,b]]
          horizontals [a, b, c,
                       d, e, f,
                       g, h, i] = [[a,d,g], [b,e,h], [c,f,i]]
          verticals [a, b, c,
                     d, e, f,
                     g, h, i] = [[a,b,c], [d,e,f], [g,h,i]]

applyMove :: Move -> GameState -> GameState
applyMove move gState = applyMove' [] move gState

applyMove' :: [Square] -> Move -> GameState -> GameState
applyMove' front 1 (Game (x:xs) player) = if x == Empty then
                                          -- | Actually Apply Moves
                                              if player == Human then
                                                  Game (front ++ (X:xs)) AI
                                              else 
                                                  Game (front ++ (O:xs)) Human
                                          -- | Square already taken, do not apply move
                                          else
                                            Game (front ++ (x:xs)) player
applyMove' front move (Game (x:xs) player) = 
    applyMove' (front++[x]) (move-1) (Game xs player)

renderGame :: GameState -> IO ()
renderGame gState = renderGame' 1 gState

renderGame' :: Int -> GameState -> IO ()
renderGame' _ (Game [] _) = putStrLn "+---+---+---+"
renderGame' counter (Game (a:b:c:xs) player) = do
    putStrLn "+---+---+---+"
    renderLine counter [a,b,c]
    renderGame' (counter + 3) (Game xs player)

renderLine :: Int -> [Square] -> IO ()
renderLine _ [] = putStr "|\n"
renderLine counter (x:xs) = do
                            if x == Empty then 
                                putStr ("| " ++ (show counter) ++ " ")
                            else 
                                putStr ("|"++val++val++val)
                            renderLine (counter+1) xs
                            where val = (show x)
