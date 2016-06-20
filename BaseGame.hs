module BaseGame where

import System.Random

type Value = Float
type Move = Int
type Board = [Square]

data Player = AI | Human deriving (Eq, Show, Ord)
data Square = X | O | Empty deriving (Eq, Ord)
instance Show Square where
    show X = "X"
    show O = "O"
    show Empty = "/"
data GameState = Game {board :: Board, activePlayer :: Player} deriving (Eq, Ord, Show)
data GameResult = Win | Lose | Draw deriving (Eq)

initialGameState :: GameState
initialGameState = Game [Empty,Empty,Empty, 
                         Empty,Empty,Empty, 
                         Empty,Empty,Empty]
                         AI

getRandomMove :: StdGen -> IO Move
getRandomMove gen = do 
                let (randNumber, newGen) = randomR (1,9) gen :: (Int, StdGen)
                return randNumber

getHumanMove :: IO Move
getHumanMove = do
    putStrLn $ ("Make a move, petty human!:  ")
    fmap read getLine

getPermittedMoves :: GameState -> [Move]
getPermittedMoves gState = getPermittedMoves' 1 gState
    where getPermittedMoves' :: Int -> GameState -> [Move]
          getPermittedMoves' _ (Game [] _) = []
          getPermittedMoves' count (Game (x:xs) p) = 
              if x == Empty then
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
    where applyMove' :: [Square] -> Move -> GameState -> GameState
          applyMove' front 1 (Game (x:xs) player) = 
              if x == Empty then
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
    where renderGame' :: Int -> GameState -> IO ()
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
