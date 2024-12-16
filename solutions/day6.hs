module Main where

import Control.Monad.State
import Data.Either (rights)
import Data.List (intercalate, (!?))
import Data.Void
import Data.Set (fromList, size)
import Text.Megaparsec (Parsec, anySingle, many, parse)
import Debug.Trace

type Parser = Parsec Void String

data MapElement = Free | Obstacle

-- |     x ->
-- | y  (0, 0) (1, 0) (2, 0) ...
-- | ↓  (0, 1) (1, 1) ...
-- |    (0, 2) ...
-- |    ...
newtype Map = Map [[MapElement]]
type Position = (Int, Int)
type Path = [Position]
data Direction = UpDir | DownDir | LeftDir | RightDir deriving (Show, Eq)
data GuardState = GuardState Path Direction Position deriving (Show)

instance Show MapElement where
  show Free = " "
  show Obstacle = "X"

instance Show Map where
  show (Map m) = intercalate "\n" (map show m)

parser :: Parser [MapElement]
parser = many f
  where
    f = do
      x <- anySingle
      case x of
        '.' -> return Free
        _ -> return Obstacle



turnDirection :: Int -> Direction -> Direction
turnDirection n dir =
  let directions = [UpDir, RightDir, DownDir, LeftDir]
   in if n > 0 
    then head . drop n . dropWhile (/= dir) $ cycle directions
    else head . drop (length directions - n) . dropWhile (/= dir) $ cycle directions

mapAt :: Map -> Position -> Maybe MapElement
mapAt (Map m) (x, y) = (m !? y) >>= (!? x)

stepIncrement :: Direction -> Position
stepIncrement dir =
  case dir of
    UpDir -> (0, -1)
    DownDir -> (0, 1)
    LeftDir -> (-1, 0)
    RightDir -> (1, 0)

walk :: Map -> Direction -> Position -> Maybe (Position, Direction)
walk (Map m) dir (x, y) = do
  let stepForward dir_ = let (a, b) = stepIncrement dir_ in (mapAt (Map m) (x + a, y + b), (x + a, y + b))
  case stepForward dir of
    (Just Free, pos) -> return (pos, dir)
    (Just Obstacle, _) -> walk (Map m) (turnDirection 1 dir) (x, y)
    _ -> Nothing

guardWalk :: Map -> State GuardState Path
guardWalk m = do
  GuardState path direction position <- get
  case walk m direction position of
    Just (pos, dir) -> do
      let newSteps = path ++ [pos]
      put $ GuardState newSteps dir pos
      return newSteps
    _ -> return path

guardPath :: GuardState -> State GuardState Path -> Path
guardPath initialState s = go [] initialState
  where go lastTotalSteps lastState = 
          let (newSteps, newState) = runState s lastState
          in if newSteps == lastTotalSteps
              then newSteps
              else go newSteps newState

main :: IO ()
main = do
  contents <- lines <$> readFile "inputs/day6.txt"
  let parsed = Map . rights $ map (parse parser "") contents
      pos = (4, 0)
      t = mapAt parsed pos
      pos2 = (2, 3)
      t2 = mapAt parsed pos2

      -- initialState = GuardState [] UpDir (4, 6)
      initialState = GuardState [(36, 52)] UpDir (36, 52)
      l = guardPath initialState (guardWalk parsed)

  print parsed

  putStrLn $ "pos: " ++ show pos ++ ": " ++ show t
  putStrLn $ "pos2: " ++ show pos2 ++ ": " ++ show t2

  putStrLn "Result part 1:"
  print . size $ fromList l

  putStrLn "Result part 2:"
