module Main where

import Control.Monad
import Control.Monad.State
import Data.Either (rights)
import Data.List (intercalate)
import Data.Void
import Text.Megaparsec ( Parsec, anySingle, many, parse )

type Parser = Parsec Void String

data MapElement = Free | Obstacle

newtype Map = Map [[MapElement]]

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

type Steps = Int
type Position = (Int, Int)
data Direction = Up | Down | Left | Right deriving Show
data GuardState = GuardState Direction Position deriving Show

guardWalk :: Map -> State GuardState Steps
guardWalk m = do
  GuardState direction position <- get
  return 0

main :: IO ()
main = do
  contents <- lines <$> readFile "inputs/day6.txt"
  let parsed = Map . rights $ map (parse parser "") contents

  print parsed

  putStrLn "Result part 1:"

  putStrLn "Result part 2:"
