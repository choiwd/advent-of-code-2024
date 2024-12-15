module Main where

import Data.List.HT
import Data.Maybe (mapMaybe)
import Data.Void
import Debug.Trace
import Text.Megaparsec
  ( Parsec,
    many,
    parseMaybe,
    some,
    (<|>),
  )
import Text.Megaparsec.Char

count :: (a -> Bool) -> [a] -> Int
count p = length . filter p

type Parser = Parsec Void String

integer :: Parser Int
integer = read <$> some numberChar

ws :: Parser Char
ws = char ' '

parser :: Parser [Int]
parser = many tries
  where
    tries = integer <|> (ws *> integer)

rotateList :: Int -> [a] -> [a]
rotateList _ [] = []
rotateList n x = zipWith const (drop n (cycle x)) x

isDecreasingByOneOrTwoOrThree :: [Int] -> Bool
isDecreasingByOneOrTwoOrThree l = all (\x -> 0 < x && x <= 3) (init (zipWith (-) l l2))
  where
    l2 = rotateList 1 l

isIncreasingByOneOrTwoOrThree :: [Int] -> Bool
isIncreasingByOneOrTwoOrThree l = all (\x -> 0 < x && x <= 3) (init (zipWith (-) l2 l))
  where
    l2 = rotateList 1 l

dampener :: [Int] -> [[Int]]
dampener a = map (\(x, _, xs) -> x ++ xs) (splitEverywhere a)

main :: IO ()
main = do
  contents <- lines <$> readFile "inputs/day2.txt"
  let inputs = mapMaybe (parseMaybe parser) contents

      res = map (\x -> (x, isDecreasingByOneOrTwoOrThree x, isIncreasingByOneOrTwoOrThree x, any isDecreasingByOneOrTwoOrThree (dampener x), any isIncreasingByOneOrTwoOrThree (dampener x))) inputs
  mapM_ print res

  putStrLn "Result part 1:"
  print $ count (\(_, a, b, _, _) -> a || b) res

  putStrLn "Result part 2:"
  print $ count (\(_, a, b, c, d) -> a || b || c || d) res
