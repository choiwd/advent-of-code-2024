module Main where

import Common
import Control.Monad
import Data.Char (digitToInt)
import Data.List (sort)
import System.IO
import Text.Regex.TDFA

str2ints :: String -> [Int]
str2ints = map digitToInt

dist :: (Num a) => a -> a -> a
dist a b = abs $ a - b

count :: (Eq a) => a -> [a] -> Int
count x l = length $ filter (== x) l

main :: IO ()
main = do
  let regex = "([0-9]+) +([0-9]+)"
  contents <- lines <$> readFile "inputs/day1.txt"
  let f = \x -> x =~ regex :: (String, String, String, [String])
      g = \(_, _, _, x) -> x

      inputs = map (map (read :: String -> Int) . g . f) contents
      l1 = sort $ map head inputs
      l2 = sort $ map last inputs

      l3 = map (\x -> x * count x l2) l1

  mapM_ print inputs
  putStrLn "Result part 1:"
  print $ sum (zipWith dist l1 l2)

  putStrLn "Result part 2:"
  print $ sum l3
