module Main where

import Data.Either (rights)
import Data.List (transpose)
import Data.List.HT (rotate)
import Data.Void
import Debug.Trace
import Replace.Megaparsec
import Text.Megaparsec
import Text.Megaparsec.Char

type Parser = Parsec Void String

xmas :: Parser String
xmas = string "XMAS"

samx :: Parser String
samx = string "SAMX"

incrementalRotation :: Int -> [String] -> [String]
incrementalRotation step strings =
  if step >= 0
    then zipWith drop [0, step ..] strings
    else zipWith (\n l -> reverse $ take (length l + n) l) [0, step ..] strings

countHorizontal :: Parser String -> [String] -> Int
countHorizontal parser l = sum $ [length (rights x) | Just x <- map (parseMaybe (sepCap parser)) l]

countVertical :: Parser String -> [String] -> Int
countVertical parser l = countHorizontal parser (transpose l)

countDiagonal :: Parser String -> [String] -> Int
countDiagonal parser l = a + b
  where
    a = sum $ map (countVertical parser . incrementalRotation 1) (groupWithNextN 4 l)
    b = sum $ map (countVertical parser . incrementalRotation (-1)) (groupWithNextN 4 l)

groupWithNextN :: Int -> [a] -> [[a]]
groupWithNextN n (x : xs) = (x : take (n - 1) xs) : groupWithNextN n xs
groupWithNextN _ [] = []

countTotal :: [String] -> Int
countTotal l = sum (listOfFunctions <*> [l])
  where
    listOfFunctions = [countHorizontal, countVertical, countDiagonal] <*> [xmas, samx]


merge3Consecutive :: [[a]] -> [[a]]
merge3Consecutive (x:xs:xss:xsss) = (x ++ xs ++ xss):merge3Consecutive xsss
-- merge3Consecutive (x:xs:xss) = (x ++ xs):merge3Consecutive xss
-- merge3Consecutive (x:xs) = x:merge3Consecutive xs
merge3Consecutive _ = []

convolution :: String -> [String] -> Int
convolution mask strings = length $ filter maskCompare (filter (\x -> length x == 9) listOfGroupsOf9)
  where l = map (groupWithNextN 3 . transpose) (groupWithNextN 3 strings)
        listOfGroupsOf9 = merge3Consecutive . concat $ concat l
        maskCompare x = and $ zipWith f mask x
          where
            f a b
              | a == '_' = True
              | b == '_' = True
              | otherwise = a == b

main :: IO ()
main = do
  contents <- lines <$> readFile "inputs/day4.txt"

  mapM_ print contents

  let mask = "M_M_A_S_S"

  putStrLn "Result part 1:"
  print $ countTotal contents
  putStrLn "Result part 2:"
  print $ sum (map (convolution mask) [contents, transpose contents, map reverse contents, map reverse $ transpose contents])
