module Main where

import Data.Either (lefts, rights)
import Data.List (elemIndex, transpose)
import Data.List.HT (rotate)
import Data.Maybe (catMaybes, listToMaybe, mapMaybe)
import Data.Void
import Debug.Trace
import Replace.Megaparsec
import Text.Megaparsec
import Text.Megaparsec.Char

type Parser = Parsec Void String

data OrderRule = OrderRule Int Int deriving (Show)

newtype Update = Update [Int] deriving (Show)

integer :: Parser Int
integer = read <$> count 2 numberChar

parser :: Parser (Either Update OrderRule)
parser = do
  x <- integer
  separator <- anySingle
  case separator of
    '|' -> Right . OrderRule x <$> integer
    ',' -> do
      xs <- integer
      xss <- many (punctuationChar *> integer)
      return . Left $ Update (x : xs : xss)
    _ -> return . Left $ Update []

middleElement_ :: [a] -> Maybe a
middleElement_ = listToMaybe . shave
  where
    shave (x : xs : xss : xsss) = shave (tail $ init (x : xs : xss : xsss))
    shave [x, xs] = [x]
    shave [x] = [x]
    shave [] = []

middleElement :: Update -> Maybe Int
middleElement (Update l) = middleElement_ l

allRulesValid :: [OrderRule] -> Update -> Bool
allRulesValid rules update = and (map isRuleValid rules <*> [update])

isRuleValid :: OrderRule -> Update -> Bool
isRuleValid (OrderRule a b) (Update update) = posA `isLess` posB
  where
    posA = elemIndex a update
    posB = elemIndex b update
    isLess _ Nothing = True
    isLess Nothing _ = True
    isLess c d = c < d

applyRule :: OrderRule -> Update -> Update
applyRule rule update =
  if rule `isRuleValid` update
    then
      update
    else traceShow (update, swapElements rule update, rule) swapElements rule update

applyRules :: [OrderRule] -> Update -> Update
applyRules rules update =
  if allRulesValid rules update
    then update
    else applyRules rules (foldr applyRule update rules)

validRules :: [OrderRule] -> [Update] -> [Update]
validRules rules = filter (allRulesValid rules)

notValidRules :: [OrderRule] -> [Update] -> [Update]
notValidRules rules = filter (not . allRulesValid rules)

-- Requires that index(a) < index(b)
swapElements_ :: (Eq a) => a -> a -> [a] -> [a]
swapElements_ a b l = l2
  where
    (start_b, end_b) = span (/= b) l
    l1 = start_b ++ [a] ++ tail end_b
    (start_a, end_a) = span (/= a) l1
    l2 = start_a ++ [b] ++ tail end_a

swapElements :: OrderRule -> Update -> Update
swapElements (OrderRule a b) (Update l) = Update $ traceShow (l, a, b, swapElements_ b a l) (swapElements_ b a l)

main :: IO ()
main = do
  contents <- lines <$> readFile "inputs/day5.txt"
  let parsed = rights $ map (parse parser "") contents
      rules = rights parsed
      updates = lefts parsed
  mapM_ print parsed

  mapM_ print (validRules rules updates)

  putStrLn "Result part 1:"
  print $ sum (mapMaybe middleElement $ validRules rules updates)

  putStrLn "Result part 2:"
  print $ sum (mapMaybe (middleElement . applyRules rules) (notValidRules rules updates))
