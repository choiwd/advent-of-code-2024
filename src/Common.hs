module Common (groupWithNextN, middleElement, swapElements) where

import Data.List.HT ()
import Data.Maybe

groupWithNextN :: Int -> [a] -> [[a]]
groupWithNextN n (x : xs) = (x : take (n - 1) xs) : groupWithNextN n xs
groupWithNextN _ [] = []

middleElement :: [a] -> Maybe a
middleElement = listToMaybe . shave
  where
    shave (x : xs : xss : xsss) = shave (tail $ init (x : xs : xss : xsss))
    shave [x, _] = [x]
    shave [x] = [x]
    shave [] = []

-- Requires that index(a) < index(b) 
swapElements :: Eq a => a -> a -> [a] -> [a]
swapElements a b l =  l2
  where (start_b, end_b) = span (/= b) l
        l1 = start_b ++ [a] ++ tail end_b
        (start_a, end_a) = span (/= a) l1
        l2 = start_a ++ [b] ++ tail end_a
