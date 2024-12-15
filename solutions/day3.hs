module Main where

import Data.Either
import Data.Void
import Replace.Megaparsec
import Text.Megaparsec
import Text.Megaparsec.Char

data MyData = Do Bool | Mul Int deriving (Show)

type Parser = Parsec Void String

startmul :: Parser String
startmul = string "mul("

endmul :: Parser Char
endmul = char ')'

comma :: Parser Char
comma = char ','

integer :: Parser Int
integer = read <$> count' 1 3 numberChar

mul :: Parser MyData
mul = do
  _ <- startmul
  x <- integer
  _ <- comma
  y <- integer
  _ <- endmul
  return $ Mul (x * y)

trash :: Parser MyData
trash = do
  x <- try (string "don't()") <|> string "do()"
  if x == "do()"
    then return $ Do True
    else return $ Do False

main :: IO ()
main = do
  contents <- readFile "inputs/day3.txt"
  let parser = sepCap (mul <|> trash)
      inputs = parseMaybe parser contents

  mapM_ print inputs

  putStrLn "Result part 1:"
  case inputs of
    Just parsed -> print $ foldr f 0 (rights parsed)
      where
        f (Do _) x = x
        f (Mul x) y = x + y
    _ -> return ()

  putStrLn "Result part 2:"
  case inputs of
    Just parsed -> print $ foldl f (0, True) (rights parsed)
      where
        f (x, _) (Do d)  = (x, d)
        f (y, shouldDo) (Mul x)  = if shouldDo then (x + y, shouldDo) else (y, shouldDo)
    _ -> return ()
