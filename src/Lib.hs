module Lib
    ( someFunc
    ) where

import Data.List (unfoldr)


someFunc :: IO ()
someFunc = do
  putStrLn "sample unfoldr 1"
  let xs1 = funcUnfold1 [1..5]
  print xs1
  putStrLn ""

  putStrLn "sample unfoldr 2"
  let xs2 = funcUnfold2 1
  print xs2
  putStrLn ""

  putStrLn "sample unfoldr 3"
  let xs3 = funcUnfold3 (1, 10)
  mapM_ (putStrLn . toFormat31) xs3


-- [1,2,3,4,5]
funcUnfold1 :: [a] -> [a]
funcUnfold1 xs = unfoldr func1 xs


func1 :: [a] -> Maybe (a, [a])
func1 [] = Nothing
func1 (x:xs) = Just (x, xs)


-- [10,20,30,40]
funcUnfold2 :: Int -> [Int]
funcUnfold2 x = unfoldr func2 x


func2 :: Int -> Maybe (Int, Int)
func2 5 = Nothing
func2 x = Just (x * 10, x + 1)


-- [(1,10),(2,20),(3,30),(4,40),(5,50)]
funcUnfold3 :: (Int, Int) -> [(Int, Int)]
funcUnfold3 init_val = unfoldr func3 init_val

max_num = 5
step = 1

func3 :: (Int, Int) -> Maybe ((Int, Int), (Int, Int))
func3 xy@(num, val)
  | num > max_num  = Nothing
  | otherwise = Just ((num, val), (num + step, func31 val))

func31 :: Int -> Int
func31 x = x + 10

toFormat31 :: (Int, Int) -> String
toFormat31 (x, y) = (show x) ++ ", " ++ (show y)
