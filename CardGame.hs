{-# LANGUAGE ScopedTypeVariables #-}
module Main where 

import Control.Monad (forM_)
import Data.List (sort)
import System.IO

main :: IO ()
main = do
  hSetBuffering stdin LineBuffering
  s <- getLine
  let noOfCases = read s :: Integer

  forM_ [1..noOfCases] (\i -> do
    (k, ns) <- parseCase
    putStrLn $ "Case #" ++ show i ++ ": " ++ show (sumMax ns k `mod` 1000000007))

parseCase :: IO (Int, [Int])
parseCase = do
  spec <- getLine
  nums <- getLine
  let [_, k] = parse spec
      ns     = parse nums
  return (k, ns)

parse :: String -> [Int]
parse = (map read) . words

sumMax :: [Int] -> Int -> Int
sumMax = sumMax' . reverse . sort

sumMax' :: [Int] -- ^ list of values sorted in reverse
        -> Int -> Int
sumMax' ns k 
  | length ns < k = 0
  | otherwise     = let (h:t) = ns in h * choose (length t) (k - 1) + sumMax' t k

choose :: Int -> Int -> Int
choose n 0 = 1
choose 0 k = 0
choose n k = choose (n-1) (k-1) * n `div` k 

