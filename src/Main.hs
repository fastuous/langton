{-# LANGUAGE FlexibleContexts #-}
module Main where

import           Data.Array.Repa     as R hiding ((++))
import qualified Data.Map.Strict     as M
import           Data.Maybe          (fromMaybe)
import           System.Console.ANSI (hideCursor, setCursorPosition)

startLoop :: String
startLoop = "0222222220000002170140142000002022222202000002720000212000002120000212\
  \00000202000021200000272000021200000212222221222220207107107111112022222222222220"

(%) :: (Source r a) => Array r DIM2 a -> (Int, Int) -> a
arr % (i, j) = arr ! (Z :. i `mod` rows :. j `mod` cols) where
  (Z :. rows :. cols) = extent arr

printArr :: Array U DIM2 Char -> String
printArr arr = concat [getRow i arr ++ "\n" | i <- [0..rows - 1]] where
  getRow i a = [a ! (Z :. i :. j) | j <- [0..cols-1]]
  (Z :. rows :. cols) = extent arr

update :: (Source r Char) => Array r DIM2 Char -> M.Map String Char -> Array D DIM2 Char
update arr table = R.traverse arr id f where
  f _ (Z :. i :. j) = fromMaybe '0' $ M.lookup (neighbors i j) table
  neighbors i j = [arr % (i, j), arr % (i - 1, j), arr % (i, j + 1),
                   arr % (i + 1, j), arr % (i, j - 1)]

inset :: (Source r Char) => Array r DIM2 Char -> Int -> Int -> Array D DIM2 Char
inset arr newRows newCols = R.traverse arr newShape f where
  newShape = const (Z :. newRows :. newCols)
  f idx (Z :. i :. j) = if inBounds i j
                        then idx (Z :. i - rOffset :. j - cOffset)
                        else '0'
  inBounds i j = i >= rOffset && i < (rows + rOffset) &&
                 j >= cOffset && j < (cols + cOffset)
  rOffset = (newRows - rows) `div` 2
  cOffset = (newCols - cols) `div` 2
  (Z :. rows :. cols) = extent arr

buildMap :: [String] -> M.Map String Char
buildMap rules = M.fromList $ rules >>= \(c:t:r:b:l:i:_) ->
  [([c,t,r,b,l], i), ([c,r,b,l,t], i), ([c,b,l,t,r], i), ([c,l,t,r,b], i)]

loop :: (Source r Char) => M.Map String Char -> Array r DIM2 Char -> IO ()
loop ruleMap arr = do
  newArr <- computeP $ update arr ruleMap
  setCursorPosition 0 0
  putStr $ (\c -> if c == '0' then ' ' else c) <$> printArr newArr
  loop ruleMap newArr

main :: IO ()
main = do
  hideCursor
  rules <- lines <$> readFile "transTable.txt"
  let ruleMap = buildMap rules
  let startArr = fromListUnboxed (Z :. 10 :. 15) startLoop
  loop ruleMap $ inset startArr 100 300
