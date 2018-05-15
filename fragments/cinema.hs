module Codewars.G964.Movie where

movie :: Int -> Int -> Double -> Int
movie card ticket perc = find bIsCheaper [0..]
where
  bIsCheaper visits = ceiling $ bCost visits < aCost visits
  bCost visits =
