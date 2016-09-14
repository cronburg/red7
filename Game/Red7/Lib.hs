module Game.Red7.Lib
  ( shuffleCards
  , RandomGen
  , StdGen
  , mkStdGen
  , rotate
  ) where

import System.Random.Shuffle (shuffle, shuffle')
import System.Random

shuffleCards :: RandomGen gen => gen -> [a] -> [a]
shuffleCards g xs = shuffle' xs (length xs) g

-- http://stackoverflow.com/a/16379034
rotate :: Int -> [a] -> [a]
rotate _ [] = []
rotate n xs = zipWith const (drop n (cycle xs)) xs

