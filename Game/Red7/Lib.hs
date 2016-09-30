module Game.Red7.Lib
  ( shuffleCards
  , RandomGen
  , StdGen
  , mkStdGen
  , rotate
  , maybeMaxBy
  , maxByIndex
  , hoist
  , generalize
  ) where

import Data.Functor.Identity (Identity(..))
import Control.Monad.State.Lazy (StateT(..), State, execState, runState)
import System.Random.Shuffle (shuffle, shuffle')
import System.Random
import Data.Ord
import Data.List

shuffleCards :: RandomGen gen => gen -> [a] -> [a]
shuffleCards g xs = shuffle' xs (length xs) g

-- http://stackoverflow.com/a/16379034
rotate :: Int -> [a] -> [a]
rotate _ [] = []
rotate n xs = zipWith const (drop n (cycle xs)) xs

maybeMaxBy        :: (a -> a -> Maybe Ordering)
                  -> [a] -> Maybe a
maybeMaxBy _ []   = Nothing
maybeMaxBy cmp xs = foldl maxBy Nothing $ map Just xs
                      where
  maxBy Nothing Nothing = Nothing
  maxBy Nothing y = y
  maxBy x Nothing = x
  maxBy (Just x) (Just y) = case cmp x y of
    Just GT -> Just x
    Just LT -> Just y
    Just EQ -> Just y
    _       -> Nothing

-- http://stackoverflow.com/a/18288490
maxByIndex :: (a -> a -> Ordering) -> [a] -> Int
maxByIndex fncn = fst . maximumBy (\a b -> fncn (snd a) (snd b)) . zip [0..]

--maxByIndex fncn = fst . (\f xs -> maximumBy f $ map (\a b -> (snd a, snd b)) xs) . zip [0..]
-- ((uncurry fncn) . (\a b -> (snd a, snd b))) . zip [0..]

--(zip [0..] vs)

-- http://stackoverflow.com/a/17325795/1542000
-- hoist == hoistState
hoist :: Monad m => State s a -> StateT s m a
hoist = StateT . (return .) . runState

generalize :: Monad m => Identity a -> m a
generalize = return . runIdentity

