{-# LANGUAGE RankNTypes, KindSignatures, FlexibleContexts #-}
module Game.Red7.Engine where
import Data.List (maximumBy, group, groupBy, sort)
import Data.Maybe (maybeToList, fromJust)
import Control.Monad.State.Lazy (put, get)
import Control.Monad (replicateM)
import Control.Lens
import Control.Monad.State.Class (MonadState)

import Game.Red7.Types
import Game.Red7.Lib

shuffleDeck :: GameState ()
shuffleDeck = do
  g <- get
  put $ g { _deck = _shuffle g $ _deck g }

-- Draw n cards from the deck
draw :: Int -> GameState [Card]
draw n = do
  g <- get
  let cs    = take n $ _deck g
  let deck' = drop n $ _deck g
  put $ deck .~ deck' $ g
  return cs

-- TODO: figure out how to make lenses first class (pass them around)
-- without the type system barfing.

-- Deal n cards to the first player in the list of players
-- (either to their palette or to their hand)
dealHand n = do
  cs <- draw n
  g  <- get
  let p1  = head (g ^. players)
  let p1' = hand .~ (cs ++ (p1 ^. hand)) $ p1
  put $ players .~ (p1' : (tail $ g ^. players)) $ g

dealPalette n = do
  cs <- draw n
  g  <- get
  let p1 = head (g ^. players)
  let p1' = palette .~ (cs ++ (p1 ^. palette)) $ p1
  put $ players .~ (p1' : (tail $ g ^. players)) $ g

-- Rotate 
rotatePlayers :: Int -> GameState ()
rotatePlayers n = do
  g <- get
  put $ g { _players = rotate n (_players g) }

-- Deal n cards to the first player and rotate the players
dealAndShift :: Int -> GameState ()
dealAndShift n = do
  dealHand n
  rotatePlayers 1

dealHands :: Int -> GameState ()
dealHands n = do
  g <- get
  replicateM (length $ g ^. players) (dealHand n >> rotatePlayers 1)
  return ()

dealPalettes :: Int -> GameState ()
dealPalettes n = do
  g <- get
  replicateM (length $ g ^. players) (dealPalette n >> rotatePlayers 1)
  return ()

setupPlayers = undefined

-- Shuffle deck, deal 7 cards each, deal 1 card to each player's palette,
-- and shift the players list such that the player to the left of the
-- player with the highest:
setupGame :: GameState ()
setupGame = do
  shuffleDeck
  dealHands    7
  dealPalettes 1
--  setupPlayers

------------------------------------------------------------------------------
winner :: Card -> [Player] -> Maybe Player
winner top = case _color top of
  VIOLET -> most_below 4
  INDIGO -> Just . most_row
  BLUE   -> Just . most_diff_color
  GREEN  -> most_even
  YELLOW -> Just . most_color
  ORANGE -> Just . most_number
  RED    -> Just . highest

-----------------------------
most_below' n = (\cs' ->
  if length cs' > 0
    then (length cs', maximum cs')
    else (0, defCard))
  . (filter ((>) n . _num))

most_below n = maybeMaxBy (\p1 p2 ->
  let a = (most_below' n . _palette) p1
      b = (most_below' n . _palette) p2
  in  if fst a == 0 && fst b == 0
        then Nothing
        else Just $ a `compare` b)

-----------------------------
most_row' cs = case length cs of
  0 -> (0, defCard) -- should never have an empty palette
  1 -> (1, head cs)
  _ ->
    maximumBy cmp_test_fst
    $ map (\cs -> (length cs, (fst . head) cs))
    $ filter (\(c:cs) -> snd c == 1)
    $ groupBy (\a b -> snd a == snd b)
    $ zipWith (\a b -> (a, _num b - _num a)) cs (tail cs)

most_row = maximumBy (\p1 p2 ->
  (most_row' . _palette) p1 `compare` (most_row' . _palette) p2)

-----------------------------
-- http://stackoverflow.com/a/16109302
rmdups :: (Ord a) => [a] -> [a]
rmdups = map head . group . sort

most_diff_color' = (\cs' -> (length cs', maximum cs')) . rmdups

most_diff_color :: [Player] -> Player
most_diff_color = maximumBy (\p1 p2 ->
  (most_diff_color' . _palette) p1 `compare` (most_diff_color' . _palette) p2)

-----------------------------
most_even' = (\cs' ->
  if length cs' > 0
    then (length cs', maximum cs')
    else (0, defCard))
  . (filter (even . _num))

maybeMaxBy        :: (Player -> Player -> Maybe Ordering) -> [Player] -> Maybe Player
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

most_even :: [Player] -> Maybe Player
most_even = maybeMaxBy (\p1 p2 ->
  let a = (most_even' . _palette) p1
      b = (most_even' . _palette) p2
  in  if fst a == 0 && fst b == 0
        then Nothing
        else Just $ a `compare` b)

-----------------------------
most_color' cs  = maximumBy cmp_test_fst $
  [((\cs' -> (length cs', c)) . (filter ((==) c . _color))) cs | c <- colors]

most_color :: [Player] -> Player
most_color = maximumBy (\p1 p2 ->
  (most_color' . _palette) p1 `compare` (most_color' . _palette) p2)

-----------------------------
most_number' :: [Card] -> (Int, Int)
most_number' cs = maximumBy cmp_test_fst $
  [((\cs' -> (length cs', i)) . (filter ((==) i . _num))) cs | i <- numbers]

most_number :: [Player] -> Player
most_number = maximumBy (\p1 p2 ->
  (most_number' . _palette) p1 `compare` (most_number' . _palette) p2)

-----------------------------
highest :: [Player] -> Player
highest = maximumBy (\p1 p2 ->
  maximum (_palette p1) `compare` maximum (_palette p2))

{- http://asmadigames.com/Red7Rules.pdf -}
{-
RED    = high card
ORANGE = most of a number
YELLOW = most of a color
GREEN  = most even
BLUE   = most different colors
INDIGO = most in a row
VIOLET = most below 4
-}

