module Game.Red7.Engine where
import Data.List (maximumBy, group, groupBy, sort)
import Game.Red7.Types
import Data.Maybe (maybeToList, fromJust)

winner :: Card -> [Player] -> Maybe Player
winner top = case color top of
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
  . (filter ((>) n . num))

most_below n = maybeMaxBy (\p1 p2 ->
  let a = (most_below' n . palette) p1
      b = (most_below' n . palette) p2
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
    $ zipWith (\a b -> (a, num b - num a)) cs (tail cs)

most_row = maximumBy (\p1 p2 ->
  (most_row' . palette) p1 `compare` (most_row' . palette) p2)

-----------------------------
-- http://stackoverflow.com/a/16109302
rmdups :: (Ord a) => [a] -> [a]
rmdups = map head . group . sort

most_diff_color' = (\cs' -> (length cs', maximum cs')) . rmdups

most_diff_color :: [Player] -> Player
most_diff_color = maximumBy (\p1 p2 ->
  (most_diff_color' . palette) p1 `compare` (most_diff_color' . palette) p2)

-----------------------------
most_even' = (\cs' ->
  if length cs' > 0
    then (length cs', maximum cs')
    else (0, defCard))
  . (filter (even . num))

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
  let a = (most_even' . palette) p1
      b = (most_even' . palette) p2
  in  if fst a == 0 && fst b == 0
        then Nothing
        else Just $ a `compare` b)

-----------------------------
most_color' cs  = maximumBy cmp_test_fst $
  [((\cs' -> (length cs', c)) . (filter ((==) c . color))) cs | c <- colors]

most_color :: [Player] -> Player
most_color = maximumBy (\p1 p2 ->
  (most_color' . palette) p1 `compare` (most_color' . palette) p2)

-----------------------------
most_number' :: [Card] -> (Int, Int)
most_number' cs = maximumBy cmp_test_fst $
  [((\cs' -> (length cs', i)) . (filter ((==) i . num))) cs | i <- numbers]

most_number :: [Player] -> Player
most_number = maximumBy (\p1 p2 ->
  (most_number' . palette) p1 `compare` (most_number' . palette) p2)

-----------------------------
highest :: [Player] -> Player
highest = maximumBy (\p1 p2 ->
  maximum (palette p1) `compare` maximum (palette p2))

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
