{-# LANGUAGE RankNTypes, KindSignatures, FlexibleContexts,
             BangPatterns, LambdaCase #-}
module Game.Red7.Engine where
import Data.List (maximumBy, group, groupBy, sort, elemIndex)
import Data.Maybe (maybeToList, fromJust)
import Control.Monad.State.Lazy (put, get, liftIO, execState, runState)
import Control.Monad (replicateM, liftM)
import Control.Monad.Trans (lift)
import Control.Lens
import Control.Monad.State.Class (MonadState)
import Data.Ord
import System.IO.Unsafe (unsafePerformIO)

import Game.Red7.Types
import Game.Red7.Lib

import Debug.Trace (trace, traceM)

shuffleDeck :: GameState ()
shuffleDeck = appFieldS _shuffle deck deck

-- Draw n cards from the deck
draw :: Int -> GameState [Card]
draw n = do
  g <- get
  let cs    = take n $ _deck g
      deck' = drop n $ _deck g
  put $ deck .~ deck' $ g
  return cs

-- TODO: figure out how to make lenses first class (pass them around)
-- without the type system barfing.

-- Deal n cards to the first player in the list of players
-- (either to their palette or to their hand)
deal getter setter n = do
  cs <- draw n
  g  <- get
  let p  = p1 g
  -- TODO: omg I need Internet so I can remember how to lift / transform.
  let p' = execState (appField (\xs -> cs ++ xs) getter setter) p
--  let p' = setter .~ (\xs -> cs ++ (p ^. getter)) $ p
  put $ players .~ (p' : (tail $ g ^. players)) $ g

dealHand    = deal hand hand
dealPalette = deal palette palette

-- Apply fncn to field f in the state monad
appField fncn f f'  = get >>= \state -> put $ f' .~ (fncn $ state ^. f) $ state

-- appField, but where the fncn also takes in the current state
appFieldS fncn f f' = get >>= \state -> put $ f' .~ (fncn state $ state ^. f) $ state

rotateField f f' n = appField (rotate n) f f'

-- TODO: figure out how to pass Getter and Setter lense type(s) in
-- one argument.
rotatePlayers :: Int -> GameState ()
rotatePlayers = rotateField players players

rotateHand n = appField (rotate n) hand hand

--do
--  g <- get
--  let ps = _players g
--  put $ g { _players = rotate n ps }

-- Deal n cards to the first player and rotate the players
dealAndShift :: Int -> GameState ()
dealAndShift n = do
  dealHand n
  rotatePlayers 1

--dealXS :: Int -> GameState ()
dealXS dealFncn n = do
  g <- get
  replicateM (length $ g ^. players) (dealFncn n >> rotatePlayers 1)
  return ()

dealHands    = dealXS dealHand
dealPalettes = dealXS dealPalette

setupPlayers = do
  g <- get
  rotatePlayers $ 1 + maxByIndex (comparing $ head . _palette) (g ^. players)
--  maximumBy (fst . (^. palette)) (g ^. players)
  return ()

dealCanvas = do
  g <- get
  let c = head $ g ^. deck
  appField tail deck deck
  appField (\cvs -> c : cvs) canvas canvas

-- Shuffle deck, deal 7 cards each, deal 1 card to each player's palette,
-- and shift the players list such that the player to the left of the
-- player with the highest:
setupGame :: GameState ()
setupGame = do
  shuffleDeck
  dealHands    7
  dealPalettes 1
  setupPlayers
  dealCanvas

-- Take a card from the given record location (lens) of the game
takeCard src src' = do
  g <- get
  let cs  = g ^. src
      c   = head cs
      cs' = tail cs
  put $ src' .~ cs' $ g
  return c

-- Put the given card at the head of the list of the given record location (lens)
putCard dest dest' c = do
  g <- get
  let cs' = c : (g ^. dest)
  put $ dest' .~ cs' $ g

--moveCard :: (Getting [Card] s [Card]) -> (ASetter s s [Card] [Card]) -> GameState ()
moveCard s s' d d' = takeCard s s' >>= putCard d d'

play' :: Card -> PlayerState Bool
play' c = do
  p <- get
  case elemIndex c (p ^. hand) of
    Nothing -> return False -- Can't play that card
    Just i  -> do
      rotateHand i
      p <- get
      let cs  = _hand p
          c   = head cs
          cs' = tail cs
          ps' = c : _palette p
      put $ p { _hand = cs', _palette = ps' }
      return True

-- TODO: lifting between Game and Player states
play :: Card -> GameState Bool
play c = do
  g <- get
  let (s,p) = runState (play' c) (p1 g)
  put $ g { _players = p : (tail . _players $ g) }
  return s

-- Discard the given card from the player's hand
discard' :: Card -> PlayerState Bool
discard' c = do
  p <- get
  case elemIndex c (p ^. hand) of
    Nothing -> return False
    Just i -> do
      rotateHand $ (i - 1) --`mod` (length $ _hand p)
      p <- get
      --traceM $ "XXX: Card = " ++ (show $ head . _hand $ p)
      --traceM $ "p = " ++ show p
      put $ p { _hand = tail . _hand $ p }
      return True

discard :: Card -> GameState Bool
discard c = do
  g <- get
  let (s,p) = runState (discard' c) (p1 g)
  traceM $ "DISCARD: " ++ (show p)
  if s
    then put $ g { _players = p : (tail . _players $ g)
                 , _canvas  = c : (_canvas g) }
    else return ()
  return s

killPlayer :: GameState Bool
killPlayer = get >>= (\g -> put $ g { _players = tail . _players $ g }) >> return True

doAct :: Action -> GameState Bool
doAct a = do
  case a of
    Play c -> play c
    Discard c -> discard c
    PlayDiscard c1 c2 -> play c1 >> discard c2
    DoNothing -> killPlayer

describe :: Action -> GameState ()
describe a = do
  g <- get
  let p = head . _players $ g
  -- TODO lift IO
  traceM $ (_name p) ++ " does " ++ (show a)

isGameOver g = (length $ _players g) <= 1

-- TODO: remove all calls to 'head' of lists?
playGame' :: GameState Player
playGame' = do
  g <- get
  -- TODO: refer to deckbuild for how I handled lift / MonadTrans.
  -- Maybe there's a deriving instance now?
  if isGameOver g
    then return $ head . _players $ g
    else do
      let !a = unsafePerformIO $ (_strategy . head . _players $ g) g
      describe a
      doAct a
      rotatePlayers 1
      playGame'

playGame = setupGame >> playGame'

------------------------------------------------------------------------------
-- Play the top card of the 1st player's hand to their palette
playToPalette :: Game -> Game
playToPalette = execState $ do
  g <- get
  let ps  = g ^. players
  let p   = p1 g
  let p'  = hand .~ (tail $ p ^. hand) $ p
  let c   = head $ p ^. hand
  let p'' = palette .~ (c : p ^. palette) $ p'
  put $ players .~ (p'' : tail ps) $ g

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

currWinner :: Game -> Maybe Player
currWinner g = winner (head $ g ^. canvas) (g ^. players)

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
most_row' [] = (0, defCard)
most_row' cs =
  (\case
    Just xs -> ((+) 1 $ length xs, snd . last $ xs)
    Nothing -> (1, last cs))
  $ maybeMaxBy (\a b -> Just $ length a `compare` length b)
  $ filter (((==) 1) . fst . head)
  $ groupBy (\a b -> fst a == fst b)
  $ zipWith (\a b -> (_num a - _num b, a)) cs (defCard : init cs)

most_row = maximumBy $ comparing $ most_row' . sort . _palette

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

