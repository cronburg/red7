module Game.Red7.AI.Greedy where
-- A basic 'greedy' Red 7 player
import Control.Lens

import Game.Red7.Types
import Game.Red7.Engine
import Debug.Trace (trace)
import Control.Monad.State.Lazy (execState, get, put)

head' xs = head xs

-- Whether or not playing the top card of the 1st player's hand
-- makes that player the current winner.
canPlay :: Game -> Bool
canPlay g = (Just $ head' (g ^. players)) == (currWinner $ execState (play $ head . _hand . head . _players $ g) g)

greedy :: Game -> IO Action
greedy = return . greedy'

greedy' :: Game -> Action
greedy' g
    | 0 == length cs = DoNothing
    | canPlay g      = Play $ head' cs
    | otherwise      = case (rec, length cs, recDiscard) of
        (DoNothing, _, DoNothing)                -> DoNothing
        (DoNothing, 0, _)                        -> DoNothing
        (DoNothing, _, Play c)                   -> PlayDiscard c (head cs)
        (DoNothing, _, pd @ (PlayDiscard c1 c2)) -> pd
        _                                        -> rec
  where
    ps  = g ^. players
    p1  = head' ps
    p1' = hand .~ (tail cs) $ p1
    cs  = p1 ^. hand
    rec = greedy' $ players .~ (p1' : tail ps) $ g
    recDiscard = greedy' $ execState (discard $ head cs) g

