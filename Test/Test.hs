{-# LANGUAGE PackageImports, NamedFieldPuns #-}
module Main where
import Game.Red7.Types
import Game.Red7.Engine
import Game.Red7.Lib
import Game.Red7.Examples.Greedy
import Control.Monad.State.Lazy (execState, runStateT, liftIO)
import Data.List
import Control.Lens

import System.IO.Unsafe (unsafePerformIO)
import Data.Monoid
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.HUnit
import Test.QuickCheck (Property, quickCheck, (==>))
import qualified Test.QuickCheck.Monadic as TQM

finalPlayer :: Player
finalPlayer = unsafePerformIO $ do
  (p,g) <- runStateT playGame greedyGame
  return p

finalGame   :: Game
finalGame   = unsafePerformIO $ do
  (p,g) <- runStateT playGame greedyGame
  return g

t_winner_alive = finalPlayer @?= p1 finalGame
t_one_player   = 1 @?= (length . _players $ finalGame)

finalCards :: Game -> [Card]
finalCards g =
      g ^. deck
  ++  g ^. canvas
  ++  concat (map (\p -> (p ^. hand) ++ (p ^. palette)) $ g ^. players)

t_cards_distinct =
  (length (nub $ finalCards finalGame))
  @?=
  (length $ finalCards finalGame)

newGame i =
  mode .~ StdIO $ shuffle .~ shuffleCards (mkStdGen i) $ greedyGame

{-
p_cards_distinct i = True ==> TQM.monadicIO $ do
  TQM.run $ do
    (p,g) <- runStateT playGame (newGame i)
    let a = (length . nub . finalCards $ g)
    let b = (length . finalCards $ g)
    if a /= b
      then do putStrLn $ "lengths = " ++ show a ++ ", " ++ show b
              putStrLn $ show $ sort $ finalCards g
              putStrLn $ show $ g
      else return ()
  TQM.assert $ a == b
-}
main :: IO ()
main = defaultMainWithOpts
  [ testCase      "rev"                 testRev
  , testProperty  "listRevRevId"        propListRevRevId
  , testCase      "Winner alive"        t_winner_alive
  , testCase      "One player left"     t_one_player
  , testCase      "Cards neither created nor destroyed" t_cards_distinct
--  , testProperty  "Cards neither created nor destroyed" p_cards_distinct
  ] mempty

-- Example assertion and property:
testRev :: Assertion
testRev = reverse [1, 2, 3] @?= [3, 2, 1]

propListRevRevId :: [Int] -> Property
propListRevRevId xs = not (null xs) ==> reverse (reverse xs) == xs

