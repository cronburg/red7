module Game.Red7.Examples.Greedy where
import Game.Red7.Examples.First (game)
import Game.Red7.Types
import Game.Red7.Engine
import Game.Red7.Lib
import Game.Red7.AI.Greedy

import Control.Lens
import Control.Monad.State.Lazy (execState, runState)
import Control.Monad.Trans (lift)

greedyGame =
  game { _players = map (strategy .~ greedy) $ _players game }

