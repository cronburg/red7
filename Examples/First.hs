module Examples.First where
import Game.Red7.Types
import Game.Red7.Engine
import Game.Red7.Lib

import Control.Monad.State.Lazy (execState)
import Control.Lens

p1' = defPlayer
  { _name  = "Karl"
  , _palette = [Card {_color = RED, _num = 7}]
  }

p2 = defPlayer
  { _name  = "Harry"
  , _palette = [Card {_color = VIOLET, _num = 1}]
  }

p3 = defPlayer
  { _name  = "Lydia"
  , _palette = [Card {_color = YELLOW, _num = 6}]
  }

p4 = defPlayer
  { _name  = "Charles"
  , _palette = [Card {_color = ORANGE, _num = 6}]
  }

fstPlayers = [p1',p2] --,p3,p4]

noPalette p = p {_palette = []}
fstPlayers' = map noPalette fstPlayers

game = (defGame (mkStdGen 31)) {_players = fstPlayers'}

--tests = map (\(x,y) -> execState (setupGame >> x) game) [
--  (discard (Card GREEN 3), 

