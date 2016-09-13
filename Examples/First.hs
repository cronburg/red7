module Examples.First where
import Game.Red7.Types
import Game.Red7.Engine

p1 = defPlayer
  { name  = "Karl"
  , palette = [Card {color = RED, num = 7}]
  }

p2 = defPlayer
  { name  = "Harry"
  , palette = [Card {color = VIOLET, num = 1}]
  }

p3 = defPlayer
  { name  = "Lydia"
  , palette = [Card {color = YELLOW, num = 6}]
  }

p4 = defPlayer
  { name  = "Charles"
  , palette = [Card {color = ORANGE, num = 6}]
  }

players = [p1,p2,p3,p4]

