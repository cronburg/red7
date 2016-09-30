{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving #-}
module Game.Red7.Types where
import Game.Red7.Lib
import Control.Monad.State.Lazy
import Control.Lens

data Card = Card
  { _color :: Color
  , _num   :: Int
  }

instance Show Card where
  show c = "Card " ++ (show . _color) c ++ " " ++ (show . _num) c

data Color = VIOLET | INDIGO | BLUE
  | GREEN | YELLOW | ORANGE | RED
  deriving (Eq, Ord, Show, Enum, Bounded)

-- Invalid Card by default:
defCard = Card {_color = VIOLET, _num = 0}
newCard (c,n) = defCard {_color = c, _num = n}

colors  = [(minBound :: Color) ..]
numbers = [1 .. length colors]

-- All 49 cards in the deck:
cards = map newCard $ foldl (++) [] $
  [[(c,n) | c <- colors] | n <- numbers]

data Action =
    Play Card
  | Discard Card
  | PlayDiscard Card Card
  | DoNothing
  deriving (Eq, Show)

type Strategy = Game -> IO Action

data Player = Player
  { _name     :: String
  , _hand     :: [Card]
  , _palette  :: [Card]
  , _strategy :: Strategy
  }

instance Show Player where
  show p =
    "    name    = " ++ show (_name p) ++ "\n" ++
    "    hand    = " ++ show (_hand p) ++ "\n" ++
    "    palette = " ++ show (_palette p) ++ "\n"

instance Eq Player where
  p1 == p2 = _name p1 == _name p2

defPlayer = Player
  { _name     = ""
  , _hand     = []
  , _palette  = []
  , _strategy = (\g -> return DoNothing)
  }

card_comp :: Card -> Card -> Ordering
card_comp (Card {_color = c1, _num = n1})
          (Card {_color = c2, _num = n2}) =
            cmp_test_fst (n1,c1) (n2,c2)

-- Compare with comparison of a with a' taking
-- precedence over b with b'.
cmp_test_fst (a,b) (a',b') =
  if a == a'
    then b `compare` b'
    else a `compare` a'

instance Ord Card where
  compare = card_comp

instance Eq Card where
  (Card n1 c1) == (Card n2 c2) = (cmp_test_fst (n1, c1) (n2, c2) == EQ)

data Game = Game
  { _deck    :: [Card]
  , _canvas  :: [Card]
  , _players :: [Player]
  , _gen     :: StdGen
  , _shuffle :: [Card] -> [Card]
  , _mode    :: GameMode
  }

p1 = head . _players

defGame' = Game
  { _shuffle = shuffleCards (mkStdGen 0)
  , _gen     = mkStdGen 1
  , _players = []
  , _deck    = shuffleCards (mkStdGen 2) cards
  , _canvas  = []
  , _mode    = Quiet
  }

defGame gen = defGame'
  { _shuffle = shuffleCards gen
  , _gen     = gen
  , _deck    = shuffleCards gen cards
  }

instance Show Game where
  show (Game { _deck = d , _players = ps, _canvas = cvs, _mode = m }) =
       "  canvas  = " ++ show cvs ++ "\n"
    ++ "  deck    = " ++ show d   ++ "\n"
    ++ "  players = " ++ show ps  ++ "\n"
    ++ "  mode    = " ++ show m   ++ "\n"

type GameState   = StateT Game IO

type PlayerState = StateT Player IO

data GameMode =
    Quiet -- Don't print anything when playing
  | StdIO -- Print actions / events to stdout
  deriving (Eq, Ord, Show)

makeLenses ''Card
makeLenses ''Game
makeLenses ''Player


