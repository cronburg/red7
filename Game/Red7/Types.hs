module Game.Red7.Types where

data Card = Card
  { color :: Color
  , num   :: Int
  } deriving (Show)

data Color = VIOLET | INDIGO | BLUE
  | GREEN | YELLOW | ORANGE | RED
  deriving (Eq, Ord, Show, Enum, Bounded)

-- Invalid Card by default:
defCard = Card {color = VIOLET, num = 0}
newCard (c,n) = defCard {color = c, num = n}

colors  = [(minBound :: Color) ..]
numbers = [1 .. length colors]

-- All 49 cards in the deck:
cards = map newCard $
  foldl (++) [] $
    map (uncurry zip) $
      zip (map (take 7 . repeat) colors)
          (map (take 7 . repeat) numbers)

data Player = Player
  { name    :: String
  , hand    :: [Card]
  , palette :: [Card]
  } deriving (Show)

defPlayer = Player { name = "", hand = [], palette = [] }

card_comp :: Card -> Card -> Ordering
card_comp (Card {color = c1, num = n1})
          (Card {color = c2, num = n2}) =
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
  c1 == c2 = num c1 == num c2

