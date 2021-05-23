
-- suits: clubs, diamonds, hearts, spades
-- colors: black, red
-- ranks: 1,2,3,4,5,6,7,8,9,10,11,12

-- Creating the suit Type
data Suit = Clubs | Diamonds | Hearts | Spades

-- Creating a function that returns the score for each
-- suit type

suitScore :: Suit -> Integer
suitScore Clubs = 0
suitScore Diamonds = 1
suitScore Hearts = 2
suitScore Spades = 3

instance Show Suit where
  show Clubs = "-8o"
  show Diamonds = "<>"
  show Hearts = "<3"
  show Spades = "-8>"

instance Eq Suit where 
  x == y = suitScore x == suitScore y
  x /= y = suitScore x /= suitScore y

instance Ord Suit where
  x < y = suitScore x < suitScore y
  x <= y = suitScore x <= suitScore y
  x > y = suitScore x > suitScore y
  x >= y = suitScore x >= suitScore y
  max x y
    | x > y = x
    | otherwise = y
  min x y
    | x < y = x
    | otherwise = y

-- Creating color type
data Color = Black | Red deriving(Eq)

colorScore :: Color -> Integer
colorScore Red = 0
colorScore Black = 1

instance Ord Color where
  x < y = colorScore x < colorScore y
  x <= y = colorScore x <= colorScore y
  x > y = colorScore x > colorScore y
  x >= y = colorScore x >= colorScore y
  max x y
    | x > y = x
    | otherwise = y
  min x y
    | x < y = x
    | otherwise = y

instance Show Color where
  show Black = "black"
  show Red = "red"

-- Card type
--
data Card = Card Suit Color Integer

instance Eq Card where 
  x == y = (r1 == r2) && (s1 == s2) && (c1 == c2) 
    where (Card s1 c1 r1) = x
          (Card s2 c2 r2) = y

mostImportantRank :: Card -> Card -> (Integer, Integer) 
mostImportantRank (Card s1 c1 r1) (Card s2 c2 r2) = if r1 /= r2 then (r1, r2) else
                                                    if s1 /= s2 then ((suitScore s1), (suitScore s2)) else
                                                    ((colorScore c1), (colorScore c2))
                                                                  
instance Ord Card where 
  x > y = a > b where
    (a, b) = mostImportantRank x y
  x >= y = a >= b where 
    (a, b) = mostImportantRank x y
  x < y = a < b where
    (a, b) = mostImportantRank x y
  x <= y = a <= b where 
    (a, b) = mostImportantRank x y
  max x y
    | x > y = x
    | otherwise = y
  min x y
    | x < y = x
    | otherwise = y


instance Show Card where
  show (Card suit color rank) = concat [show suit, show color, show rank]


