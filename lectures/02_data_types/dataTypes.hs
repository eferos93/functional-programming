-- | Modelling a Playing Cards


-- | Every card has a suit:  ♠ ♥ ♦ ♣
data Suit = Spades | Hearts | Diamonds | Clubs
            deriving (Eq,Show)

data Colour = Black | Red deriving Show

-- | Each suit has a colour – red or black
colour :: Suit -> Colour
colour Spades   = Black
colour Clubs    = Black
colour _        = Red


-- | Cards have ranks: 2, 3 .. 10, Jack, Queen, King, Ace
data Rank = N2 | N3 | N4 | N5 | N6 | N7 | N8 | N9 | N10 | 
            Jack | Queen | King | Ace
            deriving (Show,Eq,Ord,Enum)
            
--all_ranks

-- | When does one rank beat another rank?
rankBeats :: Rank -> Rank -> Bool
rankBeats r1 r2 = r1 > r2


-- | Alternatives to the  Rank type?

--data Rank' = N2 | N3 ... | King | Ace
--all_ranks'

--------------------------------------------------------------------------------

--type Card = (Rank,Suit)

-- | A card has a rank and a suit
--data Card = Card Rank Suit deriving Show


-- | With field names
data Card = Card { rank::Rank, suit::Suit } deriving Show

--rank :: Card -> Rank
--rank (Card r s) = r

--suit :: Card -> Suit
--suit (Card r s) = s

example_card_1 = Card Ace Spades
example_card_2 = Card { rank=King, suit=Hearts }

-- | A card beats another card when it has the same suit and it beats the rank
-- of the other card
cardBeats :: Card -> Card -> Bool
cardBeats (Card r1 s1) (Card r2 s2) = s1==s2 && rankBeats r1 r2

-- | Alternative definition (using selector functions)
cardBeats' card1 card2 = suit card1 == suit card2
                         && rankBeats (rank card1) (rank card2)


--------------------------------------------------------------------------------

--type Hand = [Card]

-- | A hand contains zero or more cards
data Hand = Empty | Add Card Hand
            deriving Show

example_hand_0 = Empty

example_hand_1 = Add example_card_1 Empty

example_hand_2 = Add example_card_2 example_hand_1


-- | A empty cand beats nothing. A non-empty hand can beat a card if the first
-- card can, or if the rest of the hand can
handBeats :: Hand -> Card -> Bool
handBeats Empty           beat = False
handBeats (Add card hand) beat = cardBeats card beat || handBeats hand beat


-- | Given a card to beat and a hand, choose a card from the hand that can
-- beat the card to beat, if possible.
-- Choose the lowest card that beats the card to beat
-- If you can follow suit, choose the lowest card of the same suit
-- Otherwise, choose the lowest card

chooseCard :: Card -> Hand -> Card
chooseCard beat hand
    | handBeats hand beat = lowestCard (betterCards hand beat)
    | haveSuit hand (suit beat) = lowestCard (sameSuit hand (suit beat))
    | otherwise = lowestCard hand


-- | Check if a hand contains a card with the a suit
haveSuit :: Hand -> Suit -> Bool
haveSuit Empty     s = False
haveSuit (Add c h) s = suit c == s || haveSuit h s

-- | Find the lowest card in a hand
lowestCard :: Hand -> Card
--lowestCard Empty =
lowestCard (Add c Empty) = c
lowestCard (Add c h) | rank c<rank low = c
                     | otherwise       = low
    where
      low = lowestCard h
      

-- | Find the cards in a hand that can beat a given card
betterCards :: Hand -> Card -> Hand
betterCards Empty beat = Empty
betterCards (Add c h) beat | cardBeats c beat = Add c (betterCards h beat)
                           | otherwise        = betterCards h beat


-- | Find the cards in a hand of a given suit
sameSuit :: Hand -> Suit -> Hand
sameSuit Empty     s             = Empty
sameSuit (Add c h) s | suit c==s = Add c (sameSuit h s)
                     | otherwise = sameSuit h s
