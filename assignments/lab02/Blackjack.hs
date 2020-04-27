module BlackJack where

import Cards
import RunGame
import Test.QuickCheck

-- | Function that returns an empty hand
empty :: Hand 
empty = Empty

-- | Given a Rank, return its value
valueRank :: Rank -> Integer
valueRank Ace = 11
valueRank _ = 10

-- | Given a Card, return its value
valueCard :: Card -> Integer
valueCard (Card (Numeric num) _) = num
valueCard (Card r _)             = valueRank r

-- | Given a blackjack Hand, return its value
valueHelper :: Hand -> Integer
valueHelper Empty = 0
valueHelper (Add card h) = valueCard card + value h

-- | Given a blackjack Hand, return its value
-- counting the problem of the Ace
value :: Hand -> Integer
value hand | valueHelper hand > 21 = valueHelper hand - (10 * numberOfAces hand)
           | otherwise           = valueHelper hand

-- | function  that returns the number of Aces in a hand
numberOfAces :: Hand -> Integer
numberOfAces Empty                = 0
numberOfAces (Add (Card Ace _) h) = 1 + numberOfAces h
numberOfAces (Add _ h)            = numberOfAces h 

gameOver :: Hand -> Bool
gameOver hand = value hand > 21

-- | given the Guest's and Bank's hands, returns the winner
winner :: Hand -> Hand -> Player
winner hGuest hBank | vGuest == vBank                = Bank
                    | vBank > 21 && vGuest > 21      = Bank
                    | vBank > vGuest && vBank < 21   = Bank
                    | vGuest > vBank && vGuest < 21  = Guest
                    where
                        vBank = value hBank
                        vGuest = value hGuest

-- | Given two hands, it puts the first one on top of the second one
(<+) :: Hand -> Hand -> Hand
(<+) Empty h2            = h2
(<+) h1 Empty            = h1 --not really needed, but might increase efficiency 
(<+) (Add card h1) h2    = (Add card (h1 <+ h2)) 

-- | Associativity property of (<+) function
prop_onTopOf_assoc :: Hand -> Hand -> Hand -> Bool
prop_onTopOf_assoc p1 p2 p3 =
    p1<+(p2<+p3) == (p1<+p2)<+p3

-- | Size property
prop_size_onTopOf :: Hand -> Hand -> Bool
prop_size_onTopOf h1 h2 = size h1 + size h2 == size (h1 <+ h2)

-- | given a Rank, generate a hand with all the cards of that rank
generateHand :: Suit -> Hand
generateHand s = ( Add (Card (Numeric 2) s) (Add (Card (Numeric 3) s) (Add (Card (Numeric 4) s) (Add (Card (Numeric 5) s) (Add (Card (Numeric 6) s) (Add (Card (Numeric 7) s) (Add (Card (Numeric 8) s) (Add (Card (Numeric 9) s) (Add (Card (Numeric 10) s) (Add (Card Jack s) (Add (Card Queen s) (Add (Card King s) (Add (Card Ace s) Empty)))))))))))))

-- | Return a full BlackJack deck
fullDeck :: Hand
fullDeck = generateHand Hearts <+ generateHand Spades <+ generateHand Diamonds <+ generateHand Clubs

-- | Given a deck and a hand, draw one card from the deck, place it in the hand and return (Deck, Hand)
draw :: Hand -> Hand -> (Hand, Hand)
draw Empty _ = error "draw: The deck is empty"
draw (Add card h) hand = (h, (Add card hand))

