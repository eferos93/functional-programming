module BlackJack where
    import Cards
    import RunGame

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
-- TODO consider case in which Ace value =1
value :: Hand -> Integer
value Empty = 0
value (Add card h) = valueCard card + value h