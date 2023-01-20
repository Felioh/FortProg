data Rank = Numeric Int | Jack | Queen | King | Ace
  deriving Show

data Suit = Clubs | Spades | Hearts | Diamonds
  deriving Show

data Card = Card Suit Rank
  deriving Show

-- Compute the value of a card regarding the rules of Black Jack
getCardValue :: Card -> Int
getCardValue (Card _ r) = value r
  where
  value (Numeric v) = v
  value Jack        = 10
  value Queen       = 10
  value King        = 10
  value Ace         = 11

-- A hand of cards is either empty or it is a card added to a hand of cards
data Hand = Empty
          | Add Card Hand
  deriving Show

-- Combine two hands of cards
(<+>) :: Hand -> Hand -> Hand
Empty      <+> h2 = h2
(Add c h1) <+> h2 = Add c (h1 <+> h2)


fullDeck :: Hand
fullDeck = verteiler [(Numeric 2) , (Numeric 3) , (Numeric 4) , (Numeric 5) , (Numeric 6) , (Numeric 7) , (Numeric 8) , (Numeric 9) , (Numeric 10) , Jack , Queen , King , Ace]
        where verteiler []     = Empty
              verteiler (x:xs) = constructor x <+> verteiler xs 
            --constructor:: Rank -> Hand 
                where constructor rank = Add (Card Clubs rank) (Add (Card Spades rank) (Add (Card Hearts rank) (Add (Card Diamonds rank) Empty)))
 
numOfAces :: Hand -> Int
numOfAces Empty                        = 0
numOfAces (Add (Card _ Ace) restHand)  = 1 + numOfAces restHand
numOfAces (Add card restHand)          =  numOfAces restHand

getValue :: Hand -> Int
getValue hand  | calcValue hand > 21 = calcValue hand - (numOfAces hand * 10)
               | otherwise           = calcValue hand
        where calcValue Empty               = 0
              calcValue (Add card restHand) = getCardValue card + calcValue restHand
