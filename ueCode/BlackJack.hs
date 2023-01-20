import SimplePrelude

-- data Color = Red | Blue | Yellow | Pink | Green | ...

-- data Bool = False | True

-- data Complex = Complex Float Float

-- data IntList = Nil | Cons Int IntList

-- list123 :: IntList
-- list123 = Cons 1 (Cons 2 (Cons 3 Nil))

-- append :: IntList -> IntList -> IntList
-- append Nil         ys = ys
-- append (Cons x xs) ys = Cons x (append xs ys)

data Rank = Two
          | Three
          | Four
          | Five
          | Six
          | Seven
          | Eight
          | Nine
          | Ten
          | Jack
          | Queen
          | King
          | Ace
  deriving Show

{-
data Rank = Number Int | Jack | Queen | King | Ace
  deriving Show
-}

data Suit = Clubs | Spades | Hearts | Diamonds
  deriving Show

-- data Card = TwoClubs | TwoSpades | TwoHearts | ...
--  deriving Show
-- data Card = Card Rank Suit
--  deriving Show
data Card = Card (Rank, Suit)
  deriving Show

-- testCard = Card Three Hearts
testCard = Card (Three, Hearts)

getRankValue :: Rank -> Int
getRankValue Two   = 2
getRankValue Three = 3
getRankValue Four  = 4
getRankValue Five  = 5
getRankValue Six   = 6
getRankValue Seven = 7
getRankValue Eight = 8
getRankValue Nine  = 9
getRankValue Ten   = 10
getRankValue Jack  = 10
getRankValue Queen = 10
getRankValue King  = 10
getRankValue Ace   = 11

getCardValue :: Card -> Int
getCardValue (Card (rank, suit)) = getRankValue rank
-- getCardValue (Card rank suit) = getRankValue rank

{-
getRankValue :: Rank -> Int
getRankValue (Number i) = i
getRankValue Jack       = 10
getRankValue Queen      = 10
getRankValue King       = 10
getRankValue Ace        = 11
-}

--data Hand = EmptyHand | NonEmptyHand Card Hand
data Hand = Nil | Hand Card Hand
  deriving Show
  
data Hand2 = Hand2 [Card]

(<+>) :: Hand -> Hand -> Hand
Nil               <+> hand = hand
(Hand c restHand) <+> hand = restHand <+> Hand c hand
                          -- Hand c (restHand <+> hand)

testHand = Hand testCard (Hand (Card (Two, Diamonds)) (Hand (Card (Jack, Spades)) Nil))

(<++>) :: Hand2 -> Hand2 -> Hand2
(Hand2 list1) <++> (Hand2 list2) = Hand2 (list1 ++ list2)

