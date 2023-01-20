
data Rank = Numeric Int | Jack | Queen | King | Ace
  deriving (Show)

data Suit = Clubs | Spades | Hearts | Diamonds
  deriving (Show)

data Card = Card Suit Rank
  deriving (Show)

type Hand = [Card]


fullDeck :: Hand
fullDeck = (getRank Clubs) ++ (getRank Spades) ++ (getRank Diamonds) ++ (getRank Hearts)
  where getRank :: Suit -> [Card]
        getRank suite = [Card suite Queen] ++ 
                        [Card suite King]  ++ 
                        [Card suite Jack]  ++ 
                        [Card suite Ace]   ++ generateNums 2
                        where generateNums n = if n <= 10 then [Card suite (Numeric n)] ++ generateNums (n+1) else []
                    


testHand :: [Card]
testHand = [Card Spades Ace,Card Clubs Jack,Card Hearts Jack,Card Spades Jack]


numOfAces :: Hand -> Int
numOfAces []     = 0
numOfAces (x:xs) = if isAce x then numOfAces xs + 1 else numOfAces xs
  where isAce :: Card -> Bool
        isAce (Card _ Ace) = True 
        isAce _            = False




getValue :: Hand -> Int
getValue [] = 0
getValue (x:xs) = if valueCard x + getValue xs <= 21 then valueCard x + getValue xs else valueCard x + getValue xs - (numOfAces (x:xs) * 10) 
  where valueCard :: Card -> Int
        valueCard (Card _ (Numeric i)) = i
        valueCard (Card _ Queen)       = 10
        valueCard (Card _ King)       = 10
        valueCard (Card _ Jack)       = 10
        valueCard (Card _ Ace)       = 11

