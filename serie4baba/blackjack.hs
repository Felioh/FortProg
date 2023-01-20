
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
fullDeck = calc (Numeric 2) <+> calc (Numeric 3) <+> calc (Numeric 4) <+> calc (Numeric 5) <+> calc (Numeric 6) <+> calc (Numeric 7) <+> calc (Numeric 8)<+> calc (Numeric 9)<+> calc (Numeric 10)<+> calc (Jack)<+> calc (Queen)<+> calc (King)<+> calc (Ace)
            where calc numbers = Add (Card Clubs (numbers)) (Add (Card Clubs (numbers)) (Add (Card Clubs (numbers)) (Add(Card Clubs (numbers)) Empty)))

numOfAces :: Hand -> Int
numOfAces Empty                    = 0
numOfAces (Add (Card _ Ace) hando) = 1 + numOfAces hando
numOfAces (Add _ hando)            = numOfAces hando

-- mit akkulumator
getValue :: Hand -> Int
getValue ganzohando = getRealValue ganzohando  0 
        where getRealValue (Add karto hando) sum =  getRealValue hando (sum + getCardValue karto) 
              getRealValue Empty             sum = if sum > 21 then sum - (numOfAces ganzohando *10) 
                                                               else sum


{- 
handpimml :: Hand 
handpimml = Add (Card Hearts King)  -}


handpimml = Add (Card Clubs (Ace)) (Add (Card Clubs (Numeric 5)) (Add (Card Clubs (Numeric 10)) (Add(Card Clubs (Ace)) Empty)))
