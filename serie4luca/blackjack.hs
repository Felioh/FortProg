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


data Suit = Clubs 
          | Spades 
          | Hearts 
          | Diamonds
  deriving Show

data Card = Card Rank Suit
  deriving Show

-- testCard = Card Three Hearts
testCard = Card Three Hearts

getCardValue :: Card -> Int
getCardValue (Card Two _) = 2
getCardValue (Card Three _) = 3
getCardValue (Card Four _)  = 4
getCardValue (Card Five _)  = 5
getCardValue (Card Six _)   = 6
getCardValue (Card Seven _)= 7
getCardValue (Card Eight _) = 8
getCardValue (Card Nine _) = 9
getCardValue (Card Ace _)   = 11
getCardValue (Card _ _) = 10




  -- Linked List Haskell Implementierung. Hand mit Liste von Karten oder leer.
data Hand = Hand [Card]
  deriving Show

-- Konkatenieren von 2 Händen.
(<+>) :: Hand -> Hand -> Hand  
Hand cards1 <+> Hand cards2 = Hand (cards1 ++ cards2)

testHand = Hand [Card Two Diamonds, Card Three Clubs, Card Ace Clubs, Card Ace Diamonds]
testHand1 = Hand [Card Five Diamonds, Card Six Clubs]

-- liefert ein Deck von 52 (französischen) Spielkarten zurück
fullDeck :: Hand
fullDeck = Hand (make Clubs ++ make Spades ++ make Hearts ++ make Diamonds)
            where make suit = [Card Two suit,
                               Card Three suit,
                               Card Four suit, 
                               Card Five suit,
                               Card Six suit,
                               Card Seven suit,
                               Card Eight suit,
                               Card Nine suit,
                               Card Ten suit,
                               Card Jack suit,
                               Card Queen suit,
                               Card King suit,
                               Card Ace suit]

-- liefert die Anzahl der Asse in einer Hand zurück
numOfAces :: Hand -> Int
numOfAces (Hand hand) = count hand 0
    where count [] n                = n
          count ((Card Ace _):hs) n = count hs (n+1)
          count (h:hs) n            = count hs n 

-- berechnet den Wert einer Hand von Spielkarten
getValue :: Hand -> Int
getValue (Hand cards) | calc cards 0 <= 21 = calc cards 0
                      | otherwise          = calc cards 0 - ((numOfAces (Hand cards)) * 10)
    where calc [] n     = n
          calc (c:cs) n = calc cs (n + (getCardValue c))