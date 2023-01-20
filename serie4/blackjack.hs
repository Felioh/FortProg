import SimplePrelude
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

-- test variable der hand
-- : [Card Jack Spades]

testHand = Hand [Card Two Diamonds, Card Three Clubs, Card Ace Clubs, Card Ace Diamonds]
testHand1 = Hand [Card Five Diamonds, Card Six Clubs]

-- Französisch
-- "Kreuzprodukt" aus den Symbolen und den Zahlen ergibt ein Kartenset.
fullDeck :: Hand
fullDeck = Hand (generateCards Diamonds ++ 
                 generateCards Clubs    ++ 
                 generateCards Hearts   ++ 
                 generateCards Spades)
                 --Lokale Definition um die Karten zu generieren
    where generateCards suit = [Card Two   suit,
                                Card Three suit, 
                                Card Four  suit, 
                                Card Five  suit, 
                                Card Six   suit, 
                                Card Seven suit, 
                                Card Eight suit, 
                                Card Nine  suit, 
                                Card Ten   suit, 
                                Card Jack  suit, 
                                Card Queen suit, 
                                Card King  suit, 
                                Card Ace   suit]
                                
-- Liefert die Anzahl der Asse in einer Hand zurück
numOfAces :: Hand -> Int -- er rammt die hand in den arsc... int
numOfAces (Hand cards) = count cards 0
    where count []                         n = n                        -- List ist leer, wir geben n zurück.
          count ((Card Ace _) : restCards) n = count restCards (n+1)    -- Fall 1: karte ist ein Ass.
          count (          _  : restCards) n = count restCards n        -- Fall 2: kein ass -> Rekursiver Aufruf 


getValue :: Hand -> Int
getValue (Hand cards)  | value cards 0 <= 21 = value cards 0
                       | otherwise           = value cards 0 - (numOfAces (Hand cards) * 10)       
             
    where value []                 n = n
          value (card : restCards) n = value restCards (n + getCardValue card)