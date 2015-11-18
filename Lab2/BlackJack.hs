module BlackJack where
import Cards
import Wrapper
import System.Random
import Test.QuickCheck


-----------Task 2-----------
{-
	size hand2 = size (Add (Card (Numeric 2) Hearts)
	                  (Add (Card Jack Spades) Empty))
  = 1 + size (Add (Card Jack Spades) Empty)
  = 1 + (1 + size(Empty))
  = 1 + 1 + 0
  = 2
-}

-----------Task 3-----------

-- Returns an empty hand

empty :: Hand
empty = Empty

-- Returns the value of the rank

valueRank :: Rank -> Integer
valueRank (Numeric n) = n
valueRank Ace         = 11
valueRank _ 	        = 10

-- Returns the value of the card

valueCard :: Card -> Integer
valueCard c = valueRank (rank c)
-- Returns the number of aces in a hand

numberOfAces :: Hand -> Integer
numberOfAces Empty = 0
numberOfAces (Add (Card Ace _) hand) = 1 + numberOfAces hand
numberOfAces (Add _ hand) = numberOfAces hand

-- Returns the value of a hand using blackjack rules.

value :: Hand -> Integer
value h | value2 h <= 21 = value2 h
        | otherwise = value2 h - (numberOfAces h)*10
  where
    value2 Empty      = 0
    value2 (Add c h)  = valueCard c + value2 h

-- Returns true if the game is over, false otherwise

gameOver :: Hand -> Bool
gameOver hand = value hand > 21

-- Returns the winner of the round

winner :: Hand -> Hand -> Player
winner ph bh | (gameOver ph) = Bank
             | (gameOver bh) = Guest
             | value ph > value bh = Guest
             | value ph <= value bh = Bank

-- Operator to add a hand on top of another hand

(<+) :: Hand -> Hand -> Hand
Empty <+ hand2 = hand2
(Add card hand) <+ hand2 = (Add card (hand <+ hand2))

-- Property to check if the <+ operator is associative

prop_onTopOf_assoc :: Hand -> Hand -> Hand -> Bool
prop_onTopOf_assoc h1 h2 h3 = h1 <+(h2 <+ h3) == (h1 <+ h2) <+ h3

-- Property to check if the size is preserved after using the <+ operator

prop_size_onTopOf :: Hand -> Hand -> Bool
prop_size_onTopOf h1 h2 = (size h1) + (size h2) == size (h1 <+ h2)

-- Returns a hand consisting of all cards of the given suit

fullSuit :: Suit -> Hand
fullSuit s = (Add (Card Ace s) (Add (Card King s) (Add (Card Queen s) 
             (Add (Card Jack s)(Add (Card (Numeric 10) s) (Add (Card (Numeric 9) s) 
             (Add (Card (Numeric 8 ) s)(Add (Card (Numeric 7) s) 
             (Add (Card (Numeric 6 ) s) (Add (Card (Numeric 5) s) 
             (Add (Card (Numeric 4 ) s) (Add (Card (Numeric 3) s) 
             (Add (Card (Numeric 2 ) s) Empty )))))))))))))

-- Returns a full deck consisting of all 52 cards

fullDeck :: Hand
fullDeck = (fullSuit Hearts) <+ (fullSuit Diamonds) <+ 
           (fullSuit Clubs) <+ (fullSuit Spades)

-- Draws a card from the deck, returns error if deck is empty

draw :: Hand -> Hand -> (Hand, Hand)
draw Empty _ = error ("draw: The deck is empty")
draw (Add card deck) hand = (deck, (Add card hand))

-- Plays a bank hand

playBank :: Hand -> Hand
playBank deck = playBank' deck Empty

-- Helper function for playing a bank hand

playBank' :: Hand -> Hand -> Hand
playBank' deck hand | value hand >= 16 = hand
                    | otherwise        = uncurry playBank' newCard
  where newCard = draw deck hand

-- Shuffles a hand of cards randomly

shuffle :: StdGen -> Hand -> Hand
shuffle _ Empty = Empty
shuffle g deck  = Add removed (shuffle g' partial)
  where
    deckSize           = size deck
    (n, g')            = randomR (1, deckSize) g
    (partial, removed) = removeCard deck n Empty

-- Removes a card from a hand at given position n and moves it to the other hand.
-- Returns a tuple of the changed changed hand and the removed card

removeCard :: Hand -> Integer -> Hand -> (Hand, Card)
removeCard (Add card h1) n h2
   | n == 1      = (h1 <+ h2, card)
   | otherwise   = removeCard h1 (n-1) (Add card h2)

-- Checks if a given card belongs to hand

belongsTo :: Card -> Hand -> Bool
c `belongsTo` Empty = False
c `belongsTo` (Add c' h) = c == c' || c `belongsTo` h

-- Property to check if a shuffled hand contains the same cards
-- as it dit before

prop_shuffle_sameCards :: StdGen -> Card -> Hand -> Bool
prop_shuffle_sameCards g c h = c `belongsTo` h == c `belongsTo` shuffle g h

-- Property to check wether the size of a shuffled hand is preserved

prop_size_shuffle :: StdGen -> Hand -> Bool
prop_size_shuffle g h = (size h) == size (shuffle g h)

-- Tells the program which functions implements the defined interfaces

implementation = Interface{ 
  iEmpty = empty
, iFullDeck = fullDeck
, iValue = value
, iGameOver = gameOver
, iWinner = winner
, iDraw = draw
, iPlayBank = playBank
, iShuffle = shuffle
}

-- Main function that runs the program

main :: IO()
main = runGame implementation

