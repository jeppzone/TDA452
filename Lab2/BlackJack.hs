module BlackJack where
import Cards
import Wrapper


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
valueRank (Numeric n) 		   = n
valueRank r | r == Ace 		   = 11
			      | otherwise 	   = 10

-- Returns the value of the card

valueCard :: Card -> Integer
valueCard c = valueRank (rank c)

-- Returns the number of aces in a hand

numberOfAces :: Hand -> Integer
numberOfAces Empty = 0
numberOfAces (Add (Card r s) hand) | r == Ace  = 1 + numberOfAces hand
                                   | otherwise = numberOfAces hand

-- Returns the value of a hand using blackjack rules.

value :: Hand -> Integer
value (Add c h) | value2 (Add c h) <= 21 = valueCard c + value h
                | otherwise = value2 (Add c h) - (numberOfAces h)*10
                  where
                       value2 Empty = 0
                       value2 (Add c h) = valueCard c + value2 h

-- Returns true if the game is over, false otherwise

gameOver :: Hand -> Bool
gameOver hand = value hand > 21

-- Returns the winner of the round

winner :: Hand -> Hand -> Player
winner ph bh | (gameOver ph) = Bank
             | (gameOver bh) = Guest
             | value ph > value bh = Guest
             | value ph <= value bh = Bank