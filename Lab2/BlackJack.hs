module BlackJack where
import Cards
import Wrapper

-----------Task 2-----------
{-
	size hand2 = size (Add (Card (Numeric 2) Hearts)
					        (Add (Card Jack Spades) Empty))
	= 1 + size (Add (Card Jack Spades) Empty)
	= 1 + 1 + size(Empty)
	= 1 + 1 + 0
	= 2
-}

-----------Task 3-----------

-- Returns an empty hand

empty :: Hand
empty = Empty;

-- Returns the value of the rank

valueRank :: Rank -> Integer
valueRank (Numeric number) 		= number
valueRank rank 	| rank == Jack 	= 10
				| rank == Queen = 10
				| rank == King 	= 10
				| rank == Ace 	= 11

-- Returns the value of the card

valueCard :: Card -> Integer
valueCard (Card rank suit) = valueRank rank

-- Returns the number of aces in a hand

numberOfAces :: Hand -> Integer
numberOfAces Empty = 0
numberOfAces (Add (Card rank suit) hand ) | rank == Ace = 1 + numberOfAces hand
										  | otherwise   =  numberOfAces hand

-- Helper functions for calculating the highest 
-- and lowest possible values of a hand

valueHigh :: Hand -> Integer
valueHigh Empty = 0
valueHigh (Add card hand) = valueCard card + valueHigh hand

valueLow :: Hand -> Integer
valueLow hand =  (valueHigh hand) - (numberOfAces hand) * 10 

-- Returns the real blackjack value of a hand

value :: Hand -> Integer
value Empty = 0
value (Add card hand) | valueHigh (Add card hand) > 21 = valueLow(Add card hand)
					  | otherwise					   = valueHigh(Add card hand)

-- Returns true if the game is over, false otherwise

gameOver :: Hand -> Bool
gameOver hand = (value hand) > 21

-- Returns the winner of the round

winner :: Hand -> Hand -> Player
winner guest bank | gameOver guest 						= Bank
				  | gameOver bank 						= Guest
				  | value bank >= value guest 			= Bank
				  | otherwise							= Guest