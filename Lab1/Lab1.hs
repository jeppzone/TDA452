module Lab1 where
import System.Environment

----------------Part 1----------------

power :: Integer -> Integer -> Integer
power n k | k < 0 = error "power: negative argument"
power n 0 = 1
power n k = n * power n (k-1)

--How many computing steps are being used?
--Answer: k steps + base case -> (k+1) steps.

----------------Part 2----------------

power1 :: Integer -> Integer -> Integer
power1 n k = product([ n | x <- [1..k] ])

--Version 2:
--power1 :: Integer -> Integer -> Integer
--power1 n k = product(replicate(fromInteger k) n)
 
----------------Part 3----------------

power2 :: Integer -> Integer -> Integer
power2 n 0 = 1
power2 n k | k < 0 = error "power: negative argument"
power2 n k | even k = power2 (n*n) (div k 2) 
power2 n k | odd k = n * power2 n (k-1)


----------------Part 4----------------