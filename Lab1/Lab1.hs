module Lab1 where
import System.Environment
import Test.QuickCheck
----------------Part 1----------------

power :: Integer -> Integer -> Integer
power n k | k < 0 = error "power: negative argument"
power n 0 = 1
power n k = n * power n (k-1)

--How many computing steps are being used?
--Answer: k steps + base case -> (k+1) steps.

----------------Part 2----------------

power1 :: Integer -> Integer -> Integer
power1 n k | k < 0 = error "power: negative argument"
power1 n k = product([ n | x <- [1..k] ])
 
----------------Part 3----------------

power2 :: Integer -> Integer -> Integer
power2 n 0 = 1
power2 n k | k < 0 = error "power: negative argument"
power2 n k | even k = power2 (n*n) (div k 2) 
power2 n k | odd k = n * power2 n (k-1)


----------------Part 4----------------

--A)
--Test cases: n = [-2..2] and k =[0..5]. 
--These cases cover both negative and positive values of n,
--and reassures that all functions
--give the same sign for several orders of k.


--B)
--Checks that values of functions power and power1
--are the same as well as power and power2 =>
--all power functions give the same value.

prop_powers n k = (power n k == power1 n k) == (power n k == power2 n k)

--C)
--Tests prop_powers with a list comprehension
--of different values, and checks if all 
--resulting values are true.
powers' = and[prop_powers x y | x <-[-2..2], y <-[0..5]]

--D)

--Same as prop_powers except uses abs value of k.
prop_powers' n k = let k' = abs k in
				(power n k' == power1 n k') == (power n k' == power2 n k')

