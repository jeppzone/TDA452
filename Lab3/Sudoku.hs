module Sudoku where

import Test.QuickCheck
import Data.Maybe
import Data.Char
import Data.List

------------------------------------------ Part A ------------------------------------
data Sudoku = Sudoku { rows :: [[Maybe Int]] }
 deriving ( Show, Eq )

-- allBlankSudoku is a sudoku with just blanks
allBlankSudoku :: Sudoku
allBlankSudoku = Sudoku (replicate 9 (replicate 9 Nothing))

-- isSudoku sud checks if sud is really a valid representation of a sudoku
-- puzzle
isSudoku :: Sudoku -> Bool
isSudoku s = length (rows s) == 9 && all correctRow (rows s)

-- Checks if a single row is a valid soduko row
correctRow :: [Maybe Int] -> Bool
correctRow = all (\c -> case c of
                        Nothing -> True
                        Just c -> c `elem` [1..9]) 

-- isSolved sud checks if sud is already solved, i.e. there are no blanks
isSolved :: Sudoku -> Bool
isSolved s = isSudoku s && all isRowSolved (rows s)
  where 
    isRowSolved = all isJust

------------------------------------------ Part B ------------------------------------

-- printSudoku sud prints a representation of the sudoku sud on the screen
printSudoku :: Sudoku -> IO ()
printSudoku s = putStr $ unlines $ map printRow $ rows s

printRow :: [Maybe Int] -> String
printRow = foldr ((++) . printCell) ""

printCell :: Maybe Int -> String
printCell Nothing = "."
printCell (Just c) = show c

-- readSudoku file reads from the file, and either delivers it, or stops
-- if the file did not contain a sudoku
readSudoku :: FilePath -> IO Sudoku
readSudoku fp = do
                  file <- readFile fp
                  let sudoku = parseSudoku file
                  if isSudoku sudoku
                    then return sudoku
                  else error "Not a valid sudoku in file"

-- Helper function for parsing a sudoku                     
parseSudoku :: String -> Sudoku
parseSudoku s = Sudoku (map parseRow (lines s))

-- Helper function for parsing a row
parseRow :: String -> [Maybe Int]
parseRow = map parseCell

-- Helper function for parsing a cell
parseCell :: Char -> Maybe Int
parseCell c | c == '.' = Nothing
            | otherwise = Just (ord c - ord '0')

------------------------------------------ Part C ------------------------------------

-- cell generates an arbitrary cell in a Sudoku
cell :: Gen (Maybe Int)
cell = frequency [(9, return Nothing), (2, do n <- choose(1,9); return (Just n))]

-- an instance for generating Arbitrary Sudokus
instance Arbitrary Sudoku where
  arbitrary =
    do rows <- sequence [ sequence [ cell | j <- [1..9] ] | i <- [1..9] ]
       return (Sudoku rows)

-- Property to check if a sudoku is valid
prop_Sudoku :: Sudoku -> Bool
prop_Sudoku = isSudoku

------------------------------------------ Part D ------------------------------------

type Block = [Maybe Int]

-- Checks if the given block is valid according to sudoku rules
isOkayBlock :: Block -> Bool
isOkayBlock b = length onlyFilled == length (nub onlyFilled)
  where onlyFilled = filter isJust b

-- Returns the rows, columns and blocks given a sudoku
blocks :: Sudoku -> [Block]
blocks s = rows' ++ columns' ++ blocks'
  where rows' = rows s
        columns' = transpose rows'
        blocks' = rowHelper rows'

-- Helper function for traversing the rows three at a time
rowHelper :: [[Maybe Int]] -> [[Maybe Int]]
rowHelper [] = []
rowHelper r = colHelper(take 3 r) ++ rowHelper (drop 3 r)

-- Helper function that returns a list of blocks when given three rows
colHelper :: [[Maybe Int]] -> [[Maybe Int]]
colHelper (a:b:c:ds) 
  | null a = []
  | otherwise = (take 3 a ++ take 3 b ++ take 3 c) :
                colHelper (drop 3 a : drop 3 b : drop 3 c : ds)

-- Checks if all blocks in a sudoku are valid
isOkay:: Sudoku -> Bool
isOkay s = all isOkayBlock (blocks s)

-- Property to validate the amount of blocks and their respective sizes
prop_blocks :: Sudoku -> Bool
prop_blocks s = length blocks' == 27 && all (\b -> length b == 9) blocks'
  where blocks' = blocks s
------------------------------------------ Part E ------------------------------------

-- Type describing a position in the sudoku matrix
type Pos = (Int, Int)

-- Function for returning all blank positions in the sudoku
blanks :: Sudoku -> [Pos]
blanks s = concat [blankFinder (r !! index) index | index <- [0..8]]
  where r = rows s

-- Helper function that finds all blank positions in a sudoku
blankFinder :: [Maybe Int] -> Int -> [Pos]
blankFinder row index = zip rows indexes
  where 
    indexes = elemIndices Nothing row
    rows = replicate 9 index

-- Property to check that all positions returned actually are blank
prop_blanks :: Sudoku -> Bool
prop_blanks s = all (\e -> isNothing (r !! (fst e) !! snd e)) blanks'
  where
    blanks' = blanks s
    r = rows s

-- Operator that updates an element in a list given an index and a new value
(!!=) :: [a] -> (Int, a) -> [a]
(!!=) l (index, v) | (length l - 1) == index = fst (splitAt index l) ++ [v]
                   | otherwise = (fst divided) ++ [v] ++ (tail $ snd divided)
  where divided = splitAt index l

-- Property to check that the !!= operator behaves in a correct way,
-- replacing the correct indexed element, not changing the length of the list
-- and replacing with the correct element at the correct index  
prop_replace :: Eq a => [a] -> (Int, a) -> Property
prop_replace l (index, v) = length l > 0 &&
                            index >= 0 && index < length l ==>
                            length l == length l1 && 
                            index `elem` elemIndices v l1&& 
                            l1 !! index == v
  where
    l1 = l !!= (index, v)


-- Function that given a sudoku, a position and a new value updates the sudoku accordingly
update :: Sudoku -> Pos -> Maybe Int -> Sudoku
update s p v = Sudoku (r !!= (rowIndex, r !! rowIndex !!= (colIndex, v)))
  where
    r = rows s
    rowIndex = fst p 
    colIndex = snd p

-- Property to check that the updated sudoku actually is a valid sudoku
prop_update :: Sudoku -> Pos -> Maybe Int -> Bool
prop_update s p v = v == (r !! rowIndex !! colIndex)
  where
    r = rows (update s p' v)
    p' = (abs((fst p) `mod` 9), abs((snd p) `mod` 9))
    rowIndex = fst p'
    colIndex = snd p'

-- Function that given a sudoku and a blank position,
-- returns all the possible values that can be put in that position
candidates :: Sudoku -> Pos -> [Int]
candidates s (r, c) = [1..9] \\ [fromJust r | r <- (filter isJust list)]
  where
    blos = blocks s
    rowCol = blos !! r ++ blos !! (9 + c)
    block = blos !! (17 + candidateHelper (r `div` 3, c `div` 3))
    list = rowCol ++ block

-- Helper function for finding a certain block
candidateHelper :: Pos -> Int
candidateHelper (r, c) = (r * 3) + (c + 1)

-- Property for checking that the inserting the candidates doesn't 
-- make the sudoku invalid
prop_candidates :: Sudoku -> Property
prop_candidates s = isSudoku s && isOkay s ==> isSudoku newSud && isOkay newSud
  where 
    newSud = update s aBlank aCandidate 
    aCandidate = Just (head $ candidates s aBlank)
    aBlank = head $ blanks s

------------------------------------------ Part F ------------------------------------

-- Function that given a Sudoku, tries to solve it.
-- If no solution is found, or the sudoku is invalid the result is Nothing
solve :: Sudoku -> Maybe Sudoku
solve s | not (isSudoku s) || not (isOkay s) = Nothing
        | otherwise = solve' s blas
  where
    blas = blanks s

-- Helper function for solve
solve' :: Sudoku -> [Pos] -> Maybe Sudoku
solve' s [] = Just s
solve' s (p:ps) = solveOne s p cs
  where 
    cs = candidates s p

-- Helper function for solving one particular cell
solveOne :: Sudoku -> Pos -> [Int] -> Maybe Sudoku
solveOne s p cs | length cs == 0           = Nothing
                | isOkay s' && isSolved s' = Just s'
                | isNothing s''            = solveOne s p (tail cs)
                | otherwise                = s''
  where 
    s'  = update s p (Just (head cs))
    s'' = solve s'

-- Function that reads a sudoku from a file and tries to solve it
readAndSolve :: FilePath -> IO()
readAndSolve fp = do
                    s <- readSudoku fp
                    let solved = solve s
                    if isNothing solved
                      then error "No solution found"
                      else printSudoku (fromJust solved)

-- Function that returns all positions and the value of that position
-- in a sudoku that are not blank
nonBlanks :: Sudoku -> [(Pos, Maybe Int)]
nonBlanks s = concat [nonBlankHelper (r !! i) i | i <- [0..8]]
  where r = rows s

-- Helper function for finding all non blank positions and values
nonBlankHelper :: [Maybe Int] -> Int -> [(Pos, Maybe Int)]
nonBlankHelper row index = zip (zip rows indexes) c
  where
    indexes = findIndices (isJust) row
    rows = replicate 9 index
    c = [row !! index | index <- indexes]

-- Function that given a position and a value, checks if
-- the value in that cell is equal to the given value
hasValueCell :: Sudoku -> (Pos, Maybe Int) -> Bool
hasValueCell s ((r, c) , v ) = cellValue == v
  where
    rs = rows s
    cellValue = (rs !! r) !! c

-- Function that given two sudokus checks if the first sudoku is valid and solved,
-- and checks if the first solution for the first is also a solution for the second.   
isSolutionOf :: Sudoku -> Sudoku -> Bool
isSolutionOf s u = isOkay s && isSolved s 
                 && (all (hasValueCell s) (nonBlanks u)) 

-- Property that checks that every supposed solution produced by 
-- the solve function is actually a valid solution of the original problem
prop_SolveSound :: Sudoku -> Property
prop_SolveSound s = isJust solved ==>
                    (fromJust solved) `isSolutionOf`  s
  where solved = solve s

-- Making quickCheck accept after maxSucess number of successful tests
fewerChecks prop = quickCheckWith stdArgs{ maxSuccess = 10 } prop