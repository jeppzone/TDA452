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
blanks s = [pos | pos <- allPositions, isNothing (positionValue s pos)]
  where 
    r = rows s

-- Function that returns a list of all possible positions in a 9x9 sudoku
-- Used in both blanks and nonBlanks functions
allPositions :: [Pos]
allPositions = [(x, y) | x <- [0..8], y <- [0..8]]

-- Function that returns the value of the given position in the given sudoku
positionValue :: Sudoku -> Pos -> Maybe Int
positionValue s (x, y) = r !! x !! y
  where
    r = rows s

-- Property to check that all positions returned actually are blank
prop_blanks :: Sudoku -> Bool
prop_blanks s = all (\p -> isNothing $ positionValue s p) blanks'
  where
    blanks' = blanks s
    r = rows s

-- Operator that updates an element in a list given an index and a new value
(!!=) :: [a] -> (Int, a) -> [a]
(!!=) l (i, x) | i < (length l) = (take (i) l) ++ [x] ++ (drop (i+1) l)
               | otherwise      = error ("!!= : index out of bounds")

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
update s (row, col) v = Sudoku (r !!= (row, r !! row !!= (col, v)))
  where
    r = rows s

-- Property to check that the updated value is correct
prop_update :: Sudoku -> Pos -> Maybe Int -> Bool
prop_update s (row, col) v = v == positionValue newSud p'
  where
    newSud = update s p' v
    p' = (abs(row `mod` 9), abs(col `mod` 9))

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
prop_candidates s = isSudoku s && isOkay s ==> 
                    all(\sud -> isSudoku sud && isOkay sud) allSuds
  where
    allSuds  = concat [allSudsOneBlank b | b <- blanks s]
    allSudsOneBlank aBlank = [update s aBlank (Just c) | c <- candidates s aBlank]
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
solveOne s p (c:cs) | isNothing s''  = solveOne s p cs
                    | otherwise      = s''
  where 
    s'  = update s p (Just c)
    s'' = solve s'
solveOne _ _ _                       = Nothing

-- Function that reads a sudoku from a file and tries to solve it
readAndSolve :: FilePath -> IO()
readAndSolve fp = do
                    s <- readSudoku fp
                    let solved = solve s
                    if isNothing solved
                      then error "No solution found"
                      else printSudoku (fromJust solved)

-- Function that genereates all non blank cells in a sudoku
nonBlanks :: Sudoku -> [(Pos, Maybe Int)]
nonBlanks s = [(pos, val pos) | pos <- allPositions, isJust (val pos)]
  where
    r = rows s
    val pos = positionValue s pos

-- Function that given a position and a value, checks if
-- the value in that cell is equal to the given value
hasValueCell :: Sudoku -> (Pos, Maybe Int) -> Bool
hasValueCell s (p, v)  = cellValue == v
  where
    rs = rows s
    cellValue = positionValue s p

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
fewerChecks prop = quickCheckWith stdArgs{ maxSuccess = 5 } prop