module Sudoku where

import Test.QuickCheck
import Data.Maybe
import Data.Char
import Data.List

-------------------------------------------------------------------------

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
correctRow r = all (\c -> case c of
                        Nothing -> True
                        Just c -> c `elem` [1..9]) r

-- isSolved sud checks if sud is already solved, i.e. there are no blanks
isSolved :: Sudoku -> Bool
isSolved s = isSudoku s && all isRowSolved (rows s)
  where 
    isRowSolved = all $ not . isNothing


-------------------------------------------------------------------------

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
                  let sudoku = parseSudoko file
                  if isSudoku sudoku
                    then return sudoku
                  else error "Not a valid sudoku in file"

-- Helper function for parsing a sudoku                     
parseSudoko :: String -> Sudoku
parseSudoko s = Sudoku (map parseRow (lines s))

-- Helper function for parsing a row
parseRow :: String -> [Maybe Int]
parseRow = map parseCell

-- Helper function for parsing a cell
parseCell :: Char -> Maybe Int
parseCell c | c == '.' = Nothing
            | otherwise = Just (ord c)

-------------------------------------------------------------------------

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

-------------------------------------------------------------------------

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
  | otherwise = ((take 3 a) ++ (take 3 b) ++ (take 3 c)) :
                colHelper ((drop 3 a) : (drop 3 b) : (drop 3 c) : ds)

-- Checks if all blocks in a sudoku are valid
isOkay:: Sudoku -> Bool
isOkay s = all isOkayBlock (blocks s)

-- Property to validate the amount of blocks and their respective sizes
prop_blocks :: Sudoku -> Bool
prop_blocks s = length blocks' == 27 && all (\b -> length b == 9) blocks'
  where blocks' = blocks s