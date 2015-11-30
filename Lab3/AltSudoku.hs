module Sudoku where
import Test.QuickCheck
import Data.List
import Data.Char


data Sudoku = Sudoku { rows :: [[Maybe Int]] }
    deriving(Eq,Show)

example :: Sudoku
example =
        Sudoku
          [ [Just 3, Just 6, Nothing,Nothing,Just 7, Just 1, Just 2, Nothing,Nothing]
          , [Nothing,Just 5, Nothing,Nothing,Nothing,Nothing,Just 1, Just 8, Nothing]
          , [Nothing,Nothing,Just 9, Just 2, Nothing,Just 4, Just 7, Nothing,Nothing]
          , [Nothing,Nothing,Nothing,Nothing,Just 1, Just 3, Nothing,Just 2, Just 8]
          , [Just 4, Nothing,Nothing,Just 5, Nothing,Just 2, Nothing,Nothing,Just 9]
          , [Just 2, Just 7, Nothing,Just 4, Just 6, Nothing,Nothing,Nothing,Nothing]
          , [Nothing,Nothing,Just 5, Just 3, Nothing,Just 8, Just 9, Nothing,Nothing]
          , [Nothing,Just 8, Just 3, Nothing,Nothing,Nothing,Nothing,Just 6, Nothing]
          , [Nothing,Nothing,Just 7, Just 6, Just 9, Nothing,Nothing,Just 4, Just 3]
          ]
example2 :: Sudoku
example2 =
         Sudoku
         [[Just 1, Just 2, Just 3, Just 4, Just 5, Just 6, Just 7, Just 8, Just 9]
         ,[Just 1, Just 2, Just 3, Just 4, Just 5, Just 6, Just 7, Just 8, Just 9]
         ,[Just 1, Just 2, Just 3, Just 4, Just 5, Just 6, Just 7, Just 8, Just 9]
         ,[Just 1, Just 2, Just 3, Just 4, Just 5, Just 6, Just 7, Just 8, Just 9]
         ,[Just 1, Just 2, Just 3, Just 4, Just 5, Just 6, Just 7, Just 8, Just 9]
         ,[Just 1, Just 2, Just 3, Just 4, Just 5, Just 6, Just 7, Just 8, Just 9]
         ,[Just 1, Just 2, Just 3, Just 4, Just 5, Just 6, Just 7, Just 8, Just 9]
         ,[Just 1, Just 2, Just 3, Just 4, Just 5, Just 6, Just 7, Just 8, Just 9]
         ,[Just 1, Just 2, Just 3, Just 4, Just 5, Just 6, Just 7, Just 8, Just 9]
         ]

exampleRow :: [Maybe Int]
exampleRow = [Just 3, Just 6, Nothing,Nothing,Just 7, Just 1, Just 2, Nothing,Nothing]

example3 :: [String]
example3 = ["36..712..",".5....18.","..92.47..","....13.28","4..5.2..9" ,"27.46....","..53.89..",".83....6."]


exampleString :: String
exampleString = "36..712"


allBlankSudoku :: Sudoku
allBlankSudoku = Sudoku(replicate 9 (replicate 9 Nothing))


-- isSudoku sud checks if sud is really a valid representation of a sudoku
-- puzzle
isSudoku :: Sudoku -> Bool
isSudoku s = and [length x == 9 | x <- (rows s)]
    && length (rows s) == 9

-- isSolved sud checks if sud is already solved, i.e. there are no blanks
isSolved :: Sudoku -> Bool
isSolved sudo = isSudoku sudo && not(any fullElement (rows sudo))
    where fullElement elem = any(==Nothing) elem
-------------------------------------------------------------------------

-- printSudoku sud prints a representation of the sudoku sud on the screen
printSudoku :: Sudoku -> IO ()
printSudoku sudo = do
    putStr (unlines [convertRow x | x <- (rows sudo)])
        where
            convertRow row = [convertToChar y | y <- row]

convertToChar :: (Maybe Int) -> Char
convertToChar (Just n) = chr(ord '0' + n)
convertToChar Nothing = chr(ord '0' - 2)

convertToInt :: Char -> (Maybe Int)
convertToInt c | ((ord c) - (ord '0') == (-2)) = Nothing
               | otherwise                     = Just ((ord c)- (ord '0'))

stringToInt :: String -> [Maybe Int]
stringToInt str = [convertToInt x | x <- str]

stringsToInts :: [String] -> [[Maybe Int]]
stringsToInts str = [stringToInt x | x <- str]


-- readSudoku file reads from the file, and either delivers it, or stops
-- if the file did not contain a sudoku
readSudoku :: FilePath -> IO Sudoku
readSudoku fp = do
    sudo     <- readFile fp
    if (isSudoku (Sudoku (stringsToInts(lines sudo))))
        then return (Sudoku $ stringsToInts(lines sudo))
        else error "Program error: Not a sudoku."


-------------------------------------------------------------------------

-- cell generates an arbitrary cell in a Sudoku
cell :: Gen (Maybe Int)
cell = frequency[(1,mInt), (9,do return Nothing)]
    where
        mInt = do
            n <- choose(1,9)
            return (Just n)

-- an instance for generating Arbitrary Sudokus
instance Arbitrary Sudoku where
    arbitrary =
        do rows <- sequence [sequence [cell | j <- [1..9]] | i <- [1..9]]
           return (Sudoku rows)


prop_Sudoku :: Sudoku -> Bool
prop_Sudoku sudo = isSudoku sudo

-------------------------------------------------------------------------
type Block = [Maybe Int]

isOkayBlock :: Block -> Bool
isOkayBlock block = ((countNothing block) + (length $ nub $ block) == 10)

countNothing :: [Maybe Int] -> Int
countNothing arr = count [x == Nothing | x <- arr]
    where count list = sum $ map fromEnum list

blocks :: Sudoku -> [Block]
blocks sudo = do
    blocks <- [x |x <- (rows sudo)] ++ [y |y <- (transpose(rows sudo))]
--  ++ (makeBlocks sudo) -- Detta ska vara listan med blocks av 3x3
    return blocks




isOkay :: Sudoku -> Bool
isOkay sudo = all isOkayBlock (blocks sudo)

--Test
