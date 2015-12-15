module AltExpr where

import Parsing
import Data.Char
import Data.Maybe
import Control.Monad

-- Data type expression witch constructors for Number, Binary Operator
-- Unary operator and Variable
data Expr = Num Double
          | Bop Op Expr Expr
          | Uop Fun Expr
          | Var
  deriving Eq

-- Data type for operator with constructors for Addition and Multiplication
data Op = Add | Mul 
  deriving Eq

-- Data type for function with constructors for sine and cosine
data Fun = Sin | Cos
  deriving Eq


instance Show Expr where
  show = showExpr

-- Function for converting an expression into a nicer looking string
showExpr :: Expr -> String
showExpr (Num n) = show n
showExpr (Bop Add a b) = showExpr a ++ "+" ++ showExpr b
showExpr (Bop Mul a b) = showFactor a ++ "*" ++ showFactor b
showExpr (Var) = "x"
showExpr (Uop Sin z) = "sin " ++ showRads z
showExpr (Uop Cos z) = "cos " ++ showRads z

-- Helper function for showing a factor of a multiplication with correct 
-- paranthesis
showFactor :: Expr -> String
showFactor (Bop Add a b) = "(" ++ showExpr (Bop Add a b) ++ ")"
showFactor e = showExpr e

-- Helper function for showing the expressions inside sine and cosine in a correct way
showRads :: Expr -> String
showRads (Bop Add a b) = "(" ++ showExpr (Bop Add a b) ++ ")"
showRads (Bop Mul a b) = "(" ++ showExpr (Bop Mul a b) ++ ")"
showRads e = showExpr e

-- Function for evaluating a given expression given the value of x
eval :: Expr -> Double -> Double
eval (Num n) _ = n
eval (Bop Add e1 e2) x = (eval e1 x) + (eval e2 x)
eval (Bop Mul e1 e2) x = (eval e1 x) * (eval e2 x)
eval (Uop Sin e1) x    = sin (eval e1 x)
eval (Uop Cos e1) x    = cos (eval e1 x)
eval (Var) x           = x

-- Function for reading and parsing a given string, resulting in a Maybe expression
readExpr :: String -> Maybe Expr
readExpr s = do
         e <- parse parseExpr (removeSpaces s)
         return $ fst e

-- Function that parses adding expressions
parseExpr :: Parser Expr
parseExpr = foldr1(Bop Add) `fmap` chain parseTerm (parseOperator '+') 

-- Function that parses multiplication expressions
parseTerm :: Parser Expr
parseTerm = foldr1 (Bop Mul) `fmap` chain parseFactor (parseOperator '*')

-- Function that parses operators
parseOperator :: Char -> Parser String
parseOperator c = do
                  string[c]

-- Function that parses a factor with possible 
-- components of it
parseFactor :: Parser Expr
parseFactor = parseParenthesis +++
              parseFunction Sin "sin" +++
              parseFunction Cos "cos" +++
              parseVariable +++
              parseNumber

-- Function that parses expressions covered in paranthesis
parseParenthesis :: Parser Expr
parseParenthesis = do
                   char '('
                   e <- parseExpr
                   char ')'
                   return $ e
-- Function that parses a function and returns in with
-- the unary operator constructor
parseFunction :: Fun -> String -> Parser Expr
parseFunction f s = do
                    string s
                    e <- parseFactor
                    return $ Uop f e

-- Function that parses a variable and returns it
-- with the Var 
parseVariable :: Parser Expr
parseVariable = do
                char 'x'
                return Var

-- Function that parses a number and returns it
-- with the Num constructor
parseNumber :: Parser Expr
parseNumber = do
              d <- readsP :: Parser Double
              return (Num d)

-- Function that given a string, returns a parser for that string
string :: String -> Parser String
string s = sequence $ fmap (char) s

-- Helper function that given a string, removes all it's spaces
-- and returns the resulting string
removeSpaces :: String -> String
removeSpaces [] = ""
removeSpaces (' ':xs) = removeSpaces xs
removeSpaces (x:[]) = [x]
removeSpaces (x:xs) = x : removeSpaces xs

-- Function that simplifies any given expression to the maximum extent.
-- At the moment, it does not simplify an expression containing a variable
simplify :: Expr -> Expr
simplify (Bop o e1 e2) = simplify' (Bop o (simplify e1) (simplify e2))
simplify (Uop f e) = simplify' (Uop f (simplify e))
simplify e = e

simplify' :: Expr -> Expr
simplify' (Bop Add (Num n1) (Num n2)) = Num (n1 + n2)
simplify' exp@(Bop Add (Num n) e)
       | n == 0    = simplify e
       | otherwise = exp
simplify' exp@(Bop Add e (Num n))
       | n == 0    = simplify e
       | otherwise = exp
simplify' (Bop Mul (Num n1) (Num n2)) = Num (n1*n2)
simplify' exp@(Bop Mul (Num n) e)
       | n == 0    = Num 0
       | n == 1    = simplify e
       | otherwise = exp
simplify' exp@(Bop Mul e (Num n))
       | n == 0    = Num 0
       | n == 1    = simplify e
       | otherwise = exp

simplify' e = e

-- Function that calculates the derivative of any given expression
-- and returns the resulting expression
differentiate :: Expr -> Expr
differentiate (Num _) = Num 0.0
differentiate (Var)   = Num 1.0
differentiate (Bop Add e1 e2) = simplify (Bop Add e1' e2')
  where e1' = differentiate e1
        e2' = differentiate e2
differentiate (Bop Mul e1 e2) = simplify (Bop Add (Bop Mul e1' e2) (Bop Mul e1 e2'))
  where e1' = differentiate e1
        e2' = differentiate e2
differentiate (Uop Sin e) = simplify (Bop Mul e' (Uop Cos e))
  where e' = differentiate e
differentiate (Uop Cos e) = simplify (Bop Mul e' (Bop Mul (Num (-1)) (Uop Cos e)))
  where e' = differentiate e