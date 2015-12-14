module AltExpr where

import Parsing
import Test.QuickCheck
import Data.Char
import Data.Maybe
import Control.Monad

data Expr = Num Double
          | Bop Op Expr Expr
          | Uop Fun Expr
          | Var
  deriving Eq

data Op = Add | Mul 
  deriving Eq

data Fun = Sin | Cos
  deriving Eq


instance Show Expr where
  show = showExpr

--Converts an Expression into a String
showExpr :: Expr -> String
showExpr (Num n) = show n
showExpr (Bop Add a b) = showExpr a ++ "+" ++ showExpr b
showExpr (Bop Mul a b) = showFactor a ++ "*" ++ showFactor b
showExpr (Var) = "x"
showExpr (Uop Sin z) = "sin " ++ showRads z
showExpr (Uop Cos z) = "cos " ++ showRads z

showFactor :: Expr -> String
showFactor (Bop Add a b) = "(" ++ showExpr (Bop Add a b) ++ ")"
showFactor e = showExpr e

showRads :: Expr -> String
showRads (Bop Add a b) = "(" ++ showExpr (Bop Add a b) ++ ")"
showRads (Bop Mul a b) = "(" ++ showExpr (Bop Mul a b) ++ ")"
showRads e = showExpr e

eval :: Expr -> Double -> Double
eval (Num n) _ = n
eval (Bop Add e1 e2) x = (eval e1 x) + (eval e2 x)
eval (Bop Mul e1 e2) x = (eval e1 x) * (eval e2 x)
eval (Uop Sin e1) x    = sin (eval e1 x)
eval (Uop Cos e1) x    = cos (eval e1 x)
eval (Var) x           = x

readExpr :: String -> Maybe Expr
readExpr s = do
         e <- parse parseAdd (removeSpaces s)
         return $ fst e

parseAdd :: Parser Expr
parseAdd = foldr1(Bop Add) `fmap` chain parseMul (parseOperator '+') 

parseMul :: Parser Expr
parseMul = foldr1 (Bop Mul) `fmap` chain parseFactor (parseOperator '*')

parseOperator :: Char -> Parser String
parseOperator c = do
              string[c]

parseFactor :: Parser Expr
parseFactor = parseParenthesis +++
         parseFunction Sin "sin" +++
         parseFunction Cos "cos" +++
         parseVariable +++
         parseNumber

parseParenthesis :: Parser Expr
parseParenthesis = do
                   char '('
                   e <- parseAdd
                   char ')'
                   return $ e
parseFunction :: Fun -> String -> Parser Expr
parseFunction f s = do
                    string s
                    e <- parseFactor
                    return $ Uop f e

parseVariable :: Parser Expr
parseVariable = do
                char 'x'
                return Var

parseNumber :: Parser Expr
parseNumber = do
              d <- readsP :: Parser Double
              return (Num d)

string :: String -> Parser String
string s = sequence $ fmap (char) s

removeSpaces :: String -> String
removeSpaces [] = ""
removeSpaces (' ':xs) = removeSpaces xs
removeSpaces (x:[]) = [x]
removeSpaces (x:xs) = x : removeSpaces xs

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
simplify' exp@(Bop Add (Var) e) = (Bop Add (Num 0) (simplify e))
simplify' exp@(Bop Add e (Var)) = (Bop Add (Num 0) (simplify e))
simplify' (Bop Mul (Num n1) (Num n2)) = Num (n1*n2)
simplify' exp@(Bop Mul (Num n) e)
       | n == 0    = Num 0
       | n == 1    = simplify e
       | otherwise = exp
simplify' exp@(Bop Mul e (Num n))
       | n == 0    = Num 0
       | n == 1    = simplify e
       | otherwise = exp
simplify' exp@(Bop Mul (Var) e) = (Bop Mul (Num 1) (simplify e))
simplify' exp@(Bop Mul e (Var)) = (Bop Mul (Num 1) (simplify e))
simplify' e = e


differentiate :: Expr -> Expr
differentiate (Num _) = Num 0.0
differentiate (Var)   = Num 1.0
differentiate (Bop Add e1 e2) = (Bop Add e1' e2')
  where e1' = differentiate e1
        e2' = differentiate e2
differentiate (Bop Mul e1 e2) = (Bop Add (Bop Mul e1' e2) (Bop Mul e1 e2'))
  where e1' = differentiate e1
        e2' = differentiate e2
differentiate (Uop Sin e) = (Bop Mul e' (Uop Cos e))
  where e' = differentiate e
differentiate (Uop Cos e) = (Bop Mul e' (Uop Cos (Bop Add e (Num pi))))
  where e' = differentiate e