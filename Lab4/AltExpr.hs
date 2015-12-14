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

data Op = Add | Sub | Mul | Div 
  deriving Eq

data Fun = Sin | Cos
  deriving Eq


instance Show Expr where
    show = showExpr

showExpr :: Expr -> String
showExpr (Num n)       = show n
showExpr (Bop o e1 e2) = showExpr e1 ++ showOp o ++ showExpr e2
showExpr (Uop f e)     = showFun f ++ showExpr e
showExpr (Var)         = "x"
--showExpr (UOP f e) = showFun ++ showExpr e

showOp :: Op -> String
showOp Add = " + "
showOp Sub = " - "
showOp Mul = " * "
showOp Div = " / "

showFun :: Fun -> String
showFun Sin = "sin "
showFun Cos = "cos "

eval :: Expr -> Double -> Double
eval (Num n) _ = n
eval (Bop Add e1 e2) x = (eval e1 x) + (eval e2 x)
eval (Bop Sub e1 e2) x = (eval e1 x) - (eval e2 x)
eval (Bop Mul e1 e2) x = (eval e1 x) * (eval e2 x)
eval (Bop Div e1 e2) x = (eval e1 x) / (eval e2 x)
eval (Uop Sin e1) x    = sin (eval e1 x)
eval (Uop Cos e1) x    = cos (eval e1 x)
eval (Var) x           = x

readExpr :: String -> Maybe Expr
readExpr = undefined

simplify :: Expr -> Expr
simplify (Bop Add (Num n1) (Num n2)) = Num (n1 + n2)
simplify exp@(Bop Add (Num n) e)
       | n == 0    = simplify e
       | otherwise = exp
simplify exp@(Bop Add e (Num n))
       | n == 0    = simplify e
       | otherwise = exp
simplify (Bop Mul (Num n1) (Num n2)) = Num (n1*n2)
simplify exp@(Bop Mul (Num n) e)
       | n == 0    = Num 0
       | n == 1    = simplify e
       | otherwise = exp
simplify exp@(Bop Mul e (Num n))
       | n == 0    = Num 0
simplify e = e


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