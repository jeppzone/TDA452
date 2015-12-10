module Expr where

import Parsing

data Expr = Num Double
          | BOP Op Expr Expr
          | UOP Fun Expr
          | Var String
        deriving Eq

data Op = Add | Sub | Mul | Div 
  deriving Eq

data Fun = Sin | Cos
  deriving Eq


instance Show Expr where
    show = showExpr


showExpr :: Expr -> String
showExpr (Num n)     = show n
showExpr (BOP o e1 e2) = showExpr e1 ++ showOp o ++ showExpr e2 
--showExpr (UOP f e) = showFun ++ showExpr e

showOp :: Op -> String
showOp o | o == Add = " + "
         | o == Sub = " - "
         | o == Mul = " * "
         | o == Div = " / "


eval :: Expr -> Double -> Double
eval (Num n) _ = n
eval (BOP Add e1 e2) x = (eval e1 x) + (eval e2 x)
eval (BOP Sub e1 e2) x = (eval e1 x) - (eval e2 x)
eval (BOP Mul e1 e2) x = (eval e1 x) * (eval e2 x)
eval (BOP Div e1 e2) x = (eval e1 x) / (eval e2 x)
eval (UOP Sin e1) x    = sin (eval e1 x)
eval (UOP Cos e1) x    = cos (eval e1 x)
eval (Var s) x         = x