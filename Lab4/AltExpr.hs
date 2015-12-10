module Expr where

import Parsing

data Expr = Num Double
          | Bop Op Expr Expr
          | Uop Fun Expr
          | Var String
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
showExpr (Var x)       = x
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
eval (Var s) x         = x

readExpr :: String -> Expr
readExpr = undefined