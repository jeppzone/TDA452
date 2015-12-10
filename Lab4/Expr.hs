module Expr where

import Parsing

data Expr = Num Double
        | Bop Op Expr Expr
        | Uop Fun Expr
        | Var String
        deriving Eq

instance Show Expr where
    show = showExpr

data Op = Mul | Add
    deriving Eq

data Fun = Sin | Cos
    deriving Eq


showOp :: Op -> String
showOp  o  | o == Mul = "*"
           | o == Add = "+"

showFun :: Fun -> String
showFun  f | f == Sin = "Sin("
           | f == Cos = "Cos("


showExpr :: Expr -> String
showExpr (Num n)            = show n
showExpr (Bop o e1 e2)      = show e1 ++ showOp o ++ show e2
showExpr (Uop f e)          = showFun f ++ show e ++ ")"
showExpr (Var e)            = "x"



eval :: Expr -> Double -> Double
eval (Num n)             _    = n
eval (Bop Mul e1 e2)     d    = eval e1 d * eval e2 d
eval (Bop Add e1 e2)     d    = eval e1 d + eval e2 d
eval (Uop Sin e)         d    = sin (eval e d)
eval (Uop Cos e)         d    = cos (eval e d)
eval (Var e)             d    = d

readExpr :: String -> Maybe Expr
readExpr = undefined
