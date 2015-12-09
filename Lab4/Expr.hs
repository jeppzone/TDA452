module Expr where

data Expr = Num Double
        | Mul Expr Expr
        | Add Expr Expr
        | Sin Expr
        | Cos Expr
        | Var String
        deriving Eq

-- type X = String

instance Show Expr where
    show = showExpr


showExpr :: Expr -> String
showExpr (Num n)        = show n
showExpr (Mul e1 e2)    = show e1 ++ " * " ++ show e2
showExpr (Add e1 e2)    = show e1 ++ " + " ++ show e2
showExpr (Sin e)        = "Sin " ++ "(" ++ show e ++ ")"
showExpr (Cos e)        = "Cos " ++ "(" ++ show e ++ ")"
showExpr (Var e)        = "x"



eval :: Expr -> Double -> Double
eval (Num n)       d    = n
eval (Mul e1 e2)   d    = eval e1 d * eval e2 d
eval (Add e1 e2)   d    = eval e1 d + eval e2 d
eval (Sin e)       d    = sin (eval e d)
eval (Cos e)       d    = cos (eval e d)
eval (Var e)       d    = d
