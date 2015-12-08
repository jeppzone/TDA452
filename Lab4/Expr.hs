module Expr where

data Expr = Num Float
        | Mul Expr Expr
        | Add Expr Expr
        | Sin Expr
        | Cos Expr
        | Var String
        deriving Eq

instance Show Expr where
    show = showExpr


showExpr :: Expr -> String
showExpr (Num n)        = show n
showExpr (Mul e1 e2)    = show e1 ++ " * " ++ show e2
showExpr (Add e1 e2)    = show e1 ++ " + " ++ show e2
showExpr (Sin e)        = "Sin " ++ "(" ++ show e ++ ")"
showExpr (Cos e)        = "Cos " ++ "(" ++ show e ++ ")"
showExpr (Var e)        = e

-- Cant be compiled yet. Needs to be converted to doubles

-- eval :: Expr -> Double -> Double
-- eval (Num n)        = n
-- eval (Mul e1 e2)    = (eval e1) * eval e2
-- eval (Add e1 e2)    = eval e1 + eval e2
-- eval (Sin e)        = eval sin e
-- eval (Cos e)        = eval cos e
-- eval (Var e)        = e
