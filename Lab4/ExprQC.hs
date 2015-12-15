import Test.QuickCheck
import Data.Maybe
import AltExpr

-- Property to check that showing and reading expression does not 
-- change the value of the initial expression
prop_ShowReadExpr :: Expr -> Bool
prop_ShowReadExpr e = closeToEqual e (fromJust(readExpr (showExpr e)))

-- Property to check that simplify does not change the value
-- of the initial expression 
prop_simplify :: Expr -> Bool
prop_simplify e = closeToEqual e (simplify e)

-- Helper function that checks that two expressions are close
-- to being equal since there seem to be a rounding problem 
-- with big ńumbers
closeToEqual :: Expr -> Expr -> Bool
closeToEqual e1 e2 = abs ((eval' e1) - (eval' e2)) < 0.0001
  where eval' e = eval e 1.37

-- Function that generates a random expression
arbExpr :: Int -> Gen Expr
arbExpr n | n == 1 = oneof [(rNumber 0 100), variable]
          | otherwise = oneof [(rNumber 0 100), variable, bop, uop]
  where
    bop = do
         o <- op
         e1 <- arbExpr(n-1)
         e2 <- arbExpr (n-1)
         return (Bop o e1 e2)
    uop = do
          f <- fun
          e <- arbExpr (n-1)
          return (Uop f e)

-- Helper function that generates a random Num given a min
-- and a max value  
rNumber :: Double -> Double -> Gen Expr
rNumber min max = do
                  nbr <- choose (min, max)
                  return (Num nbr)

-- Function that generates a random Variable
variable :: Gen Expr
variable = do
           return Var

-- Function that generates a random Binary operator
op :: Gen Op
op = elements [Mul, Add]

-- Function that generates a random Unary operator
fun :: Gen Fun
fun = elements [Sin, Cos]

instance Arbitrary Expr where
  arbitrary = sized arbExpr
