import Test.QuickCheck
import Data.Maybe
import AltExpr

prop_ShowReadExpr :: Expr -> Bool
prop_ShowReadExpr e = closeToEqual e (fromJust(readExpr (showExpr e)))
  where closeToEqual e1 e2 = abs ((eval' e1) - (eval' e2)) < 0.0001
        eval' e = eval e 1.37

arbExpr :: Int -> Gen Expr
arbExpr n | n == 1 = oneOf [number, variable]
  
rNumber :: Int -> Int ->Gen Num
rNumber min max = do
                  nbr <- choose (min, max)
                  return Num nbr



op :: Gen Op
op = elements [Mul, Add]

fun :: Gen Fun
fun = elements [Sin, Cos]

instance Arbitrary Expr where
  arbitrary = sized arbExpr