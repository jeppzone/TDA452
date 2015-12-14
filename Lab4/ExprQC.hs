import Test.QuickCheck
import Data.Maybe
import AltExpr

prop_ŚhowReadExpr :: Expr -> Bool
prop_ŚhowReadExpr e = closeToEqual e (fromJust(readExpr (showExpr e)))
  where closeToEqual e1 e2 = abs ((eval' e1) - (eval' e2)) < 0.01
        eval' e = eval e 1.37

arbExpr :: Int -> Gen Expr
arbExpr n = undefined

op :: Gen Op
op = elements [Mul, Add]

fun :: Gen Fun
fun = elements [Sin, Cos]

instance Arbitrary Expr where
  arbitrary = sized arbExpr