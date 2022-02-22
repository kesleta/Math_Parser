module Evaluator (eval) where
import ExprParser
import Control.Monad

eval :: Expr -> Maybe Double
eval (ExNum x)          = Just x
eval (ExSum e1 e2)      = (+) <$> eval e1 <*> eval e2
eval (ExDif e1 e2)      = (-) <$> eval e1 <*> eval e2
eval (ExProduct e1 e2)  = (*) <$> eval e1 <*> eval e2
eval (ExQuotient e1 e2) = (/) <$> eval e1 <*> eval e2



