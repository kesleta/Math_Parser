module Evaluator
  ( eval
  ) where
import           Control.Monad
import           ExprParser

eval :: Expr -> Maybe Double
eval (ExNum x     ) = Just x
eval (ExVar _     ) = Nothing
eval (ExAdd  e1 e2) = (+) <$> eval e1 <*> eval e2
eval (ExSub  e1 e2) = (-) <$> eval e1 <*> eval e2
eval (ExMult e1 e2) = (*) <$> eval e1 <*> eval e2
eval (ExFrac e1 e2) = (/) <$> eval e1 <*> eval e2



