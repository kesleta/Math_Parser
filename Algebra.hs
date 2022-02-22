module Algebra where
import ExprParser

distribute :: Expr -> Expr
distribute (ExProduct a (ExSum x y)) = ExSum (ExProduct a x) (ExProduct a y)
distribute a = a