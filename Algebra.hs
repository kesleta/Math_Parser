{-# LANGUAGE LambdaCase #-}
module Algebra where
import           Expressions

disNeg :: Expr -> Expr
disNeg = descend $ \case
  ExSub a b -> ExAdd a (ExMult (ExNum (-1)) b)
  a         -> a

distribute :: Expr -> Expr
distribute = descend $ \case
  ExMult a (ExAdd b c) -> ExAdd (ExMult a b) (ExMult a c)
  ExMult a (ExSub b c) -> ExSub (ExMult a b) (ExMult a c)
  a                    -> a


gcf :: Expr -> Expr
gcf = descend $ \case
  ExAdd (ExMult a c) (ExMult b d) | a == b -> ExMult a (ExAdd c d)
  ExSub (ExMult a c) (ExMult b d) | a == b -> ExMult a (ExSub c d)
  a -> a



arithmatic :: Expr -> Expr
arithmatic = descend $ \case
  ExAdd  (ExNum a) (ExNum b) -> ExNum (a + b)
  ExSub  (ExNum a) (ExNum b) -> ExNum (a - b)
  ExMult (ExNum a) (ExNum b) -> ExNum (a * b)
  ExFrac (ExNum a) (ExNum b) -> ExNum (a / b)
  a                          -> a





expand :: Expr -> Expr
expand = distribute . disNeg
