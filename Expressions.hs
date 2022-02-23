module Expressions where
data Expr = ExNum Double
          | ExVar Char
          | ExAdd Expr Expr
          | ExSub Expr Expr
          | ExMult Expr Expr
          | ExFrac Expr Expr
          deriving (Eq, Read)


instance Show Expr where
  show (ExNum x   ) = show x
  show (ExVar c   ) = [c]
  show (ExAdd  a b) = show a ++ " + " ++ show b
  show (ExSub  a b) = show a ++ " - " ++ show b
  show (ExMult a b) = "(" ++ show a ++ ") * (" ++ show b ++ ")"
  show (ExFrac a b) = "(" ++ show a ++ ") / (" ++ show b ++ ")"

descend :: (Expr -> Expr) -> Expr -> Expr
descend f (ExAdd  a b) = f (ExAdd (descend f a) (descend f b))
descend f (ExSub  a b) = f (ExSub (descend f a) (descend f b))
descend f (ExMult a b) = f (ExMult (descend f a) (descend f b))
descend f (ExFrac a b) = f (ExFrac (descend f a) (descend f b))
descend f (ExNum a   ) = f (ExNum a)
descend f (ExVar a   ) = f (ExVar a)
