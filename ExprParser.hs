module ExprParser
  ( Expr(..)
  , parseExpr
  ) where
import           Control.Applicative
import           Control.Monad
import           Data.Char
import           Expressions
import qualified Parser                        as P
import           Parser                  hiding ( Parser )

type Parser a = P.Parser Char a

w :: Parser ()
w = void $ while isSpace

int :: Parser Expr
int = ExNum . read <$> while1 (\s -> isDigit s || s == '.')

var :: Parser Expr
var = ExVar <$> satisfy isAlpha

parseExpr :: String -> Maybe Expr
parseExpr = fmap fst . runParser (expr <* eoi)

expr :: Parser Expr
expr = chainl1 term (addP <|> subP)
 where
  addP = ExAdd <$ (w *> char '+' <* w)
  subP = ExSub <$ (w *> char '-' <* w)

term :: Parser Expr
term = frac <|> chainl1 factor multP
 where
  multP = ExMult <$ (w *> char '*' <* w)
  frac  = ExFrac <$> factor <*> (w *> char '/' *> w *> factor)

factor :: Parser Expr
factor = int <|> var <|> char '(' *> w *> expr <* w <* char ')'
