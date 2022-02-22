module ExprParser (Expr(..), parseExpr) where
import           Control.Applicative
import           Control.Monad
import           Data.Char
import qualified Parser                        as P
import           Parser                  hiding ( Parser )

type Parser a = P.Parser Char a

data Expr = ExNum Double
          | ExVar Char
          | ExSum [Expr]
          | ExProduct Expr Expr
          | ExQuotient Expr Expr
          deriving (Eq, Show, Read)

w :: Parser ()
w = void $ while isSpace

int :: Parser Expr
int = ExNum . read <$> while1 (\s -> (isDigit s) || (s=='.'))

var :: Parser Expr
var = ExVar <$> satisfy isAlpha

parseExpr :: String -> Maybe Expr
parseExpr = (fmap fst) . (runParser (expr <* eoi))

expr :: Parser Expr
expr = chainl1 term ((:) <$ char '+')

term :: Parser Expr
term = quotient <|> (chainl1 factor productP)
  where
    productP = ExProduct  <$ (w *> char '*' <* w)
    quotient = ExQuotient <$> factor <*> (w *> char '/' *> w *> factor)

factor :: Parser Expr
factor =
  int <|> var <|> (char '(' *> w *> expr <* w <* char ')')