module Math where
import           Control.Applicative
import           Control.Monad
import           Data.Char
import qualified Parser                        as P
import           Parser                  hiding ( Parser )

type Parser a = P.Parser Char a

data Expr = ExInt Int
          | ExVar Char
          | ExSum Expr Expr
          | ExDif Expr Expr
          | ExProduct Expr Expr
          | ExFraction Expr Expr
          deriving (Eq, Show, Read)

w :: Parser ()
w = void $ while isSpace

parseExpr :: Parser Expr
parseExpr = parseTerm <|> parseSum <|> parseDif

parseTerm :: Parser Expr
parseTerm = parseFactor <|> parseProduct <|> parseFraction

parseFactor :: Parser Expr
parseFactor =
  parseInt <|> parseVar <|> (char '(' *> w *> parseExpr <* w <* char ')')

parseInt :: Parser Expr
parseInt = ExInt . read <$> while1 isDigit

parseVar :: Parser Expr
parseVar = ExVar <$> satisfy isAlpha

parseSum :: Parser Expr
parseSum = ExSum <$> parseExpr <*> (w *> char '+' *> w *> parseExpr)

parseDif :: Parser Expr
parseDif = ExDif <$> parseExpr <*> (w *> char '-' *> w *> parseExpr)

parseProduct :: Parser Expr
parseProduct = ExProduct <$> parseExpr <*> (w *> char '*' *> w *> parseExpr)

parseFraction :: Parser Expr
parseFraction = ExFraction <$> parseExpr <*> (w *> char '/' *> w *> parseExpr)
