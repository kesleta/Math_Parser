{-# LANGUAGE LambdaCase #-}
module Parser where
import           Control.Applicative
import           Control.Monad.Trans.State

type Parser c a = StateT [c] Maybe a

runParser :: StateT s m a -> s -> m (a, s)
runParser = runStateT

(<#>) :: Parser c a -> [c] -> Maybe (a, [c])
(<#>) = runParser

parser :: (s -> m (a, s)) -> StateT s m a
parser = StateT

char :: Eq c => c -> Parser c c
char c = parser $ \case
  [] -> empty
  (x : xs) | c == x    -> return (x, xs)
           | otherwise -> empty

string :: Eq c => [c] -> Parser c [c]
string = mapM char

satisfy :: (c -> Bool) -> Parser c c
satisfy f = parser $ \case
  [] -> empty
  (x : xs) | f x       -> return (x, xs)
           | otherwise -> empty

while :: (c -> Bool) -> Parser c [c]
while = many . satisfy

while1 :: (c -> Bool) -> Parser c [c]
while1 = some . satisfy

sepBy :: Parser c b -> Parser c a -> Parser c [a]
sepBy s e = sepBy1 s e <|> return []

sepBy1 :: Parser c b -> Parser c a -> Parser c [a]
sepBy1 s e = (:) <$> e <*> many (s *> e)

chainl :: Parser c a -> Parser c (a -> a -> a) -> Parser c a -> Parser c a
chainl p op = (chainl1 p op <|>)

chainl1 :: Parser c a -> Parser c (a -> a -> a) -> Parser c a
chainl1 p op = p >>= rest
 where
  rest x =
    do
      o <- op
      y <- p
      rest (x `o` y)
    <|> return x

chainr :: Parser c a -> Parser c (a -> a -> a) -> Parser c a -> Parser c a
chainr p op = (chainr1 p op <|>)

chainr1 :: Parser c a -> Parser c (a -> a -> a) -> Parser c a
chainr1 p op = scan
 where
  scan = do
    x <- p
    rest x
  rest x =
    (do
        f <- op
        f x <$> scan
      )
      <|> return x

eoi :: Parser c ()
eoi = parser $ \case
  [] -> return ((), [])
  _  -> empty

anyChar :: Parser c c
anyChar = parser $ \case
  []       -> empty
  (x : xs) -> return (x, xs)
