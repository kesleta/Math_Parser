{-# LANGUAGE LambdaCase #-}
module Parser where
import           Control.Applicative
import           Control.Monad.State

type Parser c a = StateT [c] Maybe a

runParser :: StateT s m a -> s -> m (a, s)
runParser = runStateT

parser :: (s -> m (a, s)) -> StateT s m a
parser = StateT

char :: Eq c => c -> Parser c c
char c = parser $ \case
  [] -> Nothing
  (x : xs) | c == x    -> return (x, xs)
           | otherwise -> Nothing

string :: Eq c => [c] -> Parser c [c]
string = mapM char

satisfy :: (c -> Bool) -> Parser c c
satisfy f = parser $ \case
  [] -> Nothing
  (x : xs) | f x       -> return (x, xs)
           | otherwise -> Nothing

while :: (c -> Bool) -> Parser c [c]
while = many . satisfy

while1 :: (c -> Bool) -> Parser c [c]
while1 = some . satisfy

sepBy :: Parser c a -> Parser c b -> Parser c [a]
sepBy e s = (:) <$> e <*> many (s *> e)

-- chainl :: Parser c (b -> a -> b) -> Parser c b -> Parser c a -> Parser c a
-- chainl op p d = ((flip foldl) d) <$> op <*> sepBy p op

chainl1 :: Parser c (a -> a -> a) -> Parser c a -> Parser c a
chainl1 op p = foldl1 <$> op <*> sepBy p op
