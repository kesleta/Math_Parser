module Main where
import           Algebra
import           Control.Monad
import           Evaluator
import           ExprParser

main :: IO ()
main = do
  s <- getLine
  putStrLn $ maybe "error" show ((parseExpr >=> eval) s)
  main
