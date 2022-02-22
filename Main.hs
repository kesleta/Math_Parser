module Main where
import ExprParser
import Evaluator
import Control.Monad

main :: IO ()
main = do
  s <- getLine
  putStrLn $ maybe "error" show ((parseExpr >=> eval) s)
  main