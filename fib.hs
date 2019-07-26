-- File: fib.hs
import System.Environment

fib :: Int -> Int
fib n
  | n<2 = 1
  | n<3 = n
  | otherwise= fib(n-3) + fib(n-2) + fib(n-2)

main = do
  [n] <- getArgs
  print (fib (read n))
