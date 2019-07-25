import System.Environment

fib :: Int -> Int
fib n | n < 2 = 1
fib n | n < 3 = n
fib n = fib(n-3) + fib(n-2) + fib(n-2)

-- Comentário 1
-- Comentário 2
-- Comentário 3
-- Comentário 4
-- Comentário 5
-- Comentário 6
-- Comentário 7

-- Comentário 8

main :: IO ()
main = do
  [xs] <- getArgs
  print (fib (read xs))


