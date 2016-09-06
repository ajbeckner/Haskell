fib :: (Num a, Ord a) => a -> a
fib n 
  | n <= 1 = 1
  | otherwise = fib (n-1) + fib (n-2)

fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
