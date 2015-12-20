fib ::  Int -> Int
fib 1 = 1
fib n = if n < 1 then 0 else (fib (n-1)) + (fib (n-2))