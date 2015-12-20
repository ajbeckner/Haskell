prime :: Int -> Int
prime 1 = 1
prime 2 = 2
prime n = sum (map (n `mod`) (map prime (map (n-) [1..n-1])))
