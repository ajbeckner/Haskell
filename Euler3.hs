notDivisibleBy :: Int -> Int -> Bool
notDivisibleBy x y = (x `mod` y) /= 0 

prime :: Int -> Bool 
prime 1 = True
prime 2 = True
prime x = foldl1 (&&) $ map (x `notDivisibleBy`) [2..x-1]

bigNum :: Int
bigNum = 600851475143

maxFactor :: Int
maxFactor = floor(sqrt(fromIntegral bigNum)) 

main :: IO ()
main = do
    print $ maximum $ filter prime [x | x <- [1..maxFactor], bigNum `mod` x == 0]
