fizzbuzz :: [Int] -> [String]
fizzbuzz [] = []
fizzbuzz (x:xs)
	| fizz x && buzz x = "FizzBuzz":(fizzbuzz xs)
	| fizz x = "Fizz":(fizzbuzz xs)
	| buzz x = "Buzz":(fizzbuzz xs)
	| otherwise = (show x):(fizzbuzz xs)
		where fizz x = mod x 3 == 0
		      buzz x = mod x 5 == 0
