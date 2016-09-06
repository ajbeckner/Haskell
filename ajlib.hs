head' :: [a] -> a
head' (x:xs) = x

tail' :: [a] -> [a]
tail' (x:xs) = xs

init' :: [a] -> [a]
init' [x] = []
init' (x:xs) = x:init'(xs)

last' :: [a] -> a
last' [x] = x
last' (x:xs) = last' xs

sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + (sum' xs)

product' :: (Num a) => [a] -> a
product' [] = 1
product' (x:xs) = x * (product' xs)

length' :: [a] -> Integer
length' n = sum' [1 | x <- n] 

null' :: [a] -> Bool
null' [] = True
null' _ = False

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

take' :: Integer -> [a] -> [a]
take' n (x)
	| (length' $ init' x) == n = init' x
	| otherwise = take' n $ init' x

drop' :: Integer -> [a] -> [a]
drop' n x = reverse' (take' ((length' x) - n) (reverse'  x))

min' :: (Ord a) => a -> a -> a
min' n m = if n < m then n else m

max' :: (Ord a) => a -> a -> a
max' x y = if x > y then x else y

minimum' :: (Ord a) => [a] -> a
minimum' [x] = x
minimum' (x:xs) = min' x (minimum' xs)

maximum' :: (Ord a) => [a] -> a
maximum' [x] = x
maximum' (x:xs) = max' x (maximum' xs)

elem' :: (Eq a) => a -> [a] -> Bool
elem' _ [] = False
elem' n (x:xs)
	| n == x = True
	| otherwise = elem n xs

cycle' :: [a] -> [a]
cycle' x = x ++ (cycle' x)

repeat' :: a -> [a]
repeat' x = x:(repeat' x)

fst' :: (a,b) -> a
fst' (a,b) = a

snd' :: (a,b) -> b
snd' (a,b) = b

zip' :: [a] -> [b] -> [(a,b)]
zip' [] [] = []
zip' (x:[]) (y:_) = (x,y):[]
zip' (x:_) (y:[]) = (x,y):[] 
zip' (x:xs) (y:ys) = (x,y):(zip' xs ys)

zipwith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipwith' _ [] _ = []
zipwith' _ _ [] = []
zipwith' f (x:xs) (y:ys) = (f x y):(zipwith' f xs ys)

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = [] 
quicksort (x:xs) = 
	(quicksort [n | n <- xs, n <= x]) ++ 
	[x] ++ 
	(quicksort [n | n <- xs, n > x]) 

flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f = g
	where g x y = f y x

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = (f x):(map' f xs)

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' f (x:xs)
	| f x = x:(filter' f xs)
	| otherwise = filter' f xs 

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' f (x:xs)
	| f x = x:(takeWhile' f xs)
	| otherwise = []

foldl' :: (a -> a -> a) -> a -> [a] -> a
foldl' _ ac [] = ac
foldl' f ac (x:xs) = f (foldl' f ac xs) x

foldr' :: (a -> a -> a) -> a -> [a] -> a
foldr' f ac xs = foldl' (flip' f) ac $ (reverse' $ xs ++ [ac])

scanl' :: (a -> a -> a) -> a -> [a] -> [a]
scanl' _ _ [] = []
scanl' f ac (x:xs) = (f ac x):(scanl' f (f ac x) xs)

