euler4 = maximum $ foldl (++) [] $ filter (\a -> a /= []) $[map (\s -> read s :: Int) $ filter (\s -> s == reverse s) $ map show $ (\a -> map (*(last a)) a) $ take n [999,998..1] | n <- [1..999]] 

