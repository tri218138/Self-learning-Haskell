-- quick sort
qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x : xs) =
    qsort smaller ++ [x] ++ qsort larger
        where
            smaller = [a | a <- xs, a <= x]
            larger = [b | b <- xs, b > x]

-- merge sort
merge :: Ord a => [a] -> [a] -> [a]
merge ns [] = ns
merge [] ns = ns
merge (a:as) (b:bs) =
    if a < b then [a] ++ (merge as ([b] ++ bs))
    else [b] ++ (merge ([a] ++ as) bs)

msort :: Ord a => [a] -> [a]
msort ns
    | length ns <= 1 = ns
    | otherwise = merge (msort left) (msort right)
            where 
                left = take mid ns
                right = drop mid ns
                mid = floor(fromIntegral(length ns) / 2)
