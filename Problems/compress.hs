-- compress
-- input: compress "aaabbaacc"
-- output: "abac"
compress :: Eq a => [a] -> [a]
compress ns = helper ns []
    where    helper :: Eq a => [a] -> [a] -> [a]
	     helper [] ans = ans
	     helper (n:ns) cur
		 | length cur == 0 = helper ns (cur++[n])
                 | n /= (cur !! (length cur - 1)) = helper ns (cur++[n])
		 | otherwise = helper ns cur
