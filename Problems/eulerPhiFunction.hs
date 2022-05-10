phiE :: Int -> Int
phiE n = length [i | i <- [1..n], gcd i n == 1]
